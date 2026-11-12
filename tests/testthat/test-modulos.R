# Módulos Shiny con testServer: la simulación llena los resultados y las salidas
# de resultados (gráficos plotly, mapa, cuadros) se generan sin errores.

library(shiny)
library(bslib)
for (f in c("results_views.R", "map_views.R", "ui_helpers.R", "adapter_compose.R", "scenario_store.R")) {
  source(file.path(raiz, "R", f), encoding = "UTF-8")
}
for (f in c("mod_resultados.R", "mod_simulacion.R")) source(file.path(raiz, f), encoding = "UTF-8")

plantilla <- tablas_escenario_plantilla()
libs <- biblioteca_ejemplo(plantilla)

test_that("Simular llena los resultados con etiquetas y base vigente", {
  estado <- reactiveValues(
    libs = libs, resultados = NULL, firma = NULL,
    escenarios = list(list(name = "Reforma", sub = names(libs$sub)[2], comp = names(libs$comp)[1],
                           comparar = TRUE)))
  testServer(mod_simulacion_server, args = list(estado = estado, plantilla = plantilla), {
    session$setInputs(run = 1)
    expect_false(is.null(estado$resultados))
    expect_equal(estado$resultados$equivalencias$etiqueta, c("Base", "Escenario 1"))
    expect_true(estado$resultados$base_desde_archivo)
  })
})

test_that("las salidas de resultados se generan, incluido el mapa", {
  res <- ejecutar_escenarios(
    list(list(name = "Reforma", sub = names(libs$sub)[2], comp = names(libs$comp)[1])),
    libs, plantilla)
  testServer(mod_resultados_server, args = list(res = reactive(res)), {
    session$setInputs(tipo_pob = "general", vista = "mil", medida = "neto", esc_mapa = "Reforma",
                      esc_tar = "Reforma")
    for (o in c("g_pob_tasa", "g_pob_fgt1", "g_gini", "g_palma", "g_fiscal", "g_fiscal_pib",
                "g_reg", "g_dec", "mapa_reg")) {
      expect_match(output[[o]], "plotly|x", info = o)
    }
    expect_false(is.null(output$tbl_equiv))
  })
})

test_that("ya no existe la descarga de cuadros en Excel", {
  expect_false(any(grepl("descargar|downloadHandler", readLines(file.path(raiz, "mod_resultados.R"),
                                                                 encoding = "UTF-8"))))
})

test_that("la biblioteca carga valores sin poner nombre y pide confirmar antes de reemplazar", {
  source(file.path(raiz, "mod_esquema.R"), encoding = "UTF-8")
  estado <- reactiveValues(libs = libs, escenarios = list())
  nm0 <- names(libs$sub)[1]
  testServer(mod_esquema_server, args = list(estado = estado, plantilla = plantilla), {
    # testServer no aplica los update*Input: se fijan a mano los valores del esquema
    p0 <- libs$sub[[nm0]]
    campos <- setdiff(SUB_CAMPOS, c(SUB_CAMPOS_LIMITE, paste0("tplena", names(SUB_DISTRIBUIDORAS))))
    do.call(session$setInputs, p0[campos])
    lim <- setNames(p0[SUB_CAMPOS_LIMITE], esq_id_limite(seq_along(SUB_CAMPOS_LIMITE), "eeg"))
    do.call(session$setInputs, lim)
    session$setInputs(cargar = nm0)
    expect_equal(input$nombre %||% "", "")
    session$setInputs(nombre = nm0, guardar = 1)
    expect_false(is.null(pendiente()))
    session$setInputs(confirmar_reemplazo = 1)
    expect_null(pendiente())
    session$setInputs(nombre = "Nuevo esquema", guardar = 2)
    expect_true("Nuevo esquema" %in% names(estado$libs$sub))
  })
})

test_that("la lista de escenarios marca, limita y elimina por fila", {
  source(file.path(raiz, "mod_composer.R"), encoding = "UTF-8")
  esc <- lapply(1:5, \(i) list(name = paste("E", i), sub = names(libs$sub)[1], comp = "", comparar = i <= 4))
  estado <- reactiveValues(libs = libs, escenarios = esc)
  testServer(mod_composer_server, args = list(estado = estado), {
    session$setInputs(marcar = list(nombre = "E 5", valor = TRUE))
    expect_false(estado$escenarios[[5]]$comparar)
    session$setInputs(marcar = list(nombre = "E 1", valor = FALSE))
    expect_false(estado$escenarios[[1]]$comparar)
    session$setInputs(marcar = list(nombre = "E 5", valor = TRUE))
    expect_true(estado$escenarios[[5]]$comparar)
    session$setInputs(borrar = "E 2")
    expect_equal(vapply(estado$escenarios, `[[`, "", "name"), c("E 1", "E 3", "E 4", "E 5"))
    expect_match(as.character(output$lista$html), "fa-trash")
  })
})

test_that("un error del motor indica el escenario que falló", {
  env <- environment(ejecutar_escenarios)
  original <- get("run_scenario", envir = env)
  assign("run_scenario", \(...) stop("fallo interno"), envir = env)
  on.exit(assign("run_scenario", original, envir = env), add = TRUE)
  expect_error(ejecutar_escenarios(list(list(name = "Roto", sub = names(libs$sub)[1], comp = "")), libs, plantilla),
               "Escenario «Roto»: fallo interno")
})

test_that("la app no muestra la tarifa plena general ni la pestaña de estratos", {
  source(file.path(raiz, "mod_esquema.R"), encoding = "UTF-8")
  html <- as.character(mod_esquema_ui("esq"))
  expect_false(grepl("tplena|Tarifa plena general", html))
  expect_false(any(grepl("\"Estratos\"", readLines(file.path(raiz, "mod_resultados.R"), encoding = "UTF-8"))))
})

test_that("«Actualizar» guarda los cambios en el esquema cargado sin escribir el nombre", {
  source(file.path(raiz, "mod_esquema.R"), encoding = "UTF-8")
  estado <- reactiveValues(libs = libs, escenarios = list())
  nm0 <- names(libs$sub)[1]
  testServer(mod_esquema_server, args = list(estado = estado, plantilla = plantilla), {
    p0 <- libs$sub[[nm0]]
    campos <- setdiff(SUB_CAMPOS, c(SUB_CAMPOS_LIMITE, paste0("tplena", names(SUB_DISTRIBUIDORAS))))
    do.call(session$setInputs, p0[campos])
    do.call(session$setInputs, setNames(p0[SUB_CAMPOS_LIMITE], esq_id_limite(seq_along(SUB_CAMPOS_LIMITE), "eeg")))
    session$setInputs(cargar = nm0, nombre = "")
    expect_match(as.character(output$insignia$html), "guardado")
    session$setInputs(toeeg1 = 0.123)
    expect_match(as.character(output$insignia$html), "cambios sin guardar")
    session$setInputs(actualizar = 1)
    expect_equal(estado$libs$sub[[nm0]]$toeeg1, 0.123)
    expect_equal(estado$libs$sub[[nm0]]$tplenaeeg, p0$tplenaeeg)
    expect_length(estado$libs$sub, length(libs$sub))
    # Con la tarifa vigente cargada no hay nada que actualizar
    session$setInputs(cargar = REF_SENTINELA, actualizar = 2)
    expect_equal(names(estado$libs$sub), names(libs$sub))
  })
})

test_that("«Actualizar» guarda los cambios en la compensación cargada", {
  source(file.path(raiz, "mod_compensacion.R"), encoding = "UTF-8")
  estado <- reactiveValues(libs = libs, escenarios = list())
  nm <- names(libs$comp)[1]   # grupo 1: monto fijo hasta un decil
  testServer(mod_compensacion_server, args = list(estado = estado, plantilla = plantilla), {
    session$setInputs(cargar = nm, grupo_com = "1", decil_com = "3", valor_com_1 = 250)
    session$setInputs(actualizar = 1)
    expect_equal(estado$libs$comp[[nm]]$decil_com, 3)
    expect_equal(estado$libs$comp[[nm]]$valor_com, 250)
  })
})
