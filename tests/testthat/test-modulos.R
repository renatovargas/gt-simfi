# Módulos Shiny con testServer: la simulación llena los resultados y las salidas
# de resultados (gráficos plotly, mapa, cuadros) se generan sin errores.

library(shiny)
library(bslib)
for (f in c("results_views.R", "map_views.R", "ui_helpers.R", "adapter_compose.R", "scenario_store.R")) {
  source(file.path(raiz, "R", f), encoding = "UTF-8")
}
for (f in c("mod_resultados.R", "mod_simulacion.R")) source(file.path(raiz, f), encoding = "UTF-8")

plantilla <- tablas_escenario_plantilla()
libs <- biblioteca_inicial(plantilla)

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
                "g_estratos", "g_reg", "g_dec", "mapa_reg")) {
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
    # testServer no aplica los update*Input: se fijan los límites de tramo a mano
    lim <- setNames(as.list(c(0, 50, 100, 150, 200, 300, 400)[seq_along(SUB_CAMPOS_LIMITE)]),
                    SUB_CAMPOS_LIMITE)
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
