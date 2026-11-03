tol <- 1e-8
tablas <- tablas_escenario_plantilla()
res <- run_scenario(tablas)

test_that("la plantilla define los escenarios 0 a 3", {
  expect_equal(preparar_escenarios(tablas$escenarios)$escenario, 0:3)
})

test_that("cada patrón de parm_glob resuelve a un valor", {
  ok <- verificar_parm_glob(construir_parm_glob(leer_params_base()))
  expect_true(all(ok), info = paste(names(ok)[!ok], collapse = ", "))
})

test_that("ida y vuelta de parámetros: tablas -> xlsx -> lectura", {
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f))
  write_params_xlsx(tablas, leer_params_base(), f)
  for (h in c("escenarios", "sim_sub", "sim_extra")) {
    esperado <- tablas[[h]][, names(tablas[[h]]) != "", drop = FALSE]
    leido <- readxl::read_excel(f, sheet = h, .name_repair = "minimal")
    # Las columnas totalmente vacías pierden su tipo en el xlsx; se compara
    # solo lo que tiene datos.
    con_datos <- names(esperado)[vapply(esperado, \(x) any(!is.na(x)), logical(1))]
    expect_equal(as.data.frame(leido[, con_datos]), as.data.frame(esperado[, con_datos]),
                 tolerance = tol, info = h)
  }
})

test_that("la corrida del adaptador iguala los fixtures (escenarios 0 a 3)", {
  fx <- c(subeleh = "GTM_subeleh.rds", tarifas = "GTM_tarifas.rds",
          simcomp = "GTM_simcomp.rds", simulaciones = "GTM_simulaciones_subsidio.rds",
          resumen = "GTM_resumen.rds")
  for (k in names(fx)) {
    expect_equal(res[[k]], fixture(fx[[k]]), tolerance = tol, info = k)
  }
})

test_that("el adaptador no escribe en .GlobalEnv", {
  expect_false(exists("OPC", envir = globalenv(), inherits = FALSE) &&
                 !identical(get("OPC", globalenv()), OPC))
  expect_false(exists("escenarios", envir = globalenv(), inherits = FALSE))
})
