# Escenario 0 precalculado: paridad con los fixtures, equivalencia con y sin
# caché y detección de base obsoleta.

source(file.path(raiz, "R", "adapter_compose.R"), encoding = "UTF-8")
plantilla <- tablas_escenario_plantilla()
libs <- biblioteca_ejemplo(plantilla)

test_that("la base precalculada iguala las columnas Base del fixture del colega", {
  b <- leer_base_scenario()
  expect_false(is.null(b))
  fx <- fixture("GTM_resumen.rds")
  r <- b$resumen$escenario_1
  f <- fx$escenario_1
  for (k in c("pov_gral", "pov_ext", "pov_int", "ineq")) {
    expect_equal(unname(r[[k]][, 1]), unname(f[[k]][, 1]), tolerance = 1e-8, info = k)
  }
  claves_base <- match(c("costo_base", "hog_base", "sub_prom_base", "pib_base"), CLAVES_FIS)
  expect_equal(unname(r$fis[claves_base]), unname(f$fis[claves_base]), tolerance = 1e-8)
  expect_equal(attr(b$resumen, "resumen")[, 1], attr(fx, "resumen")[, 1], tolerance = 1e-8)
  expect_equal(r$tar_base, f$tar_base, tolerance = 1e-8)
})

test_that("los resultados de usuario no cambian con y sin la base precalculada", {
  e <- list(list(name = "Reforma + transferencia", sub = names(libs$sub)[2],
                 comp = names(libs$comp)[1]),
            list(name = "Solo tarifa", sub = names(libs$sub)[1], comp = REF_SENTINELA))
  con <- ejecutar_escenarios(e, libs, plantilla)
  sin <- ejecutar_escenarios(e, libs, plantilla, base = FALSE)
  expect_true(con$base_desde_archivo)
  expect_false(sin$base_desde_archivo)
  campos <- setdiff(names(con), c("base_desde_archivo", "base_estado"))
  expect_equal(con[campos], sin[campos], tolerance = 1e-8)
  expect_equal(con$equivalencias$etiqueta, c("Base", "Escenario 1", "Escenario 2"))
  expect_equal(con$equivalencias$esquema[3], names(libs$sub)[1])
  expect_equal(con$equivalencias$compensacion[3], "Sin compensación")
})

test_that("base_vigente detecta insumos cambiados, nuevos y base inexistente", {
  b <- leer_base_scenario()
  ins <- insumos_base()
  expect_true(base_vigente(b, ins)$vigente)

  cambiado <- ins
  cambiado[["data/disp_income.rds"]] <- "0000"
  v <- base_vigente(b, cambiado)
  expect_false(v$vigente)
  expect_equal(v$cambiados, "data/disp_income.rds")
  expect_match(v$motivos, "desactualizada")

  nuevo <- c(ins, "reference/scripts_R/99_nuevo.R" = "abc")
  expect_false(base_vigente(b, nuevo)$vigente)
  expect_false(base_vigente(b, ins[-1])$vigente)
  expect_false(base_vigente(NULL, ins)$vigente)
})

test_that("una base obsoleta avisa y los resultados siguen siendo los de la corrida", {
  e <- list(list(name = "Solo tarifa", sub = names(libs$sub)[1], comp = REF_SENTINELA))
  b <- leer_base_scenario()
  b$insumos[[1]] <- "0000"
  expect_warning(r <- ejecutar_escenarios(e, libs, plantilla, base = b), "desactualizada")
  expect_false(r$base_desde_archivo)
  sin <- ejecutar_escenarios(e, libs, plantilla, base = FALSE)
  campos <- setdiff(names(r), c("base_desde_archivo", "base_estado"))
  expect_equal(r[campos], sin[campos], tolerance = 1e-8)
})
