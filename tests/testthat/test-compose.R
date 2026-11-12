source(file.path(raiz, "R", "adapter_compose.R"), encoding = "UTF-8")

plantilla <- tablas_escenario_plantilla()
libs <- biblioteca_ejemplo(plantilla)
esc <- list(name = "Reforma de prueba",
            sub = "Descuento porcentual focalizado por bloque",
            comp = "Bono social: 150 % de la mediana")

test_that("los campos de la app existen en la plantilla", {
  expect_true(all(SUB_CAMPOS %in% names(plantilla$sim_sub)))
  expect_true(all(COMP_CAMPOS %in% names(plantilla$sim_extra)))
})

test_that("la composición conserva columnas originales y agrega la base", {
  t <- compose_scenario_inputs(list(esc), libs, plantilla)
  expect_equal(names(t$sim_sub), names(limpiar_columnas(plantilla$sim_sub)))
  expect_equal(names(t$sim_extra), names(limpiar_columnas(plantilla$sim_extra)))
  expect_equal(names(t$escenarios), names(limpiar_columnas(plantilla$escenarios)))
  expect_equal(t$escenarios$escenario, 0:1)
  expect_equal(t$escenarios$sim_sub, c(0, 1))
  expect_equal(t$escenarios$sim_com, c(0, 1))
  expect_equal(t$sim_sub$sim_sub, c(0, 1))
  expect_equal(t$sim_extra$sim_com, c(0, 1))
  expect_equal(t$escenarios$des_corto[2], "Reforma de prueba")
})

test_that("el esquema compuesto reproduce los valores de la plantilla", {
  t <- compose_scenario_inputs(list(esc), libs, plantilla)
  orig <- plantilla$sim_sub[plantilla$sim_sub$sim_sub == 2, SUB_CAMPOS]
  # Las columnas totalmente vacías se leen como lógicas en la plantilla
  num <- \(d) as.data.frame(lapply(d, as.numeric))
  expect_equal(num(t$sim_sub[2, SUB_CAMPOS]), num(orig), tolerance = 1e-12)
  orig_c <- plantilla$sim_extra[plantilla$sim_extra$sim_com == 2, COMP_CAMPOS]
  orig_n <- normalizar_comp(as.list(orig_c))
  expect_equal(as.list(t$sim_extra[2, COMP_CAMPOS]), orig_n, tolerance = 1e-12)
})

test_that("el centinela y '' significan situación actual", {
  e <- list(list(name = "A", sub = REF_SENTINELA, comp = ""),
            list(name = "B", sub = "Ampliar tarifa social hasta 100 kWh", comp = REF_SENTINELA))
  t <- compose_scenario_inputs(e, libs, plantilla)
  expect_equal(t$escenarios$sim_sub, c(0, 0, 1))
  expect_equal(t$escenarios$sim_com, c(0, 0, 0))
  expect_equal(nrow(t$sim_extra), 1)
})

test_that("componentes repetidos comparten id", {
  e <- list(list(name = "A", sub = names(libs$sub)[1], comp = names(libs$comp)[1]),
            list(name = "B", sub = names(libs$sub)[1], comp = names(libs$comp)[2]))
  t <- compose_scenario_inputs(e, libs, plantilla)
  expect_equal(t$escenarios$sim_sub, c(0, 1, 1))
  expect_equal(t$escenarios$sim_com, c(0, 1, 2))
})

test_that("la validación detecta nombres repetidos, vacíos y componentes faltantes", {
  expect_error(compose_scenario_inputs(list(esc, esc), libs, plantilla), "únicos")
  expect_error(compose_scenario_inputs(list(modifyList(esc, list(name = ""))), libs, plantilla), "nombre")
  expect_error(compose_scenario_inputs(list(modifyList(esc, list(sub = "no existe"))), libs, plantilla), "No existe")
})

test_that("la composición de la plantilla iguala el escenario 3 del colega al correr el motor", {
  skip_on_cran()
  e <- list(name = "Reforma + transferencia", sub = names(libs$sub)[2], comp = names(libs$comp)[1])
  # Esquema 2 + compensación 1 = escenario 3 de la plantilla
  r <- run_scenario(compose_scenario_inputs(list(e), libs, plantilla))$resumen$escenario_1
  fx <- fixture("GTM_resumen.rds")$escenario_3
  expect_equal(unname(r$fis), unname(fx$fis), tolerance = 1e-8)
  expect_equal(unname(r$pov_gral), unname(fx$pov_gral), tolerance = 1e-8)
  expect_equal(unname(r$ineq), unname(fx$ineq), tolerance = 1e-8)
})

test_that("normalizar_comp deja solo los campos relevantes", {
  p <- normalizar_comp(list(grupo_com = 2, metodo_com = 2, valor_com = 500, pct_com = 150,
                            decil_com = 1, elect_com = 100))
  expect_equal(unname(unlist(p)), c(2, 2, NA, 150, NA, NA))
  p1 <- normalizar_comp(list(grupo_com = 1, metodo_com = 1, valor_com = 500, pct_com = 5,
                             decil_com = 2, elect_com = 100))
  expect_equal(p1$decil_com, 2); expect_true(is.na(p1$elect_com))
})

test_that("la biblioteca inicial normalizada corre en el motor (compensación bono social)", {
  e <- list(name = "Bono", sub = REF_SENTINELA, comp = "Bono social: 150 % de la mediana")
  r <- run_scenario(compose_scenario_inputs(list(e), libs, plantilla))$resumen$escenario_1
  expect_gt(r$fis[["Costo compensación (millones Q/año)"]], 0)
})

test_that("ida y vuelta de la biblioteca por Excel", {
  source(file.path(raiz, "R", "scenario_store.R"), encoding = "UTF-8")
  esc <- list(list(name = "X", sub = names(libs$sub)[1], comp = REF_SENTINELA, comparar = TRUE))
  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f))
  guardar_xlsx(f, libs, esc)
  l <- leer_xlsx(f)
  expect_equal(names(l$libs$sub), names(libs$sub))
  expect_equal(l$libs$sub[[1]], libs$sub[[1]], tolerance = 1e-12)
  expect_equal(l$libs$comp, libs$comp, tolerance = 1e-12)
  expect_equal(l$escenarios[[1]]$name, "X")
  expect_true(l$escenarios[[1]]$comparar)
  expect_equal(normalizar_ref(l$escenarios[[1]]$comp), "")
})

test_that("una sesión nueva arranca sin componentes de ejemplo", {
  expect_equal(biblioteca_inicial(), list(sub = list(), comp = list()))
})

test_that("validar_esquema acepta la tarifa vigente y los ejemplos, y detecta faltantes", {
  base <- payload_sub_desde_fila(plantilla$sim_sub[plantilla$sim_sub$sim_sub == 0, ])
  expect_length(validar_esquema(base), 0)
  for (p in libs$sub) expect_length(validar_esquema(p), 0)
  sin_tope <- base; sin_tope$ttdeoc <- NA_real_
  expect_match(validar_esquema(sin_tope), "tarifa tope de DEOCSA")
  sin_plena <- base; sin_plena$block6 <- 500
  expect_match(validar_esquema(sin_plena), "tramo 6", all = FALSE)
  hueco <- base; hueco$block3 <- NA_real_
  expect_match(validar_esquema(hueco), "sin dejar huecos", all = FALSE)
  l2 <- list(sub = list(malo = sin_tope), comp = list())
  expect_match(validar_escenarios(list(list(name = "E", sub = "malo", comp = "")), l2), "«malo» está incompleto")
})
