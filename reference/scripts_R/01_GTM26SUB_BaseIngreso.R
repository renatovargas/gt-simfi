## =============================================================================
## Banco Mundial / INDE
## Herramienta de Microsimulación de Subsidios a la Electricidad
## Country: Guatemala 2023
## 06_GTM26SUB_BaseIngreso.R   (versión R de 06_GTM26SUB_BaseIngreso.do)
## Dependencias: 00_GTM26SUB_Master.R, x_GTM26SUB_funciones.R
##
## Contenido
##   1.1-1.2  Datos ENCOVI 2023: acceso y gasto en electricidad
##   1.3-1.4  Tarifas y funciones tar_pago / tar_inv / autoprueba
##   1.5      Clasificación de hogares (clases 0-7)
##   1.6      Diagnóstico previo (OPC$run_diag)
##   1.7      Registros administrativos (usuarios por kWh y distribuidora)
##   1.8      Calibración VECTORIZADA: posterior hogar x kWh (matriz n x 300)
##            y ajuste del prior por distribuidora contra los registros
##   1.9      M imputaciones y validación
##   1.10     Limpieza, guardar energia_ajustado.rds
##   2-3      Base de ingreso disponible y estratos -> disp_income.rds
##
## Diferencias deliberadas con el .do
##   - La malla hogar x kWh se trabaja con matrices (no con expand 300). El
##     prior se guarda como matriz 3 x 300 (distribuidora x kWh): en el .do el
##     ajuste es idéntico para todos los hogares de una distribuidora, así que
##     el resultado es el mismo con mucha menos memoria.
##   - Los sorteos usan el generador de R: la distribución es la misma, los
##     valores individuales no coinciden con Stata.
##   - El .do guarda energia_ajustado en ${presim} y la parte 2 la lee de
##     ${tmp}; aquí se usa fpresim en ambos pasos.
##
## Input:  fencovi 2_encovi_2023_hogares, 3_encovi_2023_c01se_fuentes,
##         35_encovi_2023_agregado_consumo; OPC$adm_file; finput DispIncome
## Output: fpresim energia_ajustado.rds, disp_income.rds
## =============================================================================

# 0. Parámetros de la sección ---------------------------------------------------

tol_cons <- 0.15   # kWh y gasto "consistentes" si difieren <= 15 %
k_min    <- 2      # kWh reportado < k_min = "sin dato"
bw       <- 25     # ancho del rango objetivo (kWh)
nb_adm   <- 300 / bw
K_MAX    <- 300

## Verosimilitudes y raking (igual que el .do)
cal <- list(
  sig_p  = 0.25, sig_pr = 0.35,           # gasto no redondo / redondo (Q25)
  delta  = c(1, 1, 1),                    # factor de nivel por distribuidora
  w25 = 10, w10 = 5, w5 = 2,              # ventanas de redondeo del kWh
  sig_k  = 0.50, eps_v = 0.02,
  n_iter = 25
)
M <- OPC$M

mean_st <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

# 1. Base de ingresos y características ----------------------------------------
seccion("01 - 1. Ingreso disponible")

## 1.1 Archivo de ingreso disponible -------------------------------------------
dispinc <- readRDS(paste0(fpresim, "DispIncome.RDS"))

## 1.2 Variables a incluir (equivalente a los keep con comodines del .do) -------
vars_fijas <- c(
  "hhid", "depto", "dominio", "area", "upm", "factor", "pobreza", "urban",
  "miembros", "personas", "adu_hh", "chi_hh", "hsize", "sjefe", "etjefe",
  "urjefe", "edpadre", "edmadre", "yd_pc",
  "cp", "gender", "age", "relation", "edad", "grupos_edad", "pet_ine",
  "pea_ine", "ocupados", "subvisibles", "inactivos", "hhin", "indigena",
  "etnorace", "hhead", "level_school", "nivedu", "type_school", "pet", "petj",
  "po", "rama", "ciiu", "pd", "pea", "pei", "calif", "calte", "igssaf",
  "contribut", "ssjefe", "wcontract", "ciiu_2", "inla", "ii", "ila", "hacina",
  "nbi_agua", "sal_group", "p06a02",
  "salario_a", "hextras_a", "comision_a", "aguine_a1", "bon14e_a1",
  "diferido_a", "bonovac_a", "trabvaca_a", "produc_a", "salario2_a",
  "bonife2_a", "aguine_a2", "bon14e_a2", "bono14_a", "bono142_a",
  "aguinaldo_a", "aguinaldo2_a", "venag", "venna", "gan_1", "ganag1_a",
  "ganna1_a", "gagricola", "ganag2_a", "ganna2_a", "gan_2",
  "ym_pc", "yp_pc", "yn_pc", "agreg3", "indice_ipc_encovi", "otroele"
)
patrones <- c("^inf_", "^pob", "^chi", "^old", "^n_worker", "^relab",
              "^informal$", "^formal", "^employ_status$", "^my_.*_in$",
              "^dtr_.*_.*", "^edu_.*_ri$", "^ded_", "^pline_")

faltan <- setdiff(setdiff(vars_fijas, "otroele"), names(dispinc))
if (length(faltan)) message("Aviso: variables no encontradas en DispIncome: ",
                            paste(faltan, collapse = ", "))
imprescindibles <- c("hhid", "hhin", "depto", "factor", "yd_pc", "relation",
                     "hsize", "indice_ipc_encovi", "pline_mod", "pline_ext")
stopifnot("Faltan variables imprescindibles en DispIncome" =
            all(imprescindibles %in% names(dispinc)))

dispinc <- dispinc %>%
  select(any_of(vars_fijas), matches(paste(patrones, collapse = "|")))

## 2.3 Merge con electricidad ---------------------------------------------------
energia_ajustado <- readRDS(paste0(fpresim, "energia_ajustado.rds"))

dispinc <- dispinc %>%
  stata_merge(energia_ajustado, by = "hhid",
              keepusing = c("celq", "cele", "inf_elect")) %>%
  mutate(celq = if_else(eqv(relation, 1), celq, NA_real_),
         cele = if_else(eqv(relation, 1), cele, NA_real_)) %>%
  filter(!is.na(hhin))

## 2.4 Distribuidora (aprox. por depto) ----------------------------------------
## PLACEHOLDER: cooperativas Ixcán, Joyabaj y Retalhuleu no identificadas.
dispinc <- dispinc %>%
  mutate(empresa_elec  = empresa_de_depto(depto),
         region        = region_de_depto(depto),
         coop_excluida = 0L)

## 2.5 Ajuste en términos reales (varlist y*pc del .do) -------------------------
dispinc <- dispinc %>%
  mutate(across(matches("^y.*pc$"), ~ .x / indice_ipc_encovi)) %>%
  group_by(hhid) %>%
  mutate(yd_hh = sum(yd_pc, na.rm = TRUE)) %>%
  ungroup()

# 3. Estratos y deciles (línea base) --------------------------------------------
dispinc <- dispinc %>%
  mutate(gr_estrato = estrato4(yd_pc),
         decyd      = statar::xtile(yd_pc, n = 10, wt = factor))

tab_w(ESTRATOS[as.character(dispinc$gr_estrato)], dispinc$factor,
      titulo = "Estratos (línea base)")
cat(sprintf("Pobreza (yd_pc < pline_mod): %.2f%% | extrema: %.2f%%\n",
            fgt(dispinc$yd_pc, dispinc$pline_mod, dispinc$factor),
            fgt(dispinc$yd_pc, dispinc$pline_ext, dispinc$factor)))

saveRDS(dispinc, paste0(fpresim, "disp_income.rds"))

suppressWarnings(rm(
  dispinc, energia_ajustado, vars_fijas, patrones, faltan, imprescindibles,
  cal, TAR, CLASES, tol_cons, k_min, bw, nb_adm, K_MAX, M, mean_st,
  tar_params, tar_pago, tar_inv, tar_selftest, posterior))
