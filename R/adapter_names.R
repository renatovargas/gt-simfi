# Todo el acoplamiento con nombres del colega (archivos, hojas, columnas,
# etiquetas) vive en el adaptador. Nada fuera de adapter_*.R conoce estos nombres.

# Paquetes que el Master del colega adjunta (líneas 32-43); sus scripts 01-05
# no llaman a library().
REF_PAQUETES <- c(
  "readxl", "tidyverse", "janitor", "openxlsx", "data.table", "statar",
  "scales", "collapse", "rlang", "ragg", "sf", "haven"
)

# Scripts de referencia (carpeta reference/scripts_R/).
REF_SCRIPTS <- c(
  funciones     = "x_GTM26SUB_funciones.R",
  subsidio      = "02_GTM26SUB_Subsidio.R",
  transferencia = "03_GTM26SUB_Transferencia.R",
  consolida     = "04_GTM26SUB_Consolida.R"
)

# Datos base (data/) -> nombre que esperan los scripts dentro de `fpresim`.
REF_DATOS_PRESIM <- c(
  "disp_income.rds"      = "disp_income.rds",
  "energia_ajustado.rds" = "energia_ajustado.rds"
)

# Archivos que escriben 02-04 en `fdbmod` y su nombre en la lista de resultados.
REF_SALIDAS <- c(
  subeleh       = "GTM_subeleh.rds",
  tarifas       = "GTM_tarifas.rds",
  simcomp       = "GTM_simcomp.rds",
  simulaciones  = "GTM_simulaciones_subsidio.rds",
  resumen       = "GTM_resumen.rds"
)

# Hojas del libro de parámetros que leen los scripts.
REF_HOJAS <- c("global_params", "escenarios", "sim_sub", "sim_extra")

# Patrones con los que `04` busca parámetros en `parm_glob$descripcion`
# (`param_glob(patron)`). Si cambia la etiqueta en global_params, deja de resolver.
REF_PARM_PATRONES <- c(
  pib      = "^PIB",
  sub_inde = "Subsidio Aporte"
)

# Reproduce lo que el Master hace con la hoja `escenarios` (líneas 93-107).
preparar_escenarios <- function(escenarios) {
  esc <- escenarios |>
    janitor::clean_names() |>
    dplyr::filter(activo == 1) |>
    dplyr::arrange(escenario)

  if (!"des_corto" %in% names(esc) || all(is.na(esc$des_corto))) {
    esc$des_corto <- esc$des_escenario
  }
  esc <- esc |>
    dplyr::mutate(des_corto = dplyr::coalesce(
      des_corto, des_escenario, paste("Escenario", escenario)
    ))

  stopifnot(
    "El escenario 0 (base) debe estar activo" = 0 %in% esc$escenario,
    "Hay escenarios repetidos"                 = !anyDuplicated(esc$escenario)
  )
  esc
}
