# Parámetros: tablas de escenario <-> forma de las hojas del libro del colega.

# Plantilla de referencia (verbatim del colega).
RUTA_PLANTILLA <- "reference/params/GTM23SUB_parametros.xlsx"

#' Lee las tablas de escenario (escenarios, sim_sub, sim_extra) de la plantilla,
#' con los nombres de columna originales. Son el modelo de escenarios de la app.
tablas_escenario_plantilla <- function(ruta = ruta_app(RUTA_PLANTILLA)) {
  hojas <- c("escenarios", "sim_sub", "sim_extra")
  setNames(lapply(hojas, \(h) {
    readxl::read_excel(ruta, sheet = h, .name_repair = "minimal")
  }), hojas)
}

#' Lee data/params_base.rds (hoja global_params limpia: descripcion, valor, observaciones).
leer_params_base <- function(ruta = ruta_app("data", "params_base.rds")) {
  readRDS(ruta)
}

#' `parm_glob` tal como lo arma el Master: global_params con nombres limpios.
construir_parm_glob <- function(params_base) {
  janitor::clean_names(params_base)
}

#' Escribe un libro con las mismas hojas y columnas que la plantilla del colega.
#' Las columnas sin nombre (hay una vacía en sim_sub) se omiten.
write_params_xlsx <- function(tablas, params_base, ruta) {
  hojas <- c(list(global_params = as.data.frame(params_base)),
             lapply(tablas[c("escenarios", "sim_sub", "sim_extra")], as.data.frame))
  hojas <- lapply(hojas, \(d) d[, !is.na(names(d)) & names(d) != "", drop = FALSE])

  wb <- openxlsx::createWorkbook()
  for (h in names(hojas)) {
    openxlsx::addWorksheet(wb, h)
    openxlsx::writeData(wb, h, hojas[[h]])
  }
  openxlsx::saveWorkbook(wb, ruta, overwrite = TRUE)
  invisible(ruta)
}

#' Verifica que cada patrón de `04` resuelve a exactamente un valor no NA.
verificar_parm_glob <- function(parm_glob, patrones = REF_PARM_PATRONES) {
  vapply(patrones, \(p) {
    v <- parm_glob$valor[stringr::str_detect(parm_glob$descripcion,
                                             stringr::regex(p, ignore_case = TRUE))]
    length(v) == 1 && !is.na(v)
  }, logical(1))
}
