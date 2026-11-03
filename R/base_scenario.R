# Escenario 0 (situación base) reconstruible: lectura de data/base_scenario.rds y
# comprobación de que sigue vigente respecto de reference/ y de los datos.
#
# El script `04` del colega siempre recorre el escenario 0 dentro de su bucle y
# exige al menos un escenario de reforma, así que cada corrida de usuario lo
# vuelve a calcular internamente. Aquí solo se garantiza que la columna "Base"
# de los resultados salga SIEMPRE del archivo construido por
# data-raw/build_base_scenario.R (una sola fuente), y se avisa si ese archivo
# quedó desactualizado.
#
# GANCHO PARA sync_reference() (aún no existe): al terminar una sincronización
# debe llamar a `base_vigente()`; si `$vigente` es FALSE, pedir que se corra
# `data-raw/build_base_scenario.R` desde la raíz y volver a ejecutar las pruebas.

BASE_ARCHIVO <- "base_scenario.rds"

#' Rutas (relativas a la raíz) de los insumos que determinan la base: todo
#' `reference/` (salvo su registro de copia) y los datos que lee el motor.
insumos_base_rutas <- function() {
  ref <- list.files(ruta_app("reference"), recursive = TRUE, all.files = FALSE)
  ref <- ref[basename(ref) != ".manifest.rds"]
  c(file.path("reference", sort(ref)),
    file.path("data", c(names(REF_DATOS_PRESIM), "params_base.rds")))
}

#' md5 de los insumos de la base, con nombres = ruta relativa; `opc` resume los
#' interruptores permanentes de `R/config.R` que afectan el cálculo.
insumos_base <- function() {
  rutas <- insumos_base_rutas()
  md5 <- unname(tools::md5sum(vapply(rutas, ruta_app, "")))
  names(md5) <- rutas
  opc <- OPC[setdiff(names(OPC), c("shp_deptos", "shp_codcol", "run_diag"))]
  c(md5, "OPC (R/config.R)" = rlang::hash(opc))
}

leer_base_scenario <- function() {
  f <- ruta_app("data", BASE_ARCHIVO)
  if (file.exists(f)) readRDS(f) else NULL
}

#' Compara los insumos actuales con los registrados al construir la base.
#'
#' @return lista con `vigente` (lógico), `motivos` (texto en español) y
#'   `cambiados` (rutas distintas, nuevas o faltantes).
base_vigente <- function(base = leer_base_scenario(), insumos = insumos_base()) {
  if (is.null(base) || is.null(base$insumos)) {
    return(list(vigente = FALSE, cambiados = character(),
                motivos = "No existe la base precalculada (data/base_scenario.rds)."))
  }
  reg <- base$insumos
  cambiados <- union(names(insumos)[!names(insumos) %in% names(reg) |
                                      is.na(insumos[names(insumos)]) |
                                      insumos != reg[names(insumos)]],
                     setdiff(names(reg), names(insumos)))
  cambiados <- cambiados[!is.na(cambiados)]
  list(vigente = !length(cambiados), cambiados = cambiados,
       motivos = if (length(cambiados)) {
         paste0("La base precalculada está desactualizada; cambió: ",
                paste(cambiados, collapse = ", "),
                ". Reconstruirla con data-raw/build_base_scenario.R.")
       } else character())
}
