# Envoltorios de los scripts 02, 03 y 04. Cada uno se ejecuta con sys.source()
# en el entorno hijo; los resultados se leen de la carpeta temporal de la corrida.

ejecutar_ref <- function(env, clave, silenciar = TRUE) {
  archivo <- file.path(env$fscripts, REF_SCRIPTS[[clave]])
  correr <- \() sys.source(archivo, envir = env, keep.source = FALSE)
  if (silenciar) {
    invisible(utils::capture.output(suppressMessages(correr())))
  } else {
    correr()
  }
  invisible(env)
}

run_schemes        <- function(env, silenciar = TRUE) ejecutar_ref(env, "subsidio", silenciar)       # 02
run_compensations  <- function(env, silenciar = TRUE) ejecutar_ref(env, "transferencia", silenciar)  # 03
run_consolidation  <- function(env, silenciar = TRUE) ejecutar_ref(env, "consolida", silenciar)      # 04

#' Lee los resultados que 02-04 dejaron en la carpeta `fdbmod` de la corrida.
leer_resultados_ref <- function(env) {
  lapply(REF_SALIDAS, \(f) readRDS(file.path(env$fdbmod, f)))
}

#' Único punto de entrada al motor: ejecuta 02 -> 03 -> 04 para las tablas dadas.
#'
#' Escribe solo en un directorio temporal propio, que se borra al terminar
#' (salvo que se pase `session_dir`, que el llamador se encarga de limpiar).
run_scenario <- function(tablas, session_dir = NULL, params_base = leer_params_base(),
                         opc = OPC, silenciar = TRUE) {
  if (is.null(session_dir)) {
    session_dir <- tempfile("gtsimfi_")
    on.exit(unlink(session_dir, recursive = TRUE), add = TRUE)
  }
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)

  env <- make_ref_env(session_dir, tablas, params_base, opc)
  run_schemes(env, silenciar)
  run_compensations(env, silenciar)
  run_consolidation(env, silenciar)
  leer_resultados_ref(env)
}
