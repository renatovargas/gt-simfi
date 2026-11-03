# Entorno hijo con las globales que esperan los scripts de referencia.
# Nunca se lee ni escribe en .GlobalEnv ni se usa setwd().

#' Adjunta los paquetes del Master (mismo orden, mismo enmascaramiento).
adjuntar_paquetes_ref <- function(paquetes = REF_PAQUETES) {
  for (p in paquetes) {
    suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE))
  }
  invisible(paquetes)
}

#' Crea el entorno y el directorio de la corrida.
#'
#' @param tablas lista con `escenarios`, `sim_sub`, `sim_extra` (nombres originales).
#' @param session_dir directorio temporal propio de la corrida (se crea si no existe).
make_ref_env <- function(session_dir, tablas, params_base = leer_params_base(),
                         opc = OPC) {
  adjuntar_paquetes_ref()

  carpeta <- \(...) paste0(normalizePath(file.path(session_dir, ...), mustWork = FALSE), "/")
  fpresim  <- carpeta("presim")
  fdbmod   <- carpeta("mod")
  finput   <- carpeta("input")
  fresults <- carpeta("results")
  for (d in c(fpresim, fdbmod, finput, fresults)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # Datos base con los nombres que esperan los scripts
  origen <- vapply(names(REF_DATOS_PRESIM), \(f) ruta_app("data", f), character(1))
  stopifnot(all(file.exists(origen)))
  file.copy(origen, file.path(fpresim, REF_DATOS_PRESIM), overwrite = TRUE)

  # Libro de parámetros de la corrida
  fparams <- file.path(finput, "GTM23SUB_parametros.xlsx")
  write_params_xlsx(tablas, params_base, fparams)

  env <- new.env(parent = parent.env(globalenv()))
  env$OPC        <- opc
  env$finput     <- finput
  env$fpresim    <- fpresim
  env$fdbmod     <- fdbmod
  env$fresults   <- fresults
  env$fencovi    <- carpeta("encovi")
  env$fscripts   <- paste0(normalizePath(ruta_app("reference", "scripts_R")), "/")
  env$fparams    <- fparams
  env$escenarios <- preparar_escenarios(readxl::read_excel(fparams, sheet = "escenarios"))
  env$parm_glob  <- construir_parm_glob(params_base)

  sys.source(file.path(env$fscripts, REF_SCRIPTS[["funciones"]]), envir = env)
  env
}
