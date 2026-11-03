# Almacén de componentes y escenarios.
#
# Una biblioteca (`libs`) tiene dos tipos de componente, cada uno una lista
# nombrada `nombre -> payload` (campos numéricos de SUB_CAMPOS / COMP_CAMPOS):
#   libs$sub[[nombre]]   esquema tarifario
#   libs$comp[[nombre]]  compensación
# Un escenario compone un componente de cada tipo por nombre ("" / "__ref__" =
# situación actual o sin compensación): list(name, sub, comp, comparar).
#
# Persistencia: autosave local (RDS en app_state/) y Excel para compartir
# (hojas `esquemas`, `compensaciones` y `escenarios`, formato ancho).

# --- Excel ----------------------------------------------------------------------

.libs_a_df <- function(lista, campos) {
  if (!length(lista)) {
    d <- as.data.frame(matrix(numeric(), 0, length(campos), dimnames = list(NULL, campos)))
    return(cbind(componente = character(), d))
  }
  filas <- lapply(names(lista), \(nm) {
    v <- vapply(campos, \(c) as.numeric(lista[[nm]][[c]] %||% NA_real_), numeric(1))
    cbind(data.frame(componente = nm, stringsAsFactors = FALSE), as.data.frame(as.list(v)))
  })
  do.call(rbind, filas)
}

.df_a_libs <- function(df, campos) {
  out <- list()
  if (is.null(df) || !nrow(df) || !"componente" %in% names(df)) return(out)
  for (i in seq_len(nrow(df))) {
    nm <- as.character(df$componente[i])
    if (is.na(nm) || !nzchar(nm)) next
    out[[nm]] <- setNames(lapply(campos, \(c) if (c %in% names(df)) suppressWarnings(as.numeric(df[[c]][i])) else NA_real_), campos)
  }
  out
}

.escenarios_a_df <- function(escenarios) {
  if (!length(escenarios)) {
    return(data.frame(escenario = character(), esquema = character(),
                      compensacion = character(), comparar = logical()))
  }
  do.call(rbind, lapply(escenarios, \(e) data.frame(
    escenario = as.character(e$name %||% ""),
    esquema = normalizar_ref(e$sub), compensacion = normalizar_ref(e$comp),
    comparar = isTRUE(e$comparar), stringsAsFactors = FALSE)))
}

.df_a_escenarios <- function(df) {
  if (is.null(df) || !nrow(df)) return(list())
  lapply(seq_len(nrow(df)), \(i) list(
    name = as.character(df$escenario[i]),
    sub = as.character(df$esquema[i] %||% ""),
    comp = as.character(df$compensacion[i] %||% ""),
    comparar = isTRUE(as.logical(df$comparar[i]))))
}

guardar_xlsx <- function(ruta, libs, escenarios) {
  hojas <- list(
    esquemas       = .libs_a_df(libs$sub, SUB_CAMPOS),
    compensaciones = .libs_a_df(libs$comp, COMP_CAMPOS),
    escenarios     = .escenarios_a_df(escenarios)
  )
  openxlsx::write.xlsx(hojas, file = ruta, overwrite = TRUE)
  invisible(ruta)
}

#' Lee un libro exportado por la app. Devuelve `list(libs, escenarios)` o falla
#' con un mensaje claro si no tiene el formato esperado.
leer_xlsx <- function(ruta) {
  hojas <- tryCatch(openxlsx::getSheetNames(ruta), error = \(e) character())
  falta <- setdiff(c("esquemas", "compensaciones", "escenarios"), hojas)
  if (length(falta)) stop("El archivo no tiene el formato esperado (faltan las hojas: ",
                          paste(falta, collapse = ", "), ").", call. = FALSE)
  rd <- \(h) openxlsx::read.xlsx(ruta, sheet = h)
  libs <- list(sub = .df_a_libs(rd("esquemas"), SUB_CAMPOS),
               comp = lapply(.df_a_libs(rd("compensaciones"), COMP_CAMPOS), normalizar_comp))
  list(libs = libs, escenarios = .df_a_escenarios(rd("escenarios")))
}

# --- Autosave local (RDS) ---------------------------------------------------------

ruta_autosave <- function(raiz = ".") {
  dir <- file.path(raiz, "app_state")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, "autosave.rds")
}

autosave_escribir <- function(raiz, libs, escenarios) {
  tryCatch({
    destino <- ruta_autosave(raiz)
    tmp <- paste0(destino, ".tmp")
    saveRDS(list(libs = libs, escenarios = escenarios, ts = Sys.time()), tmp)
    file.rename(tmp, destino)
    TRUE
  }, error = \(e) FALSE)
}

autosave_leer <- function(raiz = ".") {
  ruta <- file.path(raiz, "app_state", "autosave.rds")
  if (!file.exists(ruta)) return(NULL)
  tryCatch(readRDS(ruta), error = \(e) NULL)
}

autosave_borrar <- function(raiz = ".") {
  ruta <- file.path(raiz, "app_state", "autosave.rds")
  if (file.exists(ruta)) unlink(ruta)
  invisible(TRUE)
}
