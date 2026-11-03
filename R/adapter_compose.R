# Composición de escenarios: registro de la app -> tablas del motor.
# Aquí viven los nombres de columna originales de `sim_sub` y `sim_extra`;
# los módulos solo manejan "campos" a través de estas constantes y funciones.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# Centinela de "sin componente / situación actual" en los desplegables.
# `selectize.js` trata "" como placeholder, por eso se usa "__ref__".
REF_SENTINELA <- "__ref__"

normalizar_ref <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1]) || identical(x[1], REF_SENTINELA)) "" else as.character(x[1])
}

# --- Esquema tarifario (sim_sub) -----------------------------------------------

SUB_DISTRIBUIDORAS <- c(eeg = "EEGSA", deoc = "DEOCSA", deor = "DEORSA")
SUB_N_TRAMOS <- 7L

#' Nombres de columna de `sim_sub` de una distribuidora.
sub_campos_dist <- function(emp) {
  list(
    plena      = paste0("t", emp, seq_len(SUB_N_TRAMOS)),
    objetivo   = paste0("to", emp, seq_len(SUB_N_TRAMOS)),
    cargo      = paste0("b", emp, seq_len(SUB_N_TRAMOS)),
    tope       = paste0("tt", emp),
    plena_gral = paste0("tplena", emp)
  )
}

SUB_CAMPOS_LIMITE <- paste0("block", seq_len(SUB_N_TRAMOS))
SUB_CAMPOS <- c(SUB_CAMPOS_LIMITE,
                unlist(lapply(names(SUB_DISTRIBUIDORAS), \(e) unlist(sub_campos_dist(e))),
                       use.names = FALSE))

# --- Compensación (sim_extra) --------------------------------------------------

COMP_CAMPOS <- c("grupo_com", "metodo_com", "valor_com", "pct_com", "decil_com", "elect_com")

COMP_GRUPOS <- c(
  "Monto fijo mensual a hogares hasta cierto decil de ingreso" = 1L,
  "Beneficiarios del bono social"                              = 2L,
  "Hogares con bajo consumo de electricidad"                   = 3L
)
COMP_METODOS_BONO <- c(
  "Monto fijo mensual"                       = 1L,
  "Porcentaje de la mediana anual del bono"  = 2L
)

# --- Payloads desde las filas de la plantilla -----------------------------------

#' Descarta columnas sin nombre (hay una vacía en `sim_sub`).
limpiar_columnas <- function(df) df[, !is.na(names(df)) & names(df) != "", drop = FALSE]

payload_sub_desde_fila <- function(fila) {
  v <- lapply(SUB_CAMPOS, \(c) if (c %in% names(fila)) as.numeric(fila[[c]][1]) else NA_real_)
  setNames(v, SUB_CAMPOS)
}

payload_comp_desde_fila <- function(fila) {
  v <- lapply(COMP_CAMPOS, \(c) if (c %in% names(fila)) as.numeric(fila[[c]][1]) else NA_real_)
  setNames(v, COMP_CAMPOS)
}

#' Deja en NA los campos que no aplican según el grupo de compensación
#' (el motor solo lee los relevantes), para comparar y guardar sin ruido.
normalizar_comp <- function(p) {
  g <- p$grupo_com; m <- p$metodo_com
  usa <- switch(as.character(g),
    "1" = c("grupo_com", "valor_com", "decil_com"),
    "2" = c("grupo_com", "metodo_com", if (identical(as.numeric(m), 2)) "pct_com" else "valor_com"),
    "3" = c("grupo_com", "valor_com", "elect_com"),
    "grupo_com")
  for (c in COMP_CAMPOS) if (!c %in% usa) p[[c]] <- NA_real_
  p[COMP_CAMPOS]
}

#' Biblioteca inicial: esquemas 1-2 y compensaciones 1-3 de la plantilla.
#' El esquema 0 es "situación actual" y no se guarda como componente.
biblioteca_inicial <- function(plantilla = tablas_escenario_plantilla()) {
  ss <- plantilla$sim_sub
  se <- plantilla$sim_extra
  fila_sub <- \(id) ss[ss$sim_sub == id, , drop = FALSE]
  fila_com <- \(id) se[se$sim_com == id, , drop = FALSE]
  list(
    sub = list(
      "Ampliar tarifa social hasta 100 kWh"   = payload_sub_desde_fila(fila_sub(1)),
      "Descuento porcentual focalizado por bloque" = payload_sub_desde_fila(fila_sub(2))
    ),
    comp = list(
      "Monto fijo hasta el decil 2"            = normalizar_comp(payload_comp_desde_fila(fila_com(1))),
      "Bono social: 150 % de la mediana"       = normalizar_comp(payload_comp_desde_fila(fila_com(2))),
      "Monto fijo a hogares con consumo bajo"  = normalizar_comp(payload_comp_desde_fila(fila_com(3)))
    )
  )
}

# --- Validación y composición ---------------------------------------------------

#' Valida una lista de escenarios (cada uno `list(name, sub, comp)`).
#' Devuelve un vector de mensajes de error (vacío si todo está bien).
validar_escenarios <- function(escenarios, libs) {
  err <- character()
  nombres <- vapply(escenarios, \(e) trimws(as.character(e$name %||% "")), "")
  if (any(!nzchar(nombres))) err <- c(err, "Todos los escenarios necesitan un nombre.")
  if (anyDuplicated(nombres)) err <- c(err, "Los nombres de escenario deben ser únicos.")
  for (e in escenarios) {
    s <- normalizar_ref(e$sub); k <- normalizar_ref(e$comp)
    if (nzchar(s) && is.null(libs$sub[[s]]))  err <- c(err, sprintf("No existe el esquema tarifario «%s».", s))
    if (nzchar(k) && is.null(libs$comp[[k]])) err <- c(err, sprintf("No existe la compensación «%s».", k))
  }
  unique(err)
}

#' Convierte el registro de escenarios en las tres tablas del motor
#' (`escenarios`, `sim_sub`, `sim_extra`) con los nombres de columna originales.
#'
#' El escenario 0 (base: tarifa vigente, sin compensación) se agrega siempre.
#' Los escenarios se numeran 1..n; los esquemas y compensaciones usados reciben
#' ids 1..m en orden de aparición ("" o "__ref__" = esquema 0 / sin compensación).
compose_scenario_inputs <- function(escenarios, libs, plantilla = tablas_escenario_plantilla()) {
  err <- validar_escenarios(escenarios, libs)
  if (length(err)) stop(paste(err, collapse = " "), call. = FALSE)

  esc0 <- limpiar_columnas(plantilla$escenarios)
  ss0  <- limpiar_columnas(plantilla$sim_sub)
  se0  <- limpiar_columnas(plantilla$sim_extra)

  nombres <- vapply(escenarios, \(e) trimws(e$name), "")
  subs    <- vapply(escenarios, \(e) normalizar_ref(e$sub), "")
  comps   <- vapply(escenarios, \(e) normalizar_ref(e$comp), "")
  usados_sub  <- unique(subs[nzchar(subs)])
  usados_comp <- unique(comps[nzchar(comps)])
  id_sub  <- \(x) if (nzchar(x)) match(x, usados_sub) else 0L
  id_comp <- \(x) if (nzchar(x)) match(x, usados_comp) else 0L

  # escenarios
  fila_esc <- \(k) {
    f <- esc0[esc0$escenario == 0, , drop = FALSE]
    f$escenario <- k; f$activo <- 1
    f$sim_sub <- id_sub(subs[k]); f$sim_com <- id_comp(comps[k])
    f$des_escenario <- nombres[k]; f$des_corto <- nombres[k]
    f$descripcion_larga <- paste0(
      "Esquema: ", if (nzchar(subs[k])) subs[k] else "tarifa vigente",
      ". Compensación: ", if (nzchar(comps[k])) comps[k] else "ninguna", ".")
    f
  }
  escenarios_t <- dplyr::bind_rows(esc0[esc0$escenario == 0, , drop = FALSE],
                                   lapply(seq_along(escenarios), fila_esc))

  # sim_sub
  fila_sub <- \(nm, id) {
    f <- ss0[ss0$sim_sub == 0, , drop = FALSE]
    p <- libs$sub[[nm]]
    for (c in SUB_CAMPOS) f[[c]] <- if (is.null(p[[c]])) NA_real_ else as.numeric(p[[c]])
    f$sim_sub <- id; f$activo <- 1; f$nom_subsidio <- nm
    f
  }
  sim_sub <- dplyr::bind_rows(
    ss0[ss0$sim_sub == 0, , drop = FALSE],
    lapply(seq_along(usados_sub), \(i) fila_sub(usados_sub[i], i))
  )

  # sim_extra
  fila_com <- \(nm, id) {
    f <- se0[se0$sim_com == 0, , drop = FALSE]
    p <- libs$comp[[nm]]
    for (c in COMP_CAMPOS) f[[c]] <- if (is.null(p[[c]])) NA_real_ else as.numeric(p[[c]])
    f$sim_com <- id; f$activo <- 1; f$descr_com <- nm; f$check_no_borrar <- id
    f
  }
  sim_extra <- dplyr::bind_rows(
    se0[se0$sim_com == 0, , drop = FALSE],
    lapply(seq_along(usados_comp), \(i) fila_com(usados_comp[i], i))
  )

  list(escenarios = escenarios_t, sim_sub = sim_sub, sim_extra = sim_extra)
}
