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

#' Biblioteca inicial de una sesión nueva: vacía. El esquema 0 es la
#' "situación actual" y no se guarda como componente.
biblioteca_inicial <- function() list(sub = list(), comp = list())

#' Biblioteca de ejemplo (esquemas 1-2 y compensaciones 1-3 de la plantilla).
#' Solo para pruebas; la app arranca con la biblioteca vacía.
biblioteca_ejemplo <- function(plantilla = tablas_escenario_plantilla()) {
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

#' Valida los valores de un esquema tarifario. Devuelve mensajes (vacío si está bien).
validar_esquema <- function(p) {
  err <- character()
  val <- \(c) suppressWarnings(as.numeric(p[[c]] %||% NA_real_))
  lim <- vapply(SUB_CAMPOS_LIMITE, val, numeric(1))
  n <- if (anyNA(lim)) which(is.na(lim))[1] - 1L else length(lim)
  if (n < 1 || lim[1] != 0 || any(!is.na(lim[-seq_len(n)])) || is.unsorted(lim[seq_len(n)], strictly = TRUE)) {
    err <- c(err, "Los tramos deben empezar en 0 kWh y crecer en orden, sin dejar huecos.")
    n <- sum(!is.na(lim))
  }
  for (emp in names(SUB_DISTRIBUIDORAS)) {
    cam <- sub_campos_dist(emp)
    nom <- SUB_DISTRIBUIDORAS[[emp]]
    if (is.na(val(cam$tope))) err <- c(err, sprintf("Falta la tarifa tope de %s.", nom))
    sin_plena <- which(is.na(vapply(cam$plena[seq_len(n)], val, numeric(1))))
    if (length(sin_plena)) {
      err <- c(err, sprintf("Falta la tarifa plena de %s en el tramo %s.", nom, paste(sin_plena, collapse = ", ")))
    }
    neg <- vapply(c(cam$plena, cam$objetivo, cam$cargo, cam$tope), \(c) isTRUE(val(c) < 0), logical(1))
    if (any(neg)) err <- c(err, sprintf("Hay valores negativos en %s.", nom))
  }
  err
}

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
    if (nzchar(s) && !is.null(libs$sub[[s]])) {
      e_sub <- validar_esquema(libs$sub[[s]])
      if (length(e_sub)) err <- c(err, sprintf("El esquema «%s» está incompleto: %s", s, paste(e_sub, collapse = " ")))
    }
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

# --- Descripción de componentes (pantalla de escenarios) ----------------------------

#' Cambios de un esquema tarifario frente a la tarifa vigente: una fila por valor
#' distinto (límites de consumo, tarifas plena/objetivo/tope y aporte social por
#' tramo). Data frame vacío si el esquema es igual al vigente.
cambios_esquema <- function(p, base) {
  val <- \(x, c) suppressWarnings(as.numeric(x[[c]] %||% NA_real_))
  distinto <- \(a, b) !(is.na(a) && is.na(b)) && (is.na(a) || is.na(b) || abs(a - b) > 1e-9)
  fmt <- \(v, d = 4) if (is.na(v)) "vacío" else formatC(v, format = "f", digits = d, big.mark = ",")
  # Aporte social por kWh (misma regla que el script 02 del pipeline: max(0, min(plena, tope) - objetivo))
  aporte <- \(x, cam, k) {
    pl <- val(x, cam$plena[k]); tp <- val(x, cam$tope); ob <- val(x, cam$objetivo[k])
    lim <- suppressWarnings(min(c(pl, tp), na.rm = TRUE))
    if (is.na(ob) || !is.finite(lim)) 0 else max(0, lim - ob)
  }
  filas <- list()
  agregar <- \(param, dist, tramo, a, b, d = 4) {
    filas[[length(filas) + 1L]] <<- data.frame(
      `Distribuidora` = dist, `Tramo` = tramo, `Valor` = param,
      `Vigente` = fmt(a, d), `Escenario` = fmt(b, d), check.names = FALSE, stringsAsFactors = FALSE)
  }
  for (k in seq_len(SUB_N_TRAMOS)) {
    a <- val(base, SUB_CAMPOS_LIMITE[k]); b <- val(p, SUB_CAMPOS_LIMITE[k])
    if (distinto(a, b)) agregar("Límite de consumo (desde, kWh)", "Todas", as.character(k), a, b, 0)
  }
  for (emp in names(SUB_DISTRIBUIDORAS)) {
    cam <- sub_campos_dist(emp); nom <- SUB_DISTRIBUIDORAS[[emp]]
    a <- val(base, cam$tope); b <- val(p, cam$tope)
    if (distinto(a, b)) agregar("Tarifa tope (Q/kWh)", nom, "Todos", a, b)
    for (k in seq_len(SUB_N_TRAMOS)) {
      if (is.na(val(base, SUB_CAMPOS_LIMITE[k])) && is.na(val(p, SUB_CAMPOS_LIMITE[k]))) next
      for (cc in list(c("Tarifa plena (Q/kWh)", cam$plena[k]), c("Tarifa objetivo (Q/kWh)", cam$objetivo[k]))) {
        a <- val(base, cc[2]); b <- val(p, cc[2])
        if (distinto(a, b)) agregar(cc[1], nom, as.character(k), a, b)
      }
      a <- aporte(base, cam, k); b <- aporte(p, cam, k)
      if (distinto(a, b)) agregar("Aporte social (Q/kWh)", nom, as.character(k), a, b)
    }
  }
  if (!length(filas)) {
    return(data.frame(Distribuidora = character(), Tramo = character(), Valor = character(),
                      Vigente = character(), Escenario = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, filas)
}

#' Descripción en una frase de una compensación guardada.
describir_comp <- function(p) {
  if (is.null(p)) return("Sin compensación.")
  num <- \(x) suppressWarnings(as.numeric(x %||% NA_real_))
  q <- \(x) paste0("Q", formatC(num(x), format = "f", digits = 0, big.mark = ","))
  switch(as.character(num(p$grupo_com)),
    "1" = sprintf("%s mensuales por hogar a los hogares de los deciles 1 a %d de ingreso.",
                  q(p$valor_com), as.integer(num(p$decil_com))),
    "2" = if (identical(num(p$metodo_com), 2)) {
      sprintf("A los beneficiarios del bono social: %s %% de la mediana anual del bono.",
              formatC(num(p$pct_com), format = "f", digits = 0))
    } else {
      sprintf("%s mensuales por hogar a los beneficiarios del bono social.", q(p$valor_com))
    },
    "3" = sprintf("%s mensuales por hogar a los hogares que consumen menos de %s kWh al mes.",
                  q(p$valor_com), formatC(num(p$elect_com), format = "f", digits = 0)),
    "Sin compensación.")
}
