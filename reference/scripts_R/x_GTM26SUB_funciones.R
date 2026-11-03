## =============================================================================
## Banco Mundial / INDE
## Herramienta de Microsimulación de Subsidios a la Electricidad - Guatemala
## x_GTM26SUB_funciones.R
##
## Funciones compartidas por los scripts 06-10. Se cargan desde el master.
##   - Lectura de insumos (.rds, con respaldo .dta)
##   - Réplicas de conductas de Stata (round, merge, missing, percentiles)
##   - Clasificaciones (distribuidora, región, estratos)
##   - Indicadores (FGT, Gini, Theil, Palma, concentración, Kakwani)
## =============================================================================

## 1. Lectura ------------------------------------------------------------------

#' Lee un insumo. Acepta ruta con o sin extensión: busca .rds / .RDS y, si no
#' existe, .dta (haven). Las etiquetas de valor de Stata se eliminan para que
#' las variables queden numéricas.
leer_base <- function(ruta) {
  base_sin_ext <- sub("\\.(rds|RDS|dta)$", "", ruta)
  cands <- paste0(base_sin_ext, c(".rds", ".RDS", ".dta"))
  f <- cands[file.exists(cands)][1]
  if (is.na(f)) stop("No se encontró el insumo: ", base_sin_ext, ".rds / .dta")
  x <- if (grepl("\\.dta$", f)) haven::read_dta(f) else readRDS(f)
  x <- haven::zap_labels(haven::zap_formats(x))
  tibble::as_tibble(x)
}

## 2. Conductas de Stata -------------------------------------------------------

#' round(x, y) de Stata: y * floor(x / y + 0.5)
round_st <- function(x, y = 1) y * floor(x / y + 0.5)

#' Igualdad "segura": FALSE cuando x es missing (en Stata, . == 9 es falso)
eqv <- function(x, v) !is.na(x) & x %in% v

#' x <= v con la regla de Stata: missing es +infinito
le_st <- function(x, v) !is.na(x) & x <= v
lt_st <- function(x, v) !is.na(x) & x <  v
#' x > v / x >= v: un missing en x es VERDADERO (missing = +infinito)
gt_st <- function(x, v) is.na(x) | x >  v
ge_st <- function(x, v) is.na(x) | x >= v

#' max()/min() de Stata con escalares: ignoran missing salvo que todos lo sean
max_st <- function(...) pmax(..., na.rm = TRUE)
min_st <- function(...) pmin(..., na.rm = TRUE)

#' merge de Stata (sin update): los registros que existen en master conservan
#' los valores de master; los que solo están en using toman los de using.
#' keep: subconjunto de c("master", "using", "match").
stata_merge <- function(master, using, by, keepusing = NULL,
                        keep = c("master", "using", "match"),
                        tipo = c("m:1", "1:1", "1:m")) {
  tipo <- match.arg(tipo)
  if (!is.null(keepusing)) using <- using[, unique(c(by, keepusing)), drop = FALSE]
  if (tipo %in% c("m:1", "1:1") && anyDuplicated(using[by]))
    stop("stata_merge: la llave ", paste(by, collapse = "+"),
         " no es única en using")
  if (tipo %in% c("1:m", "1:1") && anyDuplicated(master[by]))
    stop("stata_merge: la llave ", paste(by, collapse = "+"),
         " no es única en master")
  master$.in_m <- TRUE
  using$.in_u  <- TRUE
  comunes <- setdiff(intersect(names(master), names(using)), c(by, ".in_m", ".in_u"))
  out <- dplyr::full_join(master, using, by = by, suffix = c("", ".u"))
  for (v in comunes) {
    vu <- paste0(v, ".u")
    solo_u <- is.na(out$.in_m)
    out[[v]][solo_u] <- out[[vu]][solo_u]
    out[[vu]] <- NULL
  }
  out$.merge <- dplyr::case_when(
    !is.na(out$.in_m) & is.na(out$.in_u) ~ "master",
    is.na(out$.in_m) & !is.na(out$.in_u) ~ "using",
    TRUE ~ "match"
  )
  out <- out[out$.merge %in% keep, , drop = FALSE]
  print(table(.merge = out$.merge))
  out$.in_m <- NULL
  out$.in_u <- NULL
  out$.merge <- NULL
  out
}

#' Percentil ponderado como summarize, detail [aw=] de Stata
wpctile_st <- function(x, w, p) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (!length(x)) return(NA_real_)
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w); P <- p / 100 * sum(w)
  i <- which(cw >= P * (1 - 1e-12))[1]
  if (abs(cw[i] - P) <= 1e-9 * sum(w) && i < length(x)) (x[i] + x[i + 1]) / 2 else x[i]
}

#' Tabla ponderada rápida (reemplazo de tab ... [aw=]) en porcentaje
tab_w <- function(g, w = NULL, titulo = NULL) {
  if (is.null(w)) w <- rep(1, length(g))
  t <- tapply(w, addNA(factor(g), ifany = TRUE), sum)
  out <- data.frame(valor = names(t), n_pond = as.numeric(t),
                    pct = round(100 * as.numeric(t) / sum(t), 2))
  if (!is.null(titulo)) cat("\n--", titulo, "--\n")
  print(out, row.names = FALSE)
  invisible(out)
}

## 3. Clasificaciones ----------------------------------------------------------

#' Distribuidora por departamento: 1 EEGSA, 2 DEOCSA, 3 DEORSA
#' PLACEHOLDER: cooperativas (Ixcán, Joyabaj, Retalhuleu) sin identificar.
empresa_de_depto <- function(depto) {
  dplyr::case_when(
    depto %in% c(1, 3, 5)                         ~ 1L,
    depto %in% c(4, 7:14)                         ~ 2L,
    depto %in% c(2, 6, 15:22)                     ~ 3L,
    TRUE                                          ~ NA_integer_
  )
}
EMPRESAS <- c("1" = "EEGSA", "2" = "DEOCSA", "3" = "DEORSA")

#' Regiones administrativas (8) a partir del código INE de departamento
region_de_depto <- function(depto) {
  dplyr::case_when(
    depto == 1                     ~ 1L,  # Metropolitana
    depto %in% c(15, 16)           ~ 2L,  # Norte
    depto %in% c(2, 18, 19, 20)    ~ 3L,  # Nororiente
    depto %in% c(6, 21, 22)        ~ 4L,  # Suroriente
    depto %in% c(3, 4, 5)          ~ 5L,  # Central
    depto %in% c(7:12)             ~ 6L,  # Suroccidente
    depto %in% c(13, 14)           ~ 7L,  # Noroccidente
    depto == 17                    ~ 8L,  # Petén
    TRUE                           ~ NA_integer_
  )
}
REGIONES <- c("1" = "Metropolitana", "2" = "Norte", "3" = "Nororiente",
              "4" = "Suroriente", "5" = "Central", "6" = "Suroccidente",
              "7" = "Noroccidente", "8" = "Petén")

#' Umbrales de estratos en Q anuales por persona
umbrales_estrato <- function(opc = OPC) {
  opc$ppp_fac * 365 * c(opc$est_pext, opc$est_pob, opc$est_vul)
}
ESTRATOS <- c("1" = "Pobre extremo", "2" = "Pobre", "3" = "Vulnerable",
              "4" = "Clase media o más")

#' estrato4 de 09_GTM26SUB_Consolida.do
estrato4 <- function(y, umbr = umbrales_estrato()) {
  dplyr::case_when(
    is.na(y)       ~ NA_integer_,
    y <  umbr[1]   ~ 1L,
    y <  umbr[2]   ~ 2L,
    y <  umbr[3]   ~ 3L,
    TRUE           ~ 4L
  )
}

## 4. Indicadores --------------------------------------------------------------

#' FGT(alpha) en porcentaje de personas
fgt <- function(y, z, w, alpha = 0) {
  ok <- !is.na(y) & !is.na(z) & !is.na(w)
  y <- y[ok]; z <- z[ok]; w <- w[ok]
  g <- ifelse(y < z, (z - y) / z, 0)
  100 * sum(w * ifelse(y < z, g^alpha, 0)) / sum(w)
}

#' Curva de Lorenz ponderada (P, L), ordenada por y
lorenz <- function(y, w) {
  o <- order(y); y <- y[o]; w <- w[o]
  W <- sum(w); mu <- sum(w * y) / W
  list(P = cumsum(w) / W, L = cumsum(w * y) / (mu * W))
}

lorenz_at <- function(P, L, p) {
  i <- sum(P < p) + 1
  if (i > length(P)) return(1)
  p0 <- if (i == 1) 0 else P[i - 1]
  l0 <- if (i == 1) 0 else L[i - 1]
  l0 + (L[i] - l0) * (p - p0) / (P[i] - p0)
}

#' Gini, Theil T y Palma (igual que _ineq() en 10_GTM26SUB_Reportes.do)
ineq3 <- function(y, w) {
  ok <- !is.na(y) & !is.na(w)
  y <- y[ok]; w <- w[ok]
  lz <- lorenz(y, w)
  n  <- length(y)
  Pl <- c(0, lz$P[-n]); Ll <- c(0, lz$L[-n])
  W  <- sum(w); mu <- sum(w * y) / W
  r  <- y / mu
  t  <- ifelse(r > 0, w * r * log(r), 0)
  c(Gini  = 1 - sum((lz$P - Pl) * (lz$L + Ll)),
    Theil = sum(t) / W,
    Palma = (1 - lorenz_at(lz$P, lz$L, 0.9)) / lorenz_at(lz$P, lz$L, 0.4))
}

#' Índice de concentración de y ordenado por x (igual que en RD)
conc_index <- function(y, x, w) {
  ok <- !is.na(y) & !is.na(x) & !is.na(w)
  y <- y[ok]; x <- x[ok]; w <- w[ok]
  o <- order(x); y <- y[o]; w <- w[o] / sum(w[o])
  n <- length(y)
  wc <- cumsum(w); yc <- cumsum(y * w)
  if (yc[n] == 0) return(NA_real_)
  yc <- yc / yc[n]
  sum(yc[-1] * wc[-n]) - sum(yc[-n] * wc[-1])
}

## 5. Utilidades varias --------------------------------------------------------

#' Mensaje de sección en consola
seccion <- function(...) message("\n==== ", paste0(...), " ====")

#' Suma ponderada en millones
msum <- function(x, w) sum(x * w, na.rm = TRUE) / 1e6
