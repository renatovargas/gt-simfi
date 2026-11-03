## =============================================================================
## Banco Mundial / INDE
## Herramienta de Microsimulación de Subsidios a la Electricidad
## Country: Guatemala 2023
## 06_GTM26SUB_BaseIngreso_fix.R
##
## Reemplazo de 06_GTM26SUB_BaseIngreso.R, reescrito a partir de la versión
## vigente de 02_codigo/do_files/06_GTM26SUB_BaseIngreso.do. NO modifica el 06
## original. Dependencias: 00_GTM26SUB_Master.R (OPC, fencovi, fpresim),
## x_GTM26SUB_funciones.R.
##
## Qué cambia respecto del 06 en R
##   1. `pobre` -> `pobreza` (Stata acepta la abreviatura; R no).
##   2. Tarifas: 6 tramos de tarifa social por distribuidora y período
##      (TS_EEGSA / TS_DEOCSA / TS_DEORSA) y límites 60/88/100/125/300. El 06 en
##      R usaba 3 tramos (TS + TN) y por eso la factura teórica no coincidía.
##   3. Verosimilitud del kWh: ventanas w25 = 8, w10 = 3, w5 = 1 (y 1 para el
##      resto); el 06 en R usaba 10/5/2.
##   4. Raking: una distribuidora a la vez, con normalización del prior en cada
##      iteración, hasta tol_rake = 1e-8 o n_iter = 500 (R: 25 iteraciones).
##   5. Salidas adicionales celq_esp y cele_esp (esperanzas del posterior), y
##      p_sub / celq_esp / cele_esp deterministas para los hogares fuera de la
##      calibración, como en el .do.
##   6. Solo la Parte 1 (energia_ajustado.rds). La Parte 2 del .do
##      (disp_income) la hace 01_GTM26SUB_BaseIngreso.R, que se verificó
##      idéntico a disp_income.rds.
##
## Los sorteos usan el generador de R: la distribución es la misma que en Stata
## (misma semilla nominal), los valores individuales no coinciden.
##
## Input:  fencovi 2_encovi_2023_hogares, 3_encovi_2023_c01se_fuentes,
##         35_encovi_2023_agregado_consumo; OPC$adm_file
## Output: fpresim energia_ajustado.rds
## =============================================================================

# 0. Parámetros de la sección ---------------------------------------------------

tol_cons <- 0.15   # kWh y gasto "consistentes" si difieren <= 15 %
k_min    <- 2      # kWh reportado < k_min = "sin dato"
bw       <- 25     # ancho del rango objetivo (kWh)
nb_adm   <- 300 / bw
K_MAX    <- 300
k_ts     <- 88     # kWh máximo "tarifa social" (p_sub y tarifa_social)

cal <- list(
  sig_p  = 0.25, sig_pr = 0.35,           # gasto no redondo / redondo (Q25)
  delta  = c(1, 1, 1),                    # factor de nivel por distribuidora
  w25 = 8, w10 = 3, w5 = 1,               # ventanas de redondeo del kWh
  sig_k  = 0.50, eps_v = 0.02,
  n_iter = 500, tol_rake = 1e-8
)
M <- OPC$M

mean_st <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

# 1. Base de consumo de energía eléctrica ENCOVI 2023 ---------------------------

## 1.1 Acceso a energía eléctrica ----------------------------------------------
seccion("06fix - 1.1 Datos ENCOVI")

hogar <- leer_base(paste0(fencovi, "2_encovi_2023_hogares")) %>%
  select(no_hogar, p01a05c, p01d24:p01d29, total_pers)

## 1.2 Gastos en energía --------------------------------------------------------
fuentes <- as.data.table(leer_base(paste0(fencovi, "3_encovi_2023_c01se_fuentes")))
fuentes[, item := id_fuentes + 410]
fuentes[, `:=`(cantidad = p01e08, conspago = p01e09 * 12)]

## collapse (sum) -> suma con missing = 0; (mean) -> media sin missing
## (en el .do `pobre` es la abreviatura de `pobreza`)
energia <- fuentes[, .(
  cantidad = sum(cantidad, na.rm = TRUE),
  conspago = sum(conspago, na.rm = TRUE),
  area   = mean_st(area),   factor = mean_st(factor),
  depto  = mean_st(depto),  pobre  = mean_st(pobreza),
  p01e02 = mean_st(p01e02), p01e03 = mean_st(p01e03),
  p01e04 = mean_st(p01e04), p01e05 = mean_st(p01e05)
), by = .(item, no_hogar)][item == 414]
energia <- as_tibble(energia)
rm(fuentes)

cat("Hogares con gasto en electricidad:",
    sum(energia$conspago != 0 & !is.na(energia$conspago)), "\n")  # 9436 en Stata

agregado <- leer_base(paste0(fencovi, "35_encovi_2023_agregado_consumo"))

energia <- energia %>%
  stata_merge(hogar, by = "no_hogar") %>%
  stata_merge(agregado, by = "no_hogar") %>%
  mutate(personas = coalesce(personas, total_pers)) %>%
  rename(hhid = no_hogar) %>%
  mutate(codigo = item, pccons = conspago / personas)

cat("total_pers != personas:", sum(energia$total_pers != energia$personas,
                                   na.rm = TRUE), "\n")

## desvia_ pccons: desviaciones estándar por area x codigo
energia <- energia %>%
  mutate(pccons = if_else(eqv(pccons, 0), NA_real_, pccons)) %>%
  group_by(area, codigo) %>%
  mutate(pccons_m  = mean(pccons, na.rm = TRUE),
         pccons_sd = if (sum(!is.na(pccons)) > 1) sd(pccons, na.rm = TRUE) else NA_real_) %>%
  ungroup() %>%
  mutate(ndesvia_pccons = if_else(!is.na(pccons_sd) & pccons_sd > 0,
                                  (pccons - pccons_m) / pccons_sd, NA_real_))

## Gasto per cápita extremo (> 6 desv.) en hogares que declaran no tener red o
## no usar electricidad: su gasto/kWh no es de electricidad
out_6 <- eqv(energia$item, 414) & !is.na(energia$ndesvia_pccons) &
  energia$ndesvia_pccons > 6 &
  (eqv(energia$p01d24, 2) | eqv(energia$p01a05c, 2))
energia$conspago[out_6]       <- NA
energia$cantidad[out_6]       <- NA
energia$ndesvia_pccons[out_6] <- NA
cat("Observaciones extremas anuladas:", sum(out_6), "\n")

energia <- energia %>%
  mutate(
    cele = if_else(eqv(item, 414), conspago / 12, NA_real_),
    celq = if_else(eqv(item, 414), cantidad,      NA_real_),
    empresa_elec = empresa_de_depto(depto)
  )

## Quintil de consumo
quint <- agregado %>%
  mutate(quintil_e = statar::xtile(agreg3, n = 5, wt = factor3)) %>%
  select(hhid = no_hogar, aguas3, quintil_e)
energia <- stata_merge(energia, quint, by = "hhid")

energia <- energia %>%
  mutate(
    periodo_elec = case_when(mes >= 8  & mes <= 10 ~ 1L,
                             mes >= 11 & mes <= 12 ~ 2L,
                             TRUE ~ NA_integer_),
    ## con electricidad y la usó el mes pasado (p01d24 != 1 -> 0, incl. missing)
    inf_elect = as.integer(eqv(p01a05c, 1) & eqv(p01d24, 1)),
    celq_o = celq,
    cele_n = cele
  )

## 1.3 Tarifas (septiembre / diciembre 2023) -----------------------------------
## Supuestos (verificar contra el pliego):
##   pago = (base + round(tarifa_tramo * kWh, 0.01)) * (1.12 + apc)  si apc < 1
##   pago = round((base + ...) * 1.12, 0.01) + apc                   si apc > 1
##   tarifa_tramo se aplica al consumo TOTAL (no marginal); 6 tramos con
##   límites 60 / 88 / 100 / 125 / 300 kWh (el sexto: > 300).

TS_EEGSA  <- rbind(c(0.74699, 1.16979, 1.24699, 1.24699, 1.38601, 1.47405),
                   c(0.74699, 1.16979, 1.38601, 1.38601, 1.38601, 1.47405))
TS_DEOCSA <- rbind(c(0.87656, 1.29936, 1.37656, 1.37656, 2.196618, 2.328405),
                   c(0.87655, 1.29935, 2.196611, 2.196611, 2.196611, 2.328403))
TS_DEORSA <- rbind(c(0.83358, 1.25638, 1.33358, 1.33358, 2.10361, 2.198388),
                   c(0.83359, 1.25639, 2.10361, 2.10361, 2.10361, 2.198386))

TAR <- list(
  ## Alumbrado público por departamento (apc<1: proporción; apc>1: Q fijos)
  apc = c(0.13, 26.40, 0.11, 26.00, 0.15, 34.00, 19.80, 17.50, 35.40, 36.30,
          35.90, 26.50, 20.50, 18.20, 27.50, 26.50, 39.80, 36.60, 23.80, 12.60,
          27.20, 23.00),
  lim_ts = rbind(c(60, 88, 100, 125, 300), c(60, 88, 100, 125, 300)),  # período x límite
  base   = rbind(c(10.985336, 10.985335),                              # empresa x período
                 c(22.842086, 23.27439),
                 c(22.551741, 22.964455)),
  ts     = array(NA_real_, c(3, 2, 6))                                 # empresa x período x tramo
)
TAR$ts[1, , ] <- TS_EEGSA
TAR$ts[2, , ] <- TS_DEOCSA
TAR$ts[3, , ] <- TS_DEORSA

## 1.4 Funciones: parámetros por hogar, factura teórica e inversa ---------------

#' tar_params: parámetros tarifarios de cada hogar (depto/empresa/período)
tar_params <- function(depto, empresa, periodo) {
  ok <- !is.na(depto) & !is.na(empresa) & !is.na(periodo)
  e <- ifelse(ok, empresa, NA); p <- ifelse(ok, periodo, NA)
  d <- ifelse(ok, depto, NA)
  out <- list(ok = ok,
              base = round_st(TAR$base[cbind(e, p)], 0.01),
              apc  = TAR$apc[d])
  for (i in 1:6) out[[paste0("r", i)]] <- TAR$ts[cbind(e, p, i)]
  for (i in 1:5) out[[paste0("l", i)]] <- TAR$lim_ts[cbind(p, i)]
  out
}

#' tar_pago: factura mensual teórica (Q) para un consumo entero k
#' k puede ser vector (largo n) o matriz n x K (los parámetros se reciclan por fila)
tar_pago <- function(k, P) {
  nn <- length(k)
  r  <- function(v) rep_len(v, nn)        # reciclaje por fila (column-major)
  base <- r(P$base); apc <- r(P$apc)
  rate <- ifelse(k <= r(P$l1), r(P$r1),
          ifelse(k <= r(P$l2), r(P$r2),
          ifelse(k <= r(P$l3), r(P$r3),
          ifelse(k <= r(P$l4), r(P$r4),
          ifelse(k <= r(P$l5), r(P$r5), r(P$r6))))))
  core <- base + round_st(rate * k, 0.01)
  out  <- ifelse(apc < 1,
                 round_st(core * (1.12 + apc), 0.01),
                 round_st(core * 1.12, 0.01) + apc)
  ok <- r(P$ok) & !is.na(k)
  out[!ok | eqv(apc, 1)] <- NA
  dim(out) <- dim(k)
  out
}

#' tar_inv: kWh entero cuya factura teórica es la más cercana al gasto p
tar_inv <- function(p, P) {
  inner <- ifelse(P$apc < 1, p / (1.12 + P$apc), (p - P$apc) / 1.12)
  inner[!(P$ok & !is.na(p))] <- NA
  lim <- cbind(0, P$l1, P$l2, P$l3, P$l4, P$l5)   # límite inferior de cada tramo
  kout <- NULL; eout <- NULL
  for (i in 1:6) {
    ri <- P[[paste0("r", i)]]
    ki <- pmax(round_st((inner - P$base) / ri), lim[, i] + 1)
    if (i < 6) ki <- pmin(ki, P[[paste0("l", i)]])
    ei <- abs(tar_pago(ki, P) - p)
    if (i == 1) {
      kout <- ki; eout <- ei
    } else {
      s <- !is.na(ei) & !is.na(eout) & ei < eout
      kout[s] <- ki[s]; eout <- pmin(eout, ei)
    }
  }
  eout <- eout / p
  eout[!is.finite(eout)] <- NA
  list(k = kout, e = eout)
}

#' Autoprueba: kWh -> factura -> kWh para todo depto/período (1..700 kWh)
tar_selftest <- function() {
  g <- expand.grid(kwh = 1:700, periodo = 1:2, depto = 1:22)
  g$empresa <- empresa_de_depto(g$depto)
  P  <- tar_params(g$depto, g$empresa, g$periodo)
  g$pago <- tar_pago(g$kwh, P)
  g$krec <- tar_inv(g$pago, P)$k
  g <- g[order(g$depto, g$periodo, g$kwh), ]
  mal  <- sum(g$krec != g$kwh)
  mono <- sum(ave(g$pago, g$depto, g$periodo,
                  FUN = function(x) c(FALSE, diff(x) <= 0)) == 1)
  cat("Autoprueba -> kWh no recuperados:", mal,
      "| puntos donde la factura no crece con el kWh:", mono, "\n")
  if (mal > 0) print(head(g[g$krec != g$kwh, ], 20))
  stopifnot("Autoprueba de tarifas falló" = mal == 0)
}
tar_selftest()

## 1.5 Preparación y clasificación de hogares ----------------------------------
seccion("06fix - 1.5 Clasificación de hogares")

energia <- energia %>%
  mutate(celq_o1 = celq, cele_o1 = cele)

n_sinper <- sum(is.na(energia$periodo_elec))
if (n_sinper > 0) message("Aviso: ", n_sinper,
                          " hogares sin periodo_elec (mes fuera de 8-12): clase 0")

P <- tar_params(energia$depto, energia$empresa_elec, energia$periodo_elec)
energia$ok_par <- P$ok
energia$p_min  <- tar_pago(ifelse(P$ok, 1, NA), P)

## Hogares que declaran no usar / no tener electricidad
energia <- energia %>%
  mutate(sin_uso = (eqv(p01e02, 9) & eqv(p01e03, 9) & eqv(p01e04, 9) &
                      eqv(p01e05, 9)) | eqv(p01a05c, 2) | eqv(p01d24, 2),
         cele_n = if_else(coalesce(le_st(celq, 1) & ge_st(cele, 0) & ok_par &
                                     lt_st(cele_n, p_min) & sin_uso, FALSE),
                          NA_real_, cele_n))

## drop if missing(quintil_e)  (se recalculan los parámetros sobre la base final)
energia <- energia %>% filter(!is.na(quintil_e))
P <- tar_params(energia$depto, energia$empresa_elec, energia$periodo_elec)

## Dos lecturas del mismo dato
##   k_rep: kWh reportado (válido si >= k_min)
##   k_inv: kWh implícito en el gasto (inversión exacta de la tarifa)
energia <- energia %>%
  mutate(k_rep = if_else(!is.na(celq) & round_st(celq) >= k_min,
                         round_st(celq), NA_real_))
energia$p_fwd <- tar_pago(energia$k_rep, P)
energia$v_k   <- !is.na(energia$p_fwd)
energia$v_p   <- !is.na(energia$cele_n) & energia$ok_par &
  !is.na(energia$p_min) & energia$cele_n >= energia$p_min * (1 - tol_cons)
inv <- tar_inv(energia$cele_n, P)
energia$k_inv <- inv$k
energia$e_inv <- inv$e
rm(inv)

energia <- energia %>%
  mutate(
    gap = if_else(v_k & v_p, abs(cele_n - p_fwd) / p_fwd, NA_real_),
    clase = case_when(
      !ok_par                              ~ 0L,  # sin período: no se toca
      v_k &  v_p & gap <= tol_cons         ~ 1L,  # consistentes
      v_k &  v_p & gap >  tol_cons         ~ 2L,  # se contradicen
      v_k & !v_p                           ~ 3L,  # solo kWh
      !v_k & v_p                           ~ 4L,  # solo gasto
      !v_k & !v_p & inf_elect == 1         ~ 5L,  # sin dato, con electricidad
      !v_k & !v_p & inf_elect == 0         ~ 6L   # sin electricidad
    ),
    ## Clase 7: usa electricidad pero no reporta pago (sin factura propia).
    ## No tiene contrato en los registros: fuera de la calibración y sin subsidio.
    sin_factura = le_st(cele_o1, 5) & eqv(p01a05c, 1) & eqv(p01d24, 1),
    clase = if_else(sin_factura & ok_par, 7L, clase)
  )

CLASES <- c("0" = "Sin período (sin corregir)", "1" = "kWh y gasto consistentes",
            "2" = "kWh y gasto se contradicen", "3" = "Solo kWh",
            "4" = "Solo gasto", "5" = "Sin dato (se sortea del posterior)",
            "6" = "Sin electricidad", "7" = "Usa electricidad sin factura propia")
tab_w(CLASES[as.character(energia$clase)], titulo = "Clase (hogares)")
tab_w(CLASES[as.character(energia$clase)], energia$factor,
      titulo = "Clase (ponderado)")

## 1.6 Diagnóstico previo ------------------------------------------------------
if (OPC$run_diag == 1) {
  seccion("06fix - 1.6 Diagnóstico previo")
  k_raw <- ifelse(ge_st(energia$celq_o, k_min) & !is.na(energia$celq_o),
                  round_st(energia$celq_o), NA)
  m5 <- ifelse(!is.na(k_raw) & k_raw >= 70 & k_raw <= 110, k_raw %% 5 == 0, NA)
  tab_w(m5[!is.na(m5)], energia$factor[!is.na(m5)],
        titulo = "a) Heaping: múltiplos de 5 entre 70 y 110 (sin heaping ~20%)")

  p_raw <- tar_pago(k_raw, P)
  ratio <- ifelse(gt_st(energia$cele_o1, 5) & !is.na(energia$cele_o1) &
                    !is.na(p_raw), energia$cele_o1 / p_raw, NA)
  nr5   <- !is.na(k_raw) & k_raw %% 5 != 0
  g_red <- eqv(round_st(energia$cele_o1) %% 25, 0)
  kb    <- cut(k_raw, c(1, 31, 61, 76, 89, 101, 131, 301), right = FALSE)
  sel   <- eqv(energia$empresa_elec, 1) & nr5 & !g_red & !is.na(kb)
  cat("\nb) Razón gasto / factura teórica por tramo (EEGSA)\n")
  print(tapply(ratio[sel], kb[sel], quantile, c(.25, .5, .75), na.rm = TRUE))
  sel2 <- !is.na(k_raw) & k_raw >= 1 & k_raw <= 300
  print(tapply(ratio[sel2], energia$empresa_elec[sel2], quantile,
               c(.25, .5, .75), na.rm = TRUE))
  rm(k_raw, m5, p_raw, ratio, nr5, g_red, kb, sel, sel2)
}

## 1.7 Registros administrativos -----------------------------------------------
seccion("06fix - 1.7 Registros administrativos")

admk <- read_excel(OPC$adm_file)
if (!"kwh" %in% names(admk)) names(admk)[1] <- "kwh"
admk <- admk %>%
  mutate(across(c(EEGSA, DEOCSA, DEORSA),
                ~ as.numeric(gsub(",", "", as.character(.x))))) %>%
  filter(kwh >= 1, kwh <= K_MAX) %>%
  arrange(kwh)
stopifnot("La tabla de registros debe tener 300 filas (1-300 kWh)" =
            nrow(admk) == K_MAX && all(admk$kwh == 1:K_MAX))

## prior (curva por kWh) y objetivo (cuota por rango de 25 kWh), 3 x 300 / 3 x 12
PK <- t(sapply(c("EEGSA", "DEOCSA", "DEORSA"),
               function(v) admk[[v]] / sum(admk[[v]])))
bin_k <- ceiling((1:K_MAX) / bw)
for (e in 1:3) {
  cat(sprintf("  Registros, distribuidora %d: cuota <= 60 kWh = %5.1f%% | <= 88 kWh = %5.1f%%\n",
              e, 100 * sum(PK[e, 1:60]), 100 * sum(PK[e, 1:88])))
}

## 1.8 Calibración ------------------------------------------------------------
##   P(k | hogar) ∝ prior_e(k) x L_gasto(gasto | k) x L_kWh(kWh reportado | k)
##   El prior de cada distribuidora se ajusta (raking) hasta que la cuota
##   esperada por rango de 25 kWh coincida con los registros. Una distribuidora
##   a la vez, igual que ts_calib() en el .do.
seccion("06fix - 1.8 Calibración")

energia <- energia %>%
  mutate(
    alto    = (v_k & gt_st(k_rep, 300) & !is.na(k_rep)) |
              (!v_k & v_p & !is.na(k_inv) & k_inv > 300),
    en_ts   = ok_par & !alto & !clase %in% c(6, 7),
    redondo = v_p & eqv(round_st(cele_n) %% 25, 0)
  )

idx <- which(energia$en_ts)
n   <- length(idx)
Pc  <- lapply(P, function(v) v[idx])
emp <- energia$empresa_elec[idx]
fac <- energia$factor[idx]
stopifnot("Hogares calibrados con parámetros tarifarios faltantes" =
            !anyNA(unlist(Pc[setdiff(names(Pc), "ok")])),
          "Hogares calibrados sin distribuidora 1-3" = all(emp %in% 1:3))
cat("Hogares en la calibración:", n, "\n")

kv   <- 1:K_MAX
kmat <- matrix(kv, nrow = n, ncol = K_MAX, byrow = TRUE)

## Factura teórica de cada k para cada hogar (n x 300)
Bill <- matrix(tar_pago(kmat, Pc), n, K_MAX)

## Verosimilitud del gasto (log-normal alrededor de delta x factura teórica)
vp   <- energia$v_p[idx]
celn <- energia$cele_n[idx]
dlt  <- cal$delta[emp]
sp   <- ifelse(energia$redondo[idx], cal$sig_pr, cal$sig_p)
LP <- exp(-0.5 * (log(celn / (dlt * Bill)) / sp)^2)
LP[!vp, ] <- 1

## Verosimilitud del kWh reportado: ventana de redondeo según el dígito
## (kWh no múltiplo de 5: ventana 1, el término (1-m25)(1-m10)(1-m5) del .do)
vk   <- energia$v_k[idx]
krep <- energia$k_rep[idx]
krep0 <- ifelse(is.na(krep), 0, krep)
wh   <- ifelse(krep0 %% 25 == 0, cal$w25,
         ifelse(krep0 %% 10 == 0, cal$w10,
          ifelse(krep0 %% 5 == 0, cal$w5, 1)))
tau  <- pmax(2, cal$sig_k * krep0)
dd   <- pmax(abs(kmat - krep0) - wh, 0)
LK <- exp(-0.5 * (dd / tau)^2)
LK[!vk, ] <- 1

LIK <- (LP + cal$eps_v) * (LK + cal$eps_v)
rm(LP, LK, kmat, celn, dlt, sp, krep, krep0, wh, tau, dd)

B <- outer(bin_k, seq_len(nb_adm), "==") * 1      # 300 x nb: k pertenece al rango b

KD   <- matrix(NA_real_, n, M)
PSUB <- QK <- QE <- rep(NA_real_, n)
set.seed(OPC$semilla)

for (e in 1:3) {
  ie <- which(emp == e)
  ne <- length(ie)
  if (ne == 0) next
  f  <- fac[ie]; N <- sum(f)
  L  <- LIK[ie, , drop = FALSE]
  p0 <- PK[e, ]
  tgt   <- as.numeric(crossprod(B, p0))
  prior <- p0
  conv  <- FALSE
  for (it in seq_len(cal$n_iter)) {
    W  <- sweep(L, 2, prior, "*")
    Q  <- W / rowSums(W)
    sb <- as.numeric(crossprod(B, as.numeric(crossprod(Q, f)) / N))
    gap <- max(abs(sb - tgt))
    if (gap < cal$tol_rake) { conv <- TRUE; break }
    r     <- tgt / pmax(sb, 1e-300)
    prior <- prior * as.numeric(B %*% r)
    prior <- prior / sum(prior)
  }
  W  <- sweep(L, 2, prior, "*")
  Q  <- W / rowSums(W)
  sb <- as.numeric(crossprod(B, as.numeric(crossprod(Q, f)) / N))
  gap <- max(abs(sb - tgt))
  cat(sprintf("  Distribuidora %d: %d hogares, %d iteraciones, brecha final %9.2e %s\n",
              e, ne, min(it, cal$n_iter), gap,
              if (conv) "(converge)" else "(NO CONVERGE)"))

  ## posterior esperado (sin ruido de sorteo)
  PSUB[ie] <- rowSums(Q[, 1:k_ts, drop = FALSE])
  QK[ie]   <- as.numeric(Q %*% kv)
  QE[ie]   <- rowSums(Q * Bill[ie, , drop = FALSE])

  ## M sorteos desde el posterior (inversión de la acumulada)
  C <- Q
  for (j in 2:K_MAX) C[, j] <- C[, j - 1] + Q[, j]
  DR <- matrix(NA_real_, nb_adm, M)
  for (mm in seq_len(M)) {
    u  <- runif(ne)
    kd <- pmin(1 + rowSums(C < u), K_MAX)
    KD[ie, mm] <- kd
    DR[, mm] <- as.numeric(crossprod(B[pmin(kd, K_MAX), ], f)) / N
  }

  if (OPC$run_diag == 1) {
    VAL <- cbind(Registro = 100 * tgt, Esperada = 100 * sb,
                 Sorteos_prom = 100 * rowMeans(DR), Sorteo_1 = 100 * DR[, 1],
                 Factor_prior = as.numeric(crossprod(B, prior)) / tgt)
    rownames(VAL) <- sprintf("r%d_%d", bw * (seq_len(nb_adm) - 1) + 1,
                             bw * seq_len(nb_adm))
    cat(sprintf("\nDistribuidora %d: cuota (%%) por rango de %d kWh\n", e, bw))
    print(round(VAL, 2))
  }
  rm(L, W, Q, C, DR)
}
rm(LIK, Bill, B)

## 1.9 Resultado por imputación y validación -----------------------------------
seccion("06fix - 1.9 Imputaciones")

energia$p_sub    <- NA_real_
energia$celq_esp <- NA_real_
energia$cele_esp <- NA_real_
energia$p_sub[idx]    <- PSUB
energia$celq_esp[idx] <- QK
energia$cele_esp[idx] <- QE

s_av  <- energia$alto & energia$v_k
s_ai  <- energia$alto & !energia$v_k
s_c6  <- energia$clase == 6
s_c7k <- energia$clase == 7 & energia$v_k
for (m in seq_len(M)) {
  q <- rep(NA_real_, nrow(energia))
  q[idx]   <- KD[, m]
  q[s_av]  <- energia$k_rep[s_av]
  q[s_ai]  <- energia$k_inv[s_ai]
  q[s_c6]  <- 0
  q[s_c7k] <- energia$k_rep[s_c7k]                       # dato informativo
  g <- tar_pago(q, P)
  g[eqv(q, 0) | energia$clase == 7] <- 0                 # sin factura propia
  ## clase 0 (sin período): no se corrige
  c0 <- energia$clase == 0
  q[c0] <- energia$celq[c0]
  g[c0] <- energia$cele_n[c0]
  energia[[paste0("celq_", m)]] <- q
  energia[[paste0("cele_", m)]] <- g
}

## Hogares fuera de la calibración: valores deterministas
energia$p_sub[energia$alto | energia$clase %in% c(6, 7)] <- 0
miss <- is.na(energia$p_sub)                             # clase 0
energia$p_sub[miss] <- as.numeric(coalesce(energia$celq_1[miss] > 0 &
                                             energia$celq_1[miss] <= k_ts, FALSE))
energia$celq_esp <- coalesce(energia$celq_esp, energia$celq_1)
energia$cele_esp <- coalesce(energia$cele_esp, energia$cele_1)

## Imputación 1 como base (decisión: se simula solo con la imputación 1)
energia$celq   <- energia$celq_1
energia$cele_n <- energia$cele_1

if (OPC$run_diag == 1) {
  bin_1 <- ifelse(energia$en_ts & !is.na(energia$celq_1) &
                    energia$celq_1 >= 1 & energia$celq_1 <= 300,
                  ceiling(energia$celq_1 / bw), NA)
  cat("\nCuotas ENCOVI calibrado (imputación 1), % por rango y distribuidora:\n")
  sel <- !is.na(bin_1)
  tt <- tapply(energia$factor[sel], list(bin_1[sel], energia$empresa_elec[sel]), sum)
  tt[is.na(tt)] <- 0
  print(round(100 * sweep(tt, 2, colSums(tt), "/"), 1))
  for (e in 1:3) {
    s <- energia$en_ts & eqv(energia$empresa_elec, e)
    cat(sprintf("Cuota <= %d kWh, ENCOVI calibrado (esperada), distribuidora %d: %5.1f\n",
                k_ts, e, 100 * weighted.mean(energia$p_sub[s], energia$factor[s])))
  }
  s <- energia$en_ts & !is.na(energia$celq_1) & energia$celq_1 >= 70 &
    energia$celq_1 <= 110
  tab_w(energia$celq_1[s] %% 5 == 0, energia$factor[s],
        titulo = "Heaping residual (múltiplos de 5, 70-110 kWh)")
  s <- energia$en_ts & !is.na(energia$celq_1) & energia$celq_1 >= 80 &
    energia$celq_1 <= 100
  tab_w(energia$celq_1[s], energia$factor[s], titulo = "Pila en 88/89 (debe desaparecer)")
  rm(bin_1, sel, tt, s)
}

## 1.10 Limpieza y tarifa social -----------------------------------------------
energia <- energia %>%
  mutate(celq = round_st(celq),
         tarifa_social = as.integer(coalesce(celq > 0 & celq <= k_ts, FALSE) &
                                      clase != 7))
tab_w(energia$tarifa_social, energia$factor, titulo = "Tarifa social")
if (OPC$run_diag == 1) {
  for (e in 1:3) {
    s <- eqv(energia$empresa_elec, e) & le_st(energia$celq, 300) & energia$celq >= 1
    tab_w(cut(energia$celq[s], seq(1, 301, 25), right = FALSE),
          energia$factor[s], titulo = paste("Tramos de 25 kWh -", EMPRESAS[e]))
  }
}

energia_ajustado <- energia %>%
  transmute(hhid, cele = cele_n, celq, inf_elect, empresa_elec, item = 414,
            depto, clase, p_sub, tarifa_social,
            across(matches("^(celq|cele)_[0-9]+$")),
            celq_esp, cele_esp)

saveRDS(energia_ajustado, paste0(fpresim, "energia_ajustado.rds"))
message("energia_ajustado.rds: ", nrow(energia_ajustado), " hogares")

suppressWarnings(rm(
  energia, hogar, agregado, quint, admk, PK, KD, P, Pc, emp, fac, kv,
  idx, n, vp, vk, bin_k, it, gap, m, q, g, c0, e, ie, ne, f, N, L, p0, tgt,
  prior, conv, r, sb, mm, u, kd, VAL, PSUB, QK, QE, miss,
  n_sinper, out_6, s_av, s_ai, s_c6, s_c7k, energia_ajustado,
  cal, TAR, TS_EEGSA, TS_DEOCSA, TS_DEORSA, CLASES, tol_cons, k_min, bw, nb_adm,
  K_MAX, k_ts, M, mean_st, tar_params, tar_pago, tar_inv, tar_selftest))
