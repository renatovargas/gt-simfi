## =============================================================================
## Banco Mundial / INDE
## Herramienta de Microsimulación de Subsidios a la Electricidad
## Country: Guatemala 2023
## 06_GTM26SUB_BaseIngreso.R   (versión R de 06_GTM26SUB_BaseIngreso.do)
## Dependencias: 00_GTM26SUB_Master.R, x_GTM26SUB_funciones.R
##
## Contenido
##   1.1-1.2  Datos ENCOVI 2023: acceso y gasto en electricidad
##   1.3-1.4  Tarifas y funciones tar_pago / tar_inv / autoprueba
##   1.5      Clasificación de hogares (clases 0-7)
##   1.6      Diagnóstico previo (OPC$run_diag)
##   1.7      Registros administrativos (usuarios por kWh y distribuidora)
##   1.8      Calibración VECTORIZADA: posterior hogar x kWh (matriz n x 300)
##            y ajuste del prior por distribuidora contra los registros
##   1.9      M imputaciones y validación
##   1.10     Limpieza, guardar energia_ajustado.rds
##   2-3      Base de ingreso disponible y estratos -> disp_income.rds
##
## Diferencias deliberadas con el .do
##   - La malla hogar x kWh se trabaja con matrices (no con expand 300). El
##     prior se guarda como matriz 3 x 300 (distribuidora x kWh): en el .do el
##     ajuste es idéntico para todos los hogares de una distribuidora, así que
##     el resultado es el mismo con mucha menos memoria.
##   - Los sorteos usan el generador de R: la distribución es la misma, los
##     valores individuales no coinciden con Stata.
##   - El .do guarda energia_ajustado en ${presim} y la parte 2 la lee de
##     ${tmp}; aquí se usa fpresim en ambos pasos.
##
## Input:  fencovi 2_encovi_2023_hogares, 3_encovi_2023_c01se_fuentes,
##         35_encovi_2023_agregado_consumo; OPC$adm_file; finput DispIncome
## Output: fpresim energia_ajustado.rds, disp_income.rds
## =============================================================================

# 0. Parámetros de la sección ---------------------------------------------------

tol_cons <- 0.15   # kWh y gasto "consistentes" si difieren <= 15 %
k_min    <- 2      # kWh reportado < k_min = "sin dato"
bw       <- 25     # ancho del rango objetivo (kWh)
nb_adm   <- 300 / bw
K_MAX    <- 300

## Verosimilitudes y raking (igual que el .do)
cal <- list(
  sig_p  = 0.25, sig_pr = 0.35,           # gasto no redondo / redondo (Q25)
  delta  = c(1, 1, 1),                    # factor de nivel por distribuidora
  w25 = 10, w10 = 5, w5 = 2,              # ventanas de redondeo del kWh
  sig_k  = 0.50, eps_v = 0.02,
  n_iter = 25
)
M <- OPC$M

mean_st <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

# 1. Base de consumo de energía eléctrica ENCOVI 2023 ---------------------------

## 1.1 Acceso a energía eléctrica ----------------------------------------------
seccion("06 - 1.1 Datos ENCOVI")

hogar <- leer_base(paste0(fencovi, "2_encovi_2023_hogares")) %>%
  select(no_hogar, p01a05c, p01d24:p01d29, total_pers)

## 1.2 Gastos en energía --------------------------------------------------------
fuentes <- as.data.table(leer_base(paste0(fencovi, "3_encovi_2023_c01se_fuentes")))
fuentes[, item := id_fuentes + 410]
fuentes[, `:=`(cantidad = p01e08, conspago = p01e09 * 12)]

## collapse (sum) -> suma con missing = 0; (mean) -> media sin missing
energia <- fuentes[, .(
  cantidad = sum(cantidad, na.rm = TRUE),
  conspago = sum(conspago, na.rm = TRUE),
  area   = mean_st(area),   factor = mean_st(factor),
  depto  = mean_st(depto),  pobre  = mean_st(pobre),
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
##   pago = (base + tarifa_tramo * kWh) * (1.12 + apc)  si apc < 1
##   pago = (base + tarifa_tramo * kWh) * 1.12 + apc     si apc > 1
##   tramo 1: 1-60 kWh, tramo 2: 61-88 kWh, tramo 3 (TN): 89+ kWh

TAR <- list(
  ## Alumbrado público por departamento (apc<1: proporción; apc>1: Q fijos)
  apc = c(0.13, 26.40, 0.11, 26.00, 0.15, 34.00, 19.80, 17.50, 35.40, 36.30,
          35.90, 26.50, 20.50, 18.20, 27.50, 26.50, 39.80, 36.60, 23.80, 12.60,
          27.20, 23.00),
  lim_ts = rbind(c(60, 88), c(60, 88)),                      # período x tramo
  base   = rbind(c(10.985336, 10.985335),                    # empresa x período
                 c(22.842086, 23.27439),
                 c(22.551741, 22.964455)),
  tn     = rbind(c(1.386014, 1.386014),
                 c(2.196611, 2.196611),
                 c(2.103611, 2.103611)),
  ts     = rbind(c(0.50, 0.9228), c(0.50, 0.9856))           # período x tramo
)

## 1.4 Funciones: parámetros por hogar, factura teórica e inversa ---------------

#' tar_params: parámetros tarifarios de cada hogar (depto/empresa/período)
tar_params <- function(depto, empresa, periodo) {
  ok <- !is.na(depto) & !is.na(empresa) & !is.na(periodo)
  e <- ifelse(ok, empresa, NA); p <- ifelse(ok, periodo, NA)
  d <- ifelse(ok, depto, NA)
  list(
    ok   = ok,
    base = round_st(TAR$base[cbind(e, p)], 0.01),
    apc  = TAR$apc[d],
    r1   = TAR$ts[cbind(p, 1)],
    r2   = TAR$ts[cbind(p, 2)],
    r3   = TAR$tn[cbind(e, p)],
    l1   = TAR$lim_ts[cbind(p, 1)],
    l2   = TAR$lim_ts[cbind(p, 2)]
  )
}

#' tar_pago: factura mensual teórica (Q) para un consumo entero k
#' k puede ser vector (largo n) o matriz n x K (los parámetros se reciclan por fila)
tar_pago <- function(k, P) {
  nn <- length(k)
  r  <- function(v) rep_len(v, nn)        # reciclaje por fila (column-major)
  base <- r(P$base); apc <- r(P$apc)
  rate <- ifelse(k <= r(P$l1), r(P$r1), ifelse(k <= r(P$l2), r(P$r2), r(P$r3)))
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
  k1 <- pmin(pmax(round_st((inner - P$base) / P$r1), 1), P$l1)
  k2 <- pmin(pmax(round_st((inner - P$base) / P$r2), P$l1 + 1), P$l2)
  k3 <- pmax(round_st((inner - P$base) / P$r3), P$l2 + 1)
  e1 <- abs(tar_pago(k1, P) - p)
  e2 <- abs(tar_pago(k2, P) - p)
  e3 <- abs(tar_pago(k3, P) - p)
  kout <- k1; eout <- e1
  s2 <- !is.na(e2) & !is.na(eout) & e2 < eout
  kout[s2] <- k2[s2]; eout <- pmin(eout, e2)
  s3 <- !is.na(e3) & !is.na(eout) & e3 < eout
  kout[s3] <- k3[s3]; eout <- pmin(eout, e3)
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
seccion("06 - 1.5 Clasificación de hogares")

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
  seccion("06 - 1.6 Diagnóstico previo")
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
seccion("06 - 1.7 Registros administrativos")

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
BS    <- t(apply(PK, 1, function(p) tapply(p, bin_k, sum)))   # 3 x nb_adm
for (e in 1:3) {
  cat(sprintf("  Registros, distribuidora %d: cuota <= 60 kWh = %5.1f%% | <= 88 kWh = %5.1f%%\n",
              e, 100 * sum(PK[e, 1:60]), 100 * sum(PK[e, 1:88])))
}

## 1.8 Calibración vectorizada ------------------------------------------------
##   P(k | hogar) ∝ prior_e(k) x L_gasto(gasto | k) x L_kWh(kWh reportado | k)
##   El prior de cada distribuidora se ajusta (raking) hasta que la cuota
##   esperada por rango de 25 kWh coincida con los registros.
seccion("06 - 1.8 Calibración")

energia <- energia %>%
  mutate(
    alto    = (v_k & gt_st(k_rep, 300) & !is.na(k_rep)) |
              (!v_k & v_p & !is.na(k_inv) & k_inv > 300),
    en_ts   = ok_par & !alto & !clase %in% c(6, 7),
    redondo = v_p & eqv(round_st(cele_n) %% 25, 0)
  )

idx <- which(energia$en_ts)
n   <- length(idx)
Pc  <- lapply(P, function(v) v[idx])          # parámetros del universo
emp <- energia$empresa_elec[idx]
fac <- energia$factor[idx]
Ne  <- tapply(fac, factor(emp, levels = 1:3), sum)
cat("Hogares en la calibración:", n, "\n")

kmat <- matrix(1:K_MAX, nrow = n, ncol = K_MAX, byrow = TRUE)

## Factura teórica de cada k para cada hogar (n x 300)
Pk <- tar_pago(kmat, Pc)
Pk <- matrix(Pk, n, K_MAX)

## Verosimilitud del gasto (log-normal alrededor de delta x factura teórica)
vp   <- energia$v_p[idx]
celn <- energia$cele_n[idx]
dlt  <- cal$delta[emp]
sp   <- ifelse(energia$redondo[idx], cal$sig_pr, cal$sig_p)
LP <- exp(-0.5 * (log(celn / (dlt * Pk)) / sp)^2)
LP[!vp, ] <- 1

## Verosimilitud del kWh reportado: ventana de redondeo según el dígito
vk   <- energia$v_k[idx]
krep <- energia$k_rep[idx]
wh   <- ifelse(krep %% 25 == 0, cal$w25,
         ifelse(krep %% 10 == 0, cal$w10,
          ifelse(krep %% 5 == 0, cal$w5, 1)))
tau  <- pmax(2, cal$sig_k * krep)
LK <- exp(-0.5 * (pmax(abs(kmat - krep) - wh, 0) / tau)^2)
LK[!vk, ] <- 1

LIK <- (LP + cal$eps_v) * (LK + cal$eps_v)
rm(LP, LK, Pk, kmat, celn, dlt, sp, krep, wh, tau)

## Matriz indicadora kWh -> rango (300 x nb_adm)
B <- outer(bin_k, seq_len(nb_adm), "==") * 1
emp_f <- factor(emp, levels = 1:3)
presentes <- which(Ne > 0)

posterior <- function(PK) {
  W <- PK[emp, , drop = FALSE] * LIK
  W / rowSums(W)
}

for (it in seq_len(cal$n_iter)) {
  PW <- posterior(PK)
  S  <- rowsum(PW * fac, emp_f, reorder = TRUE)      # 3 x 300 (filas presentes)
  sb <- matrix(NA_real_, 3, nb_adm)
  sb[as.integer(rownames(S)), ] <- (S %*% B) / as.numeric(Ne[rownames(S)])
  gap <- max(abs(sb[presentes, ] - BS[presentes, ]), na.rm = TRUE)
  ratio <- ifelse(!is.na(sb) & sb > 0, BS / sb, 1)
  PK <- PK * ratio[, bin_k]
  cat(sprintf("iteración %2d: brecha máxima entre cuota esperada y registros = %7.5f\n",
              it, gap))
}

## Posterior final, probabilidad de tarifa social y M sorteos
PW <- posterior(PK)
CW <- PW
for (j in 2:K_MAX) CW[, j] <- CW[, j - 1] + PW[, j]
CW[, K_MAX] <- 1
p_sub <- rowSums(PW[, 1:88, drop = FALSE])

set.seed(OPC$semilla)
KD <- sapply(seq_len(M), function(m) {
  u <- runif(n)
  1 + rowSums(CW < u)
})
KD <- matrix(KD, n, M)
rm(PW, CW, LIK, B, S, sb, ratio)

## 1.9 Resultado por imputación y validación -----------------------------------
seccion("06 - 1.9 Imputaciones")

energia$p_sub <- NA_real_
energia$p_sub[idx] <- p_sub

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
energia$p_sub[energia$alto | energia$clase %in% c(6, 7)] <- 0

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
  cat("\nCuotas de los registros (%), por rango de", bw, "kWh:\n")
  print(round(100 * t(BS), 1))
  for (e in 1:3) {
    s <- energia$en_ts & eqv(energia$empresa_elec, e)
    cat(sprintf("Cuota <= 88 kWh, ENCOVI calibrado (esperada), distribuidora %d: %5.1f\n",
                e, 100 * weighted.mean(energia$p_sub[s], energia$factor[s])))
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
         tarifa_social = as.integer(lt_st(celq, 89) & celq > 0 & clase != 7))
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
            across(matches("^(celq|cele)_[0-9]+$")))

saveRDS(energia_ajustado, paste0(fpresim, "energia_ajustado.rds"))
message("energia_ajustado.rds: ", nrow(energia_ajustado), " hogares")

suppressWarnings(rm(
  energia, hogar, agregado, quint, admk, PK, BS, KD, P, Pc, emp, emp_f, fac,
  Ne, idx, n, vp, vk, p_sub, bin_k, presentes, it, gap, m, q, g, c0, e,
  n_sinper, out_6, s_av, s_ai, s_c6, s_c7k))

# 2. Base de ingresos y características ----------------------------------------
seccion("06 - 2. Ingreso disponible")

## 2.1 Archivo de ingreso disponible -------------------------------------------
dispinc <- leer_base(paste0(finput, "DispIncome"))

## 2.2 Variables a incluir (equivalente a los keep con comodines del .do) -------
vars_fijas <- c(
  "hhid", "depto", "dominio", "area", "upm", "factor", "pobreza", "urban",
  "miembros", "personas", "adu_hh", "chi_hh", "hsize", "sjefe", "etjefe",
  "urjefe", "edpadre", "edmadre", "yd_pc",
  "cp", "gender", "age", "relation", "edad", "grupos_edad", "pet_ine",
  "pea_ine", "ocupados", "subvisibles", "inactivos", "hhin", "indigena",
  "etnorace", "hhead", "level_school", "nivedu", "type_school", "pet", "petj",
  "po", "rama", "ciiu", "pd", "pea", "pei", "calif", "calte", "igssaf",
  "contribut", "ssjefe", "wcontract", "ciiu_2", "inla", "ii", "ila", "hacina",
  "nbi_agua", "sal_group", "p06a02",
  "salario_a", "hextras_a", "comision_a", "aguine_a1", "bon14e_a1",
  "diferido_a", "bonovac_a", "trabvaca_a", "produc_a", "salario2_a",
  "bonife2_a", "aguine_a2", "bon14e_a2", "bono14_a", "bono142_a",
  "aguinaldo_a", "aguinaldo2_a", "venag", "venna", "gan_1", "ganag1_a",
  "ganna1_a", "gagricola", "ganag2_a", "ganna2_a", "gan_2",
  "ym_pc", "yp_pc", "yn_pc", "agreg3", "indice", "otroele"
)
patrones <- c("^inf_", "^pob", "^chi", "^old", "^n_worker", "^relab",
              "^informal$", "^formal", "^employ_status$", "^my_.*_in$",
              "^dtr_.*_.*", "^edu_.*_ri$", "^ded_", "^pline_")

faltan <- setdiff(setdiff(vars_fijas, "otroele"), names(dispinc))
if (length(faltan)) message("Aviso: variables no encontradas en DispIncome: ",
                            paste(faltan, collapse = ", "))
imprescindibles <- c("hhid", "hhin", "depto", "factor", "yd_pc", "relation",
                     "hsize", "indice", "pline_mod", "pline_ext")
stopifnot("Faltan variables imprescindibles en DispIncome" =
            all(imprescindibles %in% names(dispinc)))

dispinc <- dispinc %>%
  select(any_of(vars_fijas), matches(paste(patrones, collapse = "|")))

## 2.3 Merge con electricidad ---------------------------------------------------
dispinc <- dispinc %>%
  stata_merge(energia_ajustado, by = "hhid",
              keepusing = c("celq", "cele", "inf_elect")) %>%
  mutate(celq = if_else(eqv(relation, 1), celq, NA_real_),
         cele = if_else(eqv(relation, 1), cele, NA_real_)) %>%
  filter(!is.na(hhin))

## 2.4 Distribuidora (aprox. por depto) ----------------------------------------
## PLACEHOLDER: cooperativas Ixcán, Joyabaj y Retalhuleu no identificadas.
dispinc <- dispinc %>%
  mutate(empresa_elec  = empresa_de_depto(depto),
         region        = region_de_depto(depto),
         coop_excluida = 0L)

## 2.5 Ajuste en términos reales (varlist y*pc del .do) -------------------------
dispinc <- dispinc %>%
  mutate(across(matches("^y.*pc$"), ~ .x / indice)) %>%
  group_by(hhid) %>%
  mutate(yd_hh = sum(yd_pc, na.rm = TRUE)) %>%
  ungroup()

# 3. Estratos y deciles (línea base) --------------------------------------------
dispinc <- dispinc %>%
  mutate(gr_estrato = estrato4(yd_pc),
         decyd      = statar::xtile(yd_pc, n = 10, wt = factor))

tab_w(ESTRATOS[as.character(dispinc$gr_estrato)], dispinc$factor,
      titulo = "Estratos (línea base)")
cat(sprintf("Pobreza (yd_pc < pline_mod): %.2f%% | extrema: %.2f%%\n",
            fgt(dispinc$yd_pc, dispinc$pline_mod, dispinc$factor),
            fgt(dispinc$yd_pc, dispinc$pline_ext, dispinc$factor)))

saveRDS(dispinc, paste0(fpresim, "disp_income.rds"))

suppressWarnings(rm(
  dispinc, energia_ajustado, vars_fijas, patrones, faltan, imprescindibles,
  cal, TAR, CLASES, tol_cons, k_min, bw, nb_adm, K_MAX, M, mean_st,
  tar_params, tar_pago, tar_inv, tar_selftest, posterior))
