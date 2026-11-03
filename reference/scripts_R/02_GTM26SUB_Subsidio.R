## =============================================================================
## Banco Mundial / INDE
## Herramienta de Microsimulación de Subsidios a la Electricidad
## Country: Guatemala 2023
## 07_GTM26SUB_Subsidio.R   (versión R de 07_GTM26SUB_Subsidio.do)
## Dependencias: 00_GTM26SUB_Master.R
##
## Para cada esquema tarifario s (hoja "sim_sub") y distribuidora:
##   tramo k: (block[s,k], block[s,k+1]]   (último tramo abierto)
##   aporte  = max(0, min(plena, tope) - objetivo);  0 si objetivo vacío
##   pago_e  = kWh x (plena - aporte)            (Q/mes)
##   sub_ele = kWh x aporte x 12                 (Q/año, hogar)
##
## Columnas de la hoja sim_sub:
##   block1..    límite inferior de cada tramo
##   t<emp>1..   tarifa plena por tramo     (emp = eeg, deoc, deor)
##   to<emp>1..  tarifa objetivo por tramo
##   tt<emp>     tarifa tope del esquema
##   b<emp>1..   cargo fijo (no se usa en el cálculo; se reporta)
##
## Input:  fpresim energia_ajustado.rds; fparams hoja sim_sub
## Output: fdbmod GTM_subeleh.rds  (hogar: hhid, empresa_elec, inf_elect, celq,
##                                   sub_ele<s>_hh, pago_e_<s>)
##         fdbmod GTM_tarifas.rds  (tablas tarifarias para el reporte)
## =============================================================================

seccion("02 - Subsidio eléctrico por esquema tarifario")

# 0. Parámetros tarifarios -------------------------------------------------------

EMP_COD <- c("eeg", "deoc", "deor")          # orden = empresa_elec 1, 2, 3

sim_sub_esc <- sort(unique(c(0, escenarios$sim_sub)))

sim_sub <- read_excel(fparams, sheet = "sim_sub") %>%
  janitor::clean_names() %>%
  filter(!is.na(sim_sub)) %>%
  filter(activo == 1 | sim_sub == 0) %>%
  filter(sim_sub %in% sim_sub_esc) %>%
  arrange(sim_sub)

faltan_esq <- setdiff(sim_sub_esc, sim_sub$sim_sub)
if (length(faltan_esq)) stop("Esquemas usados en 'escenarios' sin fila activa en ",
                             "'sim_sub': ", paste(faltan_esq, collapse = ", "))

#' Extrae columnas <prefijo>1, <prefijo>2, ... como matriz (filas = esquema)
mat_tramos <- function(df, prefijo, nb = NULL) {
  cols <- grep(paste0("^", prefijo, "[0-9]+$"), names(df), value = TRUE)
  cols <- cols[order(as.integer(sub(prefijo, "", cols)))]
  if (!is.null(nb)) {
    cols <- paste0(prefijo, seq_len(nb))
    for (cc in setdiff(cols, names(df))) df[[cc]] <- NA_real_
  }
  m <- as.matrix(df[, cols, drop = FALSE])
  storage.mode(m) <- "double"
  rownames(m) <- df$sim_sub
  m
}

## Límites de tramo: se descartan columnas vacías en todos los esquemas
block <- mat_tramos(sim_sub, "block")
block <- block[, colSums(!is.na(block)) > 0, drop = FALSE]
nb    <- ncol(block)

tarifas <- list(block = block)
for (emp in EMP_COD) {
  tarifas[[paste0("tr", emp)]] <- mat_tramos(sim_sub, paste0("t",  emp), nb)
  tarifas[[paste0("to", emp)]] <- mat_tramos(sim_sub, paste0("to", emp), nb)
  tarifas[[paste0("bf", emp)]] <- mat_tramos(sim_sub, paste0("b",  emp), nb)
  tt <- sim_sub[[paste0("tt", emp)]]
  names(tt) <- sim_sub$sim_sub
  tarifas[[paste0("tt", emp)]] <- as.numeric(tt) %>% setNames(sim_sub$sim_sub)
  if (anyNA(tt)) stop("Tarifa tope tt", emp, " vacía o no numérica en los esquemas: ",
                      paste(sim_sub$sim_sub[is.na(tt)], collapse = ", "))
}
tarifas$tplena <- sim_sub %>%
  select(sim_sub, any_of(paste0("tplena", EMP_COD))) %>% as.data.frame()
tarifas$nombre <- setNames(
  if ("nom_subsidio" %in% names(sim_sub)) as.character(sim_sub$nom_subsidio)
  else paste("Esquema", sim_sub$sim_sub),
  sim_sub$sim_sub)

#' Tabla tarifaria de un esquema: una fila por distribuidora x tramo
tabla_tarifas <- function(s, tar = tarifas) {
  s  <- as.character(s)
  bl <- tar$block[s, ]
  map_dfr(seq_along(EMP_COD), function(i) {
    emp  <- EMP_COD[i]
    tope <- tar[[paste0("tt", emp)]][s]
    map_dfr(seq_len(length(bl)), function(k) {
      plena <- tar[[paste0("tr", emp)]][s, k]
      obj   <- tar[[paste0("to", emp)]][s, k]
      ## Stata: min() y max() ignoran missing
      lim   <- suppressWarnings(min(c(plena, tope), na.rm = TRUE))
      lim   <- if (is.finite(lim)) lim else NA_real_
      apo   <- if (is.na(obj) || is.na(lim)) 0 else max(0, lim - obj)
      tibble(esquema = as.integer(s), empresa_elec = i, distribuidora = EMPRESAS[i],
             tramo = k, desde = bl[k],
             hasta = if (k < length(bl)) bl[k + 1] else NA_real_,
             cargo_fijo = tar[[paste0("bf", emp)]][s, k],
             plena = plena, tope = tope, objetivo = obj,
             aporte = apo, paga = plena - apo)
    })
  }) %>% filter(!is.na(desde))
}

tabla_tar <- map_dfr(sim_sub$sim_sub, tabla_tarifas)
print(tabla_tar %>% select(esquema, distribuidora, tramo, desde, hasta,
                           plena, aporte, paga), n = Inf)

# 1. Datos ----------------------------------------------------------------------

elec <- readRDS(paste0(fpresim, "energia_ajustado.rds"))

# 2. Escenarios de subsidio eléctrico ------------------------------------------

sin_emp <- sum(!elec$empresa_elec %in% 1:3 & !is.na(elec$celq) & elec$celq > 0)
if (sin_emp > 0) message("Aviso: ", sin_emp,
                         " hogares con consumo y empresa_elec fuera de 1-3 (pago y subsidio = 0)")

for (s in sim_sub$sim_sub) {
  pago <- ifelse(is.na(elec$celq), NA_real_, 0)
  sub  <- ifelse(is.na(elec$celq), NA_real_, 0)
  ts   <- tabla_tar %>% filter(esquema == s)
  for (r in seq_len(nrow(ts))) {
    hi  <- if (is.na(ts$hasta[r])) Inf else ts$hasta[r]
    sel <- eqv(elec$empresa_elec, ts$empresa_elec[r]) & !is.na(elec$celq) &
      elec$celq > ts$desde[r] & elec$celq <= hi
    pago[sel] <- elec$celq[sel] * ts$paga[r]
    sub[sel]  <- elec$celq[sel] * ts$aporte[r] * 12
  }
  elec[[paste0("pago_e_", s)]]   <- pago
  elec[[paste0("pago_u_", s)]]   <- ifelse(!is.na(elec$celq) & elec$celq > 0,
                                           pago / elec$celq, NA_real_)
  elec[[paste0("sub_ele", s, "_hh")]] <- sub
}

## Control: resumen por esquema (sin ponderar, como el tabstat del .do)
elec %>%
  summarise(across(matches("^(pago_e_|sub_ele)"), list(media = ~ mean(.x, na.rm = TRUE),
                                                      suma  = ~ sum(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything()) %>% print(n = Inf)

## Subsidio por kWh y como % de la tarifa plena (EEGSA), por rango de consumo
if ("1" %in% as.character(sim_sub$sim_sub) && OPC$run_diag == 1) {
  tpl <- tarifas$tplena$tplenaeeg[tarifas$tplena$sim_sub == 0]
  elec %>%
    filter(sub_ele1_hh > 0) %>%
    mutate(rango_e = cut(celq, c(1, 25, 75, 100, 200, 300, 400, 500, 600, 700),
                         right = FALSE),
           sub_kwh = sub_ele1_hh / celq / 12,
           sub_pct = 100 * sub_kwh / tpl) %>%
    group_by(rango_e) %>%
    summarise(sub_ele1_hh = mean(sub_ele1_hh), sub_kwh = mean(sub_kwh),
              sub_pct = mean(sub_pct), .groups = "drop") %>%
    print()
}

# 3. Guardar --------------------------------------------------------------------

elec %>%
  select(hhid, empresa_elec, inf_elect, celq, matches("^sub_ele[0-9]+_hh$"),
         matches("^pago_e_[0-9]+$")) %>%
  saveRDS(paste0(fdbmod, "GTM_subeleh.rds"))

tarifas$tabla <- tabla_tar
saveRDS(tarifas, paste0(fdbmod, "GTM_tarifas.rds"))

suppressWarnings(rm(elec, sim_sub, block, nb, tarifas, tabla_tar, tt, emp, s, r,
                    ts, sel, hi, pago, sub, sin_emp, faltan_esq, tpl,
                    mat_tramos, tabla_tarifas))
