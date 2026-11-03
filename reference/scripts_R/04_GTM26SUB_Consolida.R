## =============================================================================
## Banco Mundial / INDE
## Herramienta de Microsimulación de Subsidios a la Electricidad
## Country: Guatemala 2023
## 04_GTM26SUB_Consolida.R
##   (versión R de 04_GTM26SUB_Consolida.do + cálculo de indicadores al estilo
##    de 05_DOM26_ConsolidaEscenarios.R)
## Dependencias: 00_GTM26SUB_Master.R
##
## Conceptos de ingreso (per cápita, Q anuales reales):
##   yd_pc      ingreso disponible observado (base)
##   ys<e>_pc   yd_pc + (subsidio escenario e - subsidio vigente)   [ys0 = yd]
##   yt<e>_pc   ys<e>_pc + compensación del escenario e
##
## Estratos (4): 1 Pobre extremo, 2 Pobre, 3 Vulnerable, 4 Clase media o más.
##   gr_estrato fijo sobre yd; gr_estrato<e> sobre ys<e>; gr_estratot<e> sobre yt<e>.
## Deciles fijos sobre yd_pc.
##
## Input:  fpresim disp_income.rds; fdbmod GTM_subeleh.rds, GTM_simcomp.rds,
##         GTM_tarifas.rds
## Output: fdbmod GTM_simulaciones_subsidio.rds  (microdatos)
##         fdbmod GTM_resumen.rds                (lista de resultados por escenario)
## =============================================================================

seccion("04 - Consolidación")

# 0. Parámetros ------------------------------------------------------------------

esc_all     <- escenarios$escenario                  # incluye 0
esc_reforma <- setdiff(esc_all, 0)
if (!length(esc_reforma)) stop("No hay escenarios de reforma activos (escenario != 0)")

subs <- setNames(escenarios$sim_sub, escenarios$escenario)
scom <- setNames(coalesce(escenarios$sim_com, 0), escenarios$escenario)
if (subs["0"] != 0) warning("El escenario 0 usa el esquema ", subs["0"],
                            "; el subsidio base se toma siempre del esquema 0")

#' Valor de global_params por coincidencia en la descripción (acepta "=997.68")
param_glob <- function(patron) {
  v <- parm_glob$valor[str_detect(parm_glob$descripcion, regex(patron, ignore_case = TRUE))]
  if (!length(v)) return(NA_real_)
  v <- v[1]
  if (is.character(v)) v <- suppressWarnings(as.numeric(sub("^=", "", v)))
  as.numeric(v)
}
macro <- list(
  pib      = param_glob("^PIB"),                    # millones de Q
  sub_inde = param_glob("Subsidio Aporte")          # millones de Q (verificar)
)
if (is.na(macro$pib)) warning("PIB no definido en global_params: % del PIB = NA")
if (is.na(macro$sub_inde)) message("Aviso: 'Subsidio Aporte Tarifa Social' vacío ",
                                   "(si la celda es fórmula, guarde el Excel desde Excel)")

tol   <- OPC$tol_recibe
umbr  <- umbrales_estrato()
tarifas <- readRDS(paste0(fdbmod, "GTM_tarifas.rds"))

# 1. Unir ingreso, subsidios y compensaciones -------------------------------------

base <- readRDS(paste0(fpresim, "disp_income.rds"))

## 1.1 Subsidios (esquemas 0..K, hogar) -> per cápita, ssub_ele<s>_pc
subeleh <- readRDS(paste0(fdbmod, "GTM_subeleh.rds")) %>%
  select(hhid, matches("^sub_ele[0-9]+_hh$"))
base <- stata_merge(base, subeleh, by = "hhid", keep = c("master", "match"))

esq <- as.integer(str_match(grep("^sub_ele[0-9]+_hh$", names(base), value = TRUE),
                            "^sub_ele([0-9]+)_hh$")[, 2])
for (s in esq) {
  hh <- base[[paste0("sub_ele", s, "_hh")]]
  base[[paste0("ssub_ele", s, "_pc")]] <- coalesce(hh / base$hsize, 0)
  base[[paste0("sub_ele", s, "_hh")]]  <- NULL
}
stopifnot("Falta el esquema 0 en GTM_subeleh" = 0 %in% esq)
base$sub_ele0_pc <- base$ssub_ele0_pc

## 1.2 Compensaciones (individuo)
simcomp <- readRDS(paste0(fdbmod, "GTM_simcomp.rds")) %>%
  select(hhin, matches("^comp_[0-9]+$"))
base <- stata_merge(base, simcomp, by = "hhin", keep = c("master", "match"),
                    tipo = "1:1")

for (e in esc_all) {
  k  <- scom[as.character(e)]
  if (e == 0 || k == 0) {
    cin <- rep(0, nrow(base))
  } else {
    col <- paste0("comp_", k)
    if (!col %in% names(base)) stop("Escenario ", e, ": no existe ", col,
                                    " en GTM_simcomp.rds")
    cin <- coalesce(base[[col]], 0)
  }
  base[[paste0("comp", e, "_pc")]] <- ave(cin, base$hhid, FUN = sum) / base$hsize
}
base <- base %>% select(-matches("^comp_[0-9]+$"))

# 2. Ingreso con subsidio y con subsidio + compensación --------------------------

base <- base %>%
  mutate(decyd      = statar::xtile(yd_pc, n = 10, wt = factor),
         gr_estrato = estrato4(yd_pc, umbr))

for (e in esc_all) {
  if (e != 0) {
    s   <- subs[as.character(e)]
    src <- if (s == 0) "sub_ele0_pc" else paste0("ssub_ele", s, "_pc")
    if (!src %in% names(base)) stop("Escenario ", e, ": no existe ", src,
                                    " (esquema ", s, ") en GTM_subeleh")
    base[[paste0("sub_ele", e, "_pc")]] <- base[[src]]
  }
  ys <- base$yd_pc + (base[[paste0("sub_ele", e, "_pc")]] - base$sub_ele0_pc)
  base[[paste0("ys", e, "_pc")]]        <- ys
  base[[paste0("yt", e, "_pc")]]        <- ys + base[[paste0("comp", e, "_pc")]]
  base[[paste0("gr_estrato", e)]]       <- estrato4(ys, umbr)
  base[[paste0("gr_estratot", e)]]      <- estrato4(base[[paste0("yt", e, "_pc")]], umbr)
  base[[paste0("sub_ele", e, "_hh")]]   <- ave(base[[paste0("sub_ele", e, "_pc")]],
                                               base$hhid, FUN = sum)
}

## 2.1 Variables de agrupación y auxiliares ---------------------------------------
edad_v <- if ("age" %in% names(base)) base$age else base$edad
base <- base %>%
  mutate(.edad = edad_v) %>%
  group_by(hhid) %>%
  mutate(chihh  = as.integer(any(.edad < 15, na.rm = TRUE)),
         oldhh  = as.integer(any(.edad > 65, na.rm = TRUE)),
         hh_tag = as.integer(row_number() == 1)) %>%
  ungroup() %>%
  mutate(cathhd = case_when(chihh == 0 & oldhh == 0 ~ 1L,
                            chihh == 1 & oldhh == 0 ~ 2L,
                            chihh == 0 & oldhh == 1 ~ 3L,
                            TRUE                    ~ 4L),
         region    = region_de_depto(depto),
         pline_int = 365 * OPC$pline_int * OPC$ppp_fac) %>%
  select(-.edad)

if ("urban" %in% names(base) && !all(is.na(base$urban))) {
  base$urb <- as.integer(base$urban == 1)
} else {
  base$urb <- as.integer(eqv(base$area, 1))       # ENCOVI: 1 urbano, 2 rural
}
if (!"sjefe" %in% names(base) || all(is.na(base$sjefe))) {
  gj <- base %>% filter(eqv(relation, 1)) %>% distinct(hhid, .keep_all = TRUE) %>%
    select(hhid, sjefe = gender)
  base <- base %>% select(-any_of("sjefe")) %>% left_join(gj, by = "hhid")
}

## 2.2 Chequeos ------------------------------------------------------------------
stopifnot(
  "Faltan valores en yd_pc, factor, líneas o deciles" =
    !anyNA(base[c("yd_pc", "factor", "pline_mod", "pline_ext", "decyd", "gr_estrato")]),
  "El ponderador tiene valores <= 0" = all(base$factor > 0)
)
if (length(unique(base$decyd)) != 10) warning("decyd no tiene 10 categorías")
for (e in esc_all) {
  d <- abs(base[[paste0("ys", e, "_pc")]] -
             (base$yd_pc + base[[paste0("sub_ele", e, "_pc")]] - base$sub_ele0_pc))
  stopifnot(max(d) < 0.01)
}

## 2.3 Costo fiscal rápido (consola) ---------------------------------------------
cat(strrep("-", 60), "\nCosto fiscal anual estimado por escenario (millones de Q)\n",
    strrep("-", 60), "\n", sep = "")
for (e in esc_all) {
  cat(sprintf("  Escenario %d: subsidio %12.1f   compensación %12.1f\n", e,
              msum(base[[paste0("sub_ele", e, "_pc")]], base$factor),
              msum(base[[paste0("comp", e, "_pc")]], base$factor)))
}

saveRDS(base, paste0(fdbmod, "GTM_simulaciones_subsidio.rds"))

# 3. Indicadores por escenario ---------------------------------------------------

w   <- base$factor
tag <- base$hh_tag
yd  <- base$yd_pc
PLS <- list(pov_gral = base$pline_mod, pov_ext = base$pline_ext,
            pov_int  = base$pline_int)

## 3.1 Pobreza -------------------------------------------------------------------
#' Columna de indicadores de pobreza para un ingreso y (línea z)
pov_col <- function(y, z, y0) {
  p   <- y < z
  g   <- ifelse(p, z - y, 0)
  H   <- sum(w * tag)
  pb  <- y0 < z                          # pobres en la base
  np  <- (y0 >= z) & p                   # nuevos pobres
  Hp  <- sum(w * tag * pb)
  Hnp <- sum(w * tag * np)
  c(rate      = fgt(y, z, w, 0),
    headcount = sum(w * p),
    fgt1      = fgt(y, z, w, 1),
    fgt2      = fgt(y, z, w, 2),
    br_hh     = sum(w * g) / H / 12,
    br_pob    = if (Hp  > 0) sum(w * g * pb) / Hp  / 12 else NA_real_,
    br_npob   = if (Hnp > 0) sum(w * g * np) / Hnp / 12 else 0,
    br_npib   = if (!is.na(macro$pib)) 100 * sum(w * g * np) / (macro$pib * 1e6) else NA_real_)
}

FILAS_POV <- c("Pobreza (% personas)", "Cambio vs. base (p.p.)",
               "Nuevos pobres vs. base (personas)", "Brecha de pobreza FGT1 (%)",
               "Severidad FGT2 (%)", "Brecha mensual por hogar, todos (Q)",
               "Brecha mensual por hogar, pobres en la base (Q)",
               "Brecha mensual por hogar, nuevos pobres (Q)",
               "Brecha de nuevos pobres (% PIB)")

pov_matriz <- function(z, e) {
  Y <- list(Base = base$ys0_pc,
            ys   = base[[paste0("ys", e, "_pc")]],
            yt   = base[[paste0("yt", e, "_pc")]])
  M <- sapply(Y, pov_col, z = z, y0 = base$ys0_pc)
  out <- rbind(M["rate", ], M["rate", ] - M["rate", 1],
               M["headcount", ] - M["headcount", 1],
               M["fgt1", ], M["fgt2", ], M["br_hh", ], M["br_pob", ],
               M["br_npob", ], M["br_npib", ])
  dimnames(out) <- list(FILAS_POV, c("Base", paste0("ys", e), paste0("yt", e)))
  out
}

## 3.2 Desigualdad -----------------------------------------------------------------
ineq_matriz <- function(e) {
  M <- sapply(list(base$ys0_pc, base[[paste0("ys", e, "_pc")]],
                   base[[paste0("yt", e, "_pc")]]), ineq3, w = w)
  out <- rbind(M["Gini", ], M["Gini", ] - M["Gini", 1], M["Palma", ], M["Theil", ])
  dimnames(out) <- list(c("Gini", "Cambio en Gini vs. base", "Palma", "Theil T"),
                        c("Base", paste0("ys", e), paste0("yt", e)))
  out
}

## 3.3 Progresividad (concentración y Kakwani; transferencias: Gini - IC) ----------
gini_ref <- ineq3(yd, w)[["Gini"]]
kakwani_matriz <- function(e) {
  sub0 <- base$sub_ele0_pc
  sube <- base[[paste0("sub_ele", e, "_pc")]]
  come <- base[[paste0("comp", e, "_pc")]]
  ic <- function(v) if (sum(v * w) == 0) NA_real_ else conc_index(v, yd, w)
  b  <- c(ic(sub0), NA_real_, ic(sub0))
  r  <- c(ic(sube), ic(come), ic(sube + come))
  out <- cbind(b, gini_ref - b, r, gini_ref - r)
  dimnames(out) <- list(c("Subsidio eléctrico", "Compensación",
                          "Subsidio + compensación"),
                        c("Base concentración", "Base Kakwani",
                          "Escenario concentración", "Escenario Kakwani"))
  out
}

## 3.4 Tablas por grupo --------------------------------------------------------------
GRUPOS <- list(
  dec  = list(var = "decyd",      titulo = "Decil de ingreso disponible",
              etiquetas = setNames(paste("Decil", 1:10), 1:10)),
  estr = list(var = "gr_estrato", titulo = "Estrato de ingreso (base)",
              etiquetas = ESTRATOS),
  urb  = list(var = "urb",        titulo = "Área de residencia",
              etiquetas = c("0" = "Rural", "1" = "Urbano")),
  sex  = list(var = "sjefe",      titulo = "Sexo del jefe de hogar",
              etiquetas = c("1" = "Hombre", "2" = "Mujer")),
  cat  = list(var = "cathhd",     titulo = "Tipo de hogar",
              etiquetas = c("1" = "Sin niños ni adultos mayores",
                            "2" = "Con niños", "3" = "Con adultos mayores",
                            "4" = "Con niños y adultos mayores")),
  reg  = list(var = "region",     titulo = "Región",
              etiquetas = REGIONES)
)
for (g in names(GRUPOS)) {
  v <- base[[GRUPOS[[g]]$var]]
  extra <- setdiff(as.character(sort(unique(v))), names(GRUPOS[[g]]$etiquetas))
  if (length(extra)) {
    message("Aviso: ", GRUPOS[[g]]$var, " tiene valores sin etiqueta: ",
            paste(extra, collapse = ", "))
    GRUPOS[[g]]$etiquetas <- c(GRUPOS[[g]]$etiquetas, setNames(extra, extra))
  }
}

COLS_GRUPO <- c(sub_base = "Subsidio base", sub_esc = "Subsidio escenario",
                comp = "Compensación", sub_comp = "Subsidio + compensación",
                d_sub = "▲ Subsidio", neto = "Efecto neto (▲ subsidio + compensación)")
COLS_COB <- c(rec_base = "Recibe subsidio, base", rec_esc = "Recibe subsidio, escenario",
              pierde = "Pierde subsidio", gana = "Gana subsidio",
              rec_comp = "Recibe compensación", pob = "Población (% del total)")

tablas_grupo <- function(e, g) {
  cfg <- GRUPOS[[g]]
  gv  <- base[[cfg$var]]
  sub0 <- base$sub_ele0_pc
  sube <- base[[paste0("sub_ele", e, "_pc")]]
  come <- base[[paste0("comp", e, "_pc")]]
  dsub <- sube - sub0
  X <- cbind(sub_base = sub0, sub_esc = sube, comp = come, sub_comp = sube + come,
             d_sub = dsub, neto = dsub + come)
  D <- cbind(rec_base = sub0 > tol, rec_esc = sube > tol, pierde = dsub < -tol,
             gana = dsub > tol, rec_comp = come > tol, pob = TRUE)
  lev <- sort(unique(gv[!is.na(gv)]))
  f   <- factor(gv, levels = lev)
  suma <- function(M) {
    R <- rowsum(M * w, f, reorder = TRUE)
    R <- R[as.character(lev), , drop = FALSE]
    rbind(R, Total = colSums(M * w, na.rm = TRUE))
  }
  V  <- suma(X)                           # Q
  Yg <- suma(cbind(yd = yd))[, 1]
  Ng <- suma(cbind(n = rep(1, length(w))))[, 1]
  C  <- suma(D * 1)
  etiq <- c(unname(cfg$etiquetas[as.character(lev)]), "Total")
  mk <- function(M, nombres) {
    df <- as.data.frame(M); names(df) <- nombres
    tibble(grupo = etiq, df)
  }
  cob <- 100 * C / Ng
  cob[, "pob"] <- 100 * Ng / Ng[length(Ng)]
  list(
    mil = mk(V / 1e6, COLS_GRUPO),
    inc = mk(100 * V / Yg, COLS_GRUPO),
    con = mk(100 * sweep(V, 2, V[nrow(V), ], "/"), COLS_GRUPO),
    pib = mk(if (is.na(macro$pib)) V * NA else 100 * V / (macro$pib * 1e6), COLS_GRUPO),
    cob = mk(cob, COLS_COB)
  )
}

## 3.5 Movilidad entre estratos ---------------------------------------------------
mov_matriz <- function(e) {
  dist <- function(g) {
    t <- tapply(w, factor(g, levels = 1:4), sum); t[is.na(t)] <- 0
    100 * t / sum(t)
  }
  b <- dist(base$gr_estrato0); s <- dist(base[[paste0("gr_estrato", e)]])
  t <- dist(base[[paste0("gr_estratot", e)]])
  out <- rbind(cbind(b, s, t, s - b, t - b), Total = c(100, 100, 100, 0, 0))
  dimnames(out) <- list(c(ESTRATOS, "Total"),
                        c("Base", "Escenario: cambio en subsidio",
                          "Escenario: + compensación", "Diferencia (p.p.) subsidio",
                          "Diferencia (p.p.) + compensación"))
  out
}

## 3.6 Costo fiscal y beneficiarios ---------------------------------------------
FILAS_FIS <- c(
  "Costo subsidio base (millones Q/año)", "Costo subsidio escenario (millones Q/año)",
  "Cambio en costo del subsidio (millones Q/año)", "Costo compensación (millones Q/año)",
  "Costo total escenario: subsidio + compensación (millones Q/año)",
  "Cambio en costo total vs. base (millones Q/año)",
  "Hogares beneficiarios del subsidio, base", "Hogares beneficiarios del subsidio, escenario",
  "Hogares con compensación", "Compensación mensual promedio por hogar beneficiario (Q)",
  "Subsidio mensual promedio por hogar beneficiario, base (Q)",
  "Subsidio mensual promedio por hogar beneficiario, escenario (Q)",
  "Costo subsidio base (% PIB)", "Costo subsidio escenario (% PIB)",
  "Costo total escenario (% PIB)",
  "Aporte a la tarifa social registrado por INDE (millones Q)",
  "Subsidio base simulado / aporte INDE (%)")

fis_vector <- function(e) {
  sub0 <- base$sub_ele0_pc
  sube <- base[[paste0("sub_ele", e, "_pc")]]
  come <- base[[paste0("comp", e, "_pc")]]
  F <- numeric(17)
  F[1] <- msum(sub0, w); F[2] <- msum(sube, w); F[3] <- F[2] - F[1]
  F[4] <- msum(come, w); F[5] <- F[2] + F[4];   F[6] <- F[5] - F[1]
  ## sub/comp per cápita > tol identifica al hogar (se cuenta una vez por hogar)
  F[7] <- sum(w * tag * (sub0 > tol)); F[8] <- sum(w * tag * (sube > tol))
  F[9] <- sum(w * tag * (come > tol))
  F[10] <- if (F[9] > 0) F[4] * 1e6 / F[9] / 12 else 0
  F[11] <- if (F[7] > 0) F[1] * 1e6 / F[7] / 12 else 0
  F[12] <- if (F[8] > 0) F[2] * 1e6 / F[8] / 12 else 0
  F[13:15] <- 100 * F[c(1, 2, 5)] / macro$pib
  F[16] <- macro$sub_inde
  F[17] <- 100 * F[1] / macro$sub_inde
  setNames(F, FILAS_FIS)
}

## 3.7 Ejecutar ------------------------------------------------------------------
seccion("04 - Indicadores por escenario")

resultados <- lapply(setNames(esc_reforma, paste0("escenario_", esc_reforma)),
  function(e) {
    message("  escenario ", e)
    s <- subs[as.character(e)]
    out <- list(
      escenario = e,
      des       = escenarios$des_escenario[escenarios$escenario == e],
      des_corto = escenarios$des_corto[escenarios$escenario == e],
      sim_sub   = s,
      sim_com   = scom[as.character(e)],
      pov_gral  = pov_matriz(PLS$pov_gral, e),
      pov_ext   = pov_matriz(PLS$pov_ext,  e),
      pov_int   = pov_matriz(PLS$pov_int,  e),
      ineq      = ineq_matriz(e),
      kakwani   = kakwani_matriz(e),
      mov       = mov_matriz(e),
      fis       = fis_vector(e),
      tar_base  = tarifas$tabla %>% filter(esquema == 0),
      tar_esc   = tarifas$tabla %>% filter(esquema == s)
    )
    for (g in names(GRUPOS)) out$grupos[[g]] <- tablas_grupo(e, g)
    out
  })

## 3.8 Resumen de escenarios (base + [ys, yt] por escenario) ----------------------
FILAS_RES <- c("Pobreza (% personas)", "Cambio en pobreza vs. base (p.p.)",
               "Nuevos pobres vs. base (personas)", "Pobreza extrema (% personas)",
               "Cambio en pobreza extrema vs. base (p.p.)",
               "Pobreza línea internacional (% personas)",
               "Brecha mensual por hogar, pobres en la base (Q)",
               "Gini", "Cambio en Gini vs. base", "Palma", "Theil T",
               "Costo subsidio (millones Q/año)", "Costo compensación (millones Q/año)",
               "Costo total (millones Q/año)", "Cambio en costo total vs. base (millones Q/año)",
               "Costo total (% PIB)", "Hogares beneficiarios del subsidio",
               "Hogares con compensación")

res_col <- function(r, j) {
  P <- r$pov_gral; X <- r$pov_ext; I <- r$pov_int; Q <- r$ineq; F <- r$fis
  fis <- switch(j,
    c(F[1], 0,    F[1], 0,    F[13], F[7], 0),
    c(F[2], 0,    F[2], F[3], F[14], F[8], 0),
    c(F[2], F[4], F[5], F[6], F[15], F[8], F[9]))
  c(P[1, j], P[2, j], P[3, j], X[1, j], X[2, j], I[1, j], P[7, j],
    Q[1, j], Q[2, j], Q[3, j], Q[4, j], fis)
}
resumen <- cbind("Base (subsidio vigente)" = res_col(resultados[[1]], 1),
                 do.call(cbind, lapply(resultados, function(r) {
                   m <- cbind(res_col(r, 2), res_col(r, 3))
                   colnames(m) <- paste0(r$des_corto, c(": cambio en subsidio",
                                                        ": + compensación"))
                   m
                 })))
rownames(resumen) <- FILAS_RES
print(round(resumen, 3))

attr(resultados, "resumen") <- resumen
attr(resultados, "grupos")  <- GRUPOS
attr(resultados, "macro")   <- macro

saveRDS(resultados, paste0(fdbmod, "GTM_resumen.rds"))

suppressWarnings(rm(
  base, subeleh, simcomp, esq, s, hh, e, k, cin, col, src, ys, edad_v, gj, d,
  w, tag, yd, PLS, gini_ref, g, v, extra, resultados, resumen, subs, scom,
  esc_all, tarifas, umbr, tol, pov_col, pov_matriz, ineq_matriz,
  kakwani_matriz, tablas_grupo, mov_matriz, fis_vector, res_col, param_glob))
