## =============================================================================
## Banco Mundial / INDE
## Herramienta de Microsimulación de Subsidios a la Electricidad
## Country: Guatemala 2023
## 03_GTM26SUB_Transferencia.R   (versión R de 08_GTM26SUB_Transferencia.do)
## Dependencias: 00_GTM26SUB_Master.R
##
## Compensaciones (hoja "sim_extra"), una por cada sim_com > 0 usado en
## escenarios activos. Valores anuales en Q, deflactados por "indice".
##   grupo_com  1 = hasta el decil decil_com (monto fijo al jefe de hogar)
##              2 = beneficiarios del bono social
##                    metodo_com 1: monto fijo mensual (valor_com)
##                    metodo_com 2: pct_com % de la mediana anual del bono
##              3 = hogares con consumo 0 < kWh < elect_com (jefe de hogar)
##
## Input:  fpresim disp_income.rds; fparams hojas escenarios y sim_extra
## Output: fdbmod GTM_simcomp.rds (individuo: hhid, hhin, comp_<k>, decyd, yd_pc)
## =============================================================================

seccion("08 - Compensaciones")

# 0. Parámetros -----------------------------------------------------------------

simcomp_inc <- sort(unique(escenarios$sim_com[escenarios$sim_com > 0 &
                                                 !is.na(escenarios$sim_com)]))

sim_extra <- read_excel(fparams, sheet = "sim_extra") %>%
  janitor::clean_names() %>%
  filter(activo == 1, sim_com %in% simcomp_inc)

faltan_com <- setdiff(simcomp_inc, sim_extra$sim_com)
if (length(faltan_com)) stop("Compensaciones usadas en 'escenarios' sin fila activa ",
                             "en 'sim_extra': ", paste(faltan_com, collapse = ", "))

# 1. Estimar escenarios de compensación ------------------------------------------

base <- readRDS(paste0(fpresim, "disp_income.rds")) %>%
  arrange(yd_pc, relation) %>%
  mutate(decyd = statar::xtile(yd_pc, n = 10, wt = factor))

jefe <- eqv(base$relation, 1)

for (k in simcomp_inc) {
  p <- sim_extra %>% filter(sim_com == k) %>% slice(1)
  col <- paste0("comp_", k)

  if (p$grupo_com == 1) {                          # deciles
    comp <- p$valor_com * le_st(base$decyd, p$decil_com) * jefe * 12

  } else if (p$grupo_com == 2) {                   # bono social
    stopifnot("Faltan dtr_bono_in / dtr_bono_ri en disp_income" =
                all(c("dtr_bono_in", "dtr_bono_ri") %in% names(base)))
    recibe <- eqv(base$dtr_bono_ri, 1)
    if (p$metodo_com == 1) {
      comp <- p$valor_com * recibe * 12
    } else if (p$metodo_com == 2) {
      ## mediana (ponderada) de las transferencias anuales del bono != 0
      s <- !is.na(base$dtr_bono_in) & base$dtr_bono_in != 0
      val_anual <- wpctile_st(base$dtr_bono_in[s], base$factor[s], 50)
      cat(sprintf("comp_%d: mediana anual del bono social = %.2f\n", k, val_anual))
      comp <- val_anual * recibe * (p$pct_com / 100)
    } else {
      warning("comp_", k, ": metodo_com no reconocido (", p$metodo_com, "); se usa 0")
      comp <- rep(0, nrow(base))
    }

  } else if (p$grupo_com == 3) {                   # consumo de electricidad
    sel <- lt_st(base$celq, p$elect_com) & gt_st(base$celq, 0) & !is.na(base$celq)
    if ("otroele" %in% names(base)) {
      sel <- sel & !eqv(base$otroele, 1)           # panel, hidro o eólica
    } else {
      message("Aviso: 'otroele' no está en disp_income; no se excluyen hogares ",
              "con fuentes propias (panel, hidro, eólica)")
    }
    comp <- p$valor_com * sel * jefe * 12

  } else {
    warning("comp_", k, ": grupo_com no reconocido (", p$grupo_com, "); se usa 0")
    comp <- rep(0, nrow(base))
  }

  base[[col]] <- comp / base$indice_ipc_encovi
  cat(sprintf("comp_%d (%s): costo anual = %s millones Q | beneficiarios = %s personas\n",
              k, coalesce(as.character(p$descr_com), ""),
              comma(msum(base[[col]], base$factor), accuracy = 0.1),
              comma(sum(base$factor[base[[col]] > 0], na.rm = TRUE))))
}

# 2. Guardar --------------------------------------------------------------------

base %>%
  filter(!is.na(hhin)) %>%
  select(hhid, hhin, matches("^comp_[0-9]+$"), decyd, yd_pc) %>%
  saveRDS(paste0(fdbmod, "GTM_simcomp.rds"))

suppressWarnings(rm(base, sim_extra, simcomp_inc, faltan_com, jefe, k, p, col,
                    comp, recibe, s, val_anual, sel))
