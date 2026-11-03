## =============================================================================
## Banco Mundial / INDE
## Herramienta de Microsimulación de Subsidios a la Electricidad
## Country: Guatemala 2023
## 10_GTM26SUB_Reportes.R
##   Excel de resultados con la estructura de 06_DOM26_Reportes.R (índice,
##   resumen con gráficos, hojas por escenario, comparativos con gráficos y
##   mapa, anexo) y los bloques propios de 10_GTM26SUB_Reportes.do (cobertura,
##   movilidad entre estratos, costo fiscal, tablas tarifarias).
## Dependencias: 00_GTM26SUB_Master.R, 09_GTM26SUB_Consolida.R
##
## Convención de signos: todo se expresa como BENEFICIO para el hogar
##   (positivo = el hogar gana; negativo = pierde subsidio).
##
## Input:  fdbmod GTM_resumen.rds
## Output: fresults Resultados_GTM26SUB_<fecha>.xlsx
##         fdbmod   GTM_insumos.rds   (matrices para la interfaz interactiva)
## =============================================================================

seccion("10 - Reportes")

# 0. Configuración ---------------------------------------------------------------

DPI_GRAFICOS  <- 96
ANCHO_GRAFICO <- 6      # pulgadas
ALTO_GRAFICO  <- 4
FILAS_GRAFICO <- 21     # filas de Excel que ocupa un gráfico de 4 pulgadas

DISCLAIMER <- paste0(
  "Descargo de responsabilidad: Las simulaciones presentadas son ejercicios ",
  "teóricos y no constituyen ninguna intención o propuesta de reforma por ",
  "parte del Gobierno de Guatemala.")

resultados <- readRDS(paste0(fdbmod, "GTM_resumen.RDS"))
resumen    <- attr(resultados, "resumen")
GRUPOS     <- attr(resultados, "grupos")
macro      <- attr(resultados, "macro")

## Nombres de columnas de las tablas por grupo (definidos en 09)
COLS_GRUPO <- setNames(names(resultados[[1]]$grupos$dec$mil)[-1],
                       c("sub_base", "sub_esc", "comp", "sub_comp", "d_sub", "neto"))
COLS_COB   <- setNames(names(resultados[[1]]$grupos$dec$cob)[-1],
                       c("rec_base", "rec_esc", "pierde", "gana", "rec_comp", "pob"))

esc_ids    <- sapply(resultados, `[[`, "escenario")
esc_cortos <- sapply(resultados, `[[`, "des_corto")
if (anyDuplicated(esc_cortos)) stop("Nombres cortos de escenarios duplicados ",
                                    "(columna des_corto de la hoja 'escenarios')")

VISTAS <- list(
  mil = "Millones de Q por año",
  inc = "% del ingreso disponible del grupo",
  con = "% del total (concentración)",
  pib = "% del PIB",
  cob = "Cobertura (% de personas del grupo)"
)

# 1. Estilos y utilidades de escritura --------------------------------------------

est <- list(
  titulo     = createStyle(fontSize = 12, fontColour = "#004480",
                           textDecoration = "bold", fontName = "Segoe UI"),
  titulo2    = createStyle(fontSize = 10, fontColour = "white", fgFill = "#0b313a",
                           textDecoration = "bold", fontName = "Segoe UI"),
  encabezado = createStyle(fontSize = 10, fgFill = "#beeef8", halign = "center",
                           valign = "center", wrapText = TRUE,
                           textDecoration = "bold", fontName = "Segoe UI",
                           border = "TopBottom", borderColour = "#006d80"),
  descriptor = createStyle(fontSize = 9, textDecoration = "bold", fontName = "Segoe UI"),
  total      = createStyle(textDecoration = "bold"),
  nota       = createStyle(fontName = "Segoe UI", fontColour = "gray",
                           fontSize = 8, textDecoration = "italic"),
  aviso      = createStyle(fontColour = "#002dc0", textDecoration = "bold"),
  link       = createStyle(fontColour = "blue", textDecoration = "underline",
                           fontName = "Segoe UI", fontSize = 11),
  texto      = createStyle(fontSize = 9, fontName = "Segoe UI", wrapText = TRUE,
                           valign = "top")
)
.fmt_cache <- list()
est_num <- function(fmt) {
  if (is.null(.fmt_cache[[fmt]])) {
    .fmt_cache[[fmt]] <<- createStyle(numFmt = fmt, fontName = "Segoe UI", fontSize = 10)
  }
  .fmt_cache[[fmt]]
}

#' Convierte matriz con rownames a data.frame (primera columna = etiqueta)
como_df <- function(M, etiqueta = "") {
  if (is.data.frame(M)) return(M)
  df <- data.frame(x = rownames(M), M, check.names = FALSE, stringsAsFactors = FALSE)
  names(df)[1] <- etiqueta
  df
}

#' Escribe un bloque: título, encabezado, etiquetas y datos. Devuelve la fila
#' siguiente libre (deja dos filas en blanco).
#' fmt_filas: lista nombrada índice_de_fila -> formato (sobrescribe fmt)
escribir_tabla <- function(wb, hoja, fila, col, titulo, M, fmt = "#,##0.0",
                           fmt_filas = NULL, negrita_total = FALSE, etiqueta = "") {
  df <- como_df(M, etiqueta)
  nc <- ncol(df); nr <- nrow(df)
  cols <- col:(col + nc - 1)
  writeData(wb, hoja, titulo, startRow = fila, startCol = col)
  addStyle(wb, hoja, est$titulo2, rows = fila, cols = cols, gridExpand = TRUE)
  writeData(wb, hoja, t(names(df)), startRow = fila + 1, startCol = col,
            colNames = FALSE)
  addStyle(wb, hoja, est$encabezado, rows = fila + 1, cols = cols, gridExpand = TRUE)
  writeData(wb, hoja, df, startRow = fila + 2, startCol = col, colNames = FALSE)
  filas <- (fila + 2):(fila + 1 + nr)
  addStyle(wb, hoja, est$descriptor, rows = filas, cols = col)
  if (nc > 1) {
    addStyle(wb, hoja, est_num(fmt), rows = filas, cols = cols[-1], gridExpand = TRUE)
    for (i in names(fmt_filas)) {
      addStyle(wb, hoja, est_num(fmt_filas[[i]]), rows = fila + 1 + as.integer(i),
               cols = cols[-1], gridExpand = TRUE)
    }
  }
  if (negrita_total) addStyle(wb, hoja, est$total, rows = fila + 1 + nr, cols = cols,
                              gridExpand = TRUE, stack = TRUE)
  fila + nr + 4
}

#' Guarda un ggplot como PNG temporal (ragg si está disponible)
guardar_grafico <- function(g, ancho = ANCHO_GRAFICO, alto = ALTO_GRAFICO) {
  tf <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(tf, width = ancho, height = alto, units = "in", res = DPI_GRAFICOS)
  } else {
    png(tf, width = ancho, height = alto, units = "in", res = DPI_GRAFICOS)
  }
  print(g)
  dev.off()
  tf
}

insertar <- function(wb, hoja, g, fila, col, ancho = ANCHO_GRAFICO, alto = ALTO_GRAFICO) {
  insertImage(wb, hoja, guardar_grafico(g, ancho, alto), startRow = fila,
              startCol = col, width = ancho, height = alto, units = "in")
}

link_indice <- function(wb, hoja, fila = 1, col = 12) {
  writeFormula(wb, hoja, startRow = fila, startCol = col,
               x = 'HYPERLINK("#Indice!A1", "<< Índice")')
  addStyle(wb, hoja, est$link, rows = fila, cols = col)
}

# 2. Gráficos ----------------------------------------------------------------------

tema <- theme_classic() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 11, face = "bold"))

#' Resumen: barras (solo subsidio / con compensación) y base como línea roja
graf_resumen <- function(fila, titulo, ylab, dec = 1) {
  v <- resumen[fila, ]
  df <- tibble(
    Categoria = factor(rep(esc_cortos, each = 2), levels = esc_cortos),
    Serie = factor(rep(c("Cambio en subsidio", "Con compensación"), length(esc_cortos)),
                   levels = c("Cambio en subsidio", "Con compensación")),
    Valor = as.numeric(v[-1]))
  base_df <- tibble(Categoria = factor(esc_cortos, levels = esc_cortos),
                    Valor = as.numeric(v[1]), Serie = "Base")
  ggplot() +
    geom_col(data = df, aes(Categoria, Valor, fill = Serie),
             position = position_dodge(0.75), width = 0.7) +
    geom_text(data = df, aes(Categoria, Valor, label = comma(Valor, accuracy = 10^-dec),
                             group = Serie),
              position = position_dodge(0.75), vjust = -0.5, size = 3) +
    geom_line(data = base_df, aes(Categoria, Valor, group = 1, colour = Serie),
              linewidth = 1) +
    geom_point(data = base_df, aes(Categoria, Valor, colour = Serie), size = 2.5) +
    scale_fill_manual(values = c("steelblue", "orange")) +
    scale_colour_manual(values = c(Base = "red")) +
    scale_x_discrete(labels = wrap_format(25)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(title = titulo, y = ylab, x = NULL, fill = NULL, colour = NULL) +
    tema
}

#' Comparativo por grupo: barras por categoría y escenario
graf_grupo <- function(mat, etiquetas, series, titulo, rotulo) {
  df <- expand.grid(k = seq_along(etiquetas), j = seq_along(series)) %>%
    mutate(cat = factor(etiquetas[k], levels = etiquetas),
           Escenario = factor(series[j], levels = series),
           Valor = mat[cbind(k, j)])
  ggplot(df, aes(cat, Valor, fill = Escenario)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    scale_x_discrete(labels = wrap_format(16)) +
    labs(title = titulo, x = rotulo, y = NULL, fill = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.position = "bottom",
          legend.text = element_text(size = 7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10, face = "bold")) +
    guides(fill = guide_legend(ncol = 2))
}

# 3. Libro e índice -------------------------------------------------------------

wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Segoe UI", fontSize = 10)

addWorksheet(wb, "Indice", gridLines = FALSE)
writeData(wb, "Indice", "Herramienta de microsimulación de subsidios a la electricidad - Guatemala",
          startRow = 2, startCol = 2)
addStyle(wb, "Indice", createStyle(fontSize = 15, textDecoration = "bold"), rows = 2, cols = 2)
writeData(wb, "Indice", paste("Generado:", format(Sys.time(), "%d/%m/%Y %H:%M")),
          startRow = 3, startCol = 2)

idx <- tibble(`Escenario` = esc_ids, `Descripción` = sapply(resultados, `[[`, "des"),
              `Esquema subsidio (sim_sub)` = sapply(resultados, `[[`, "sim_sub"),
              `Compensación (sim_com)` = sapply(resultados, `[[`, "sim_com"),
              `Hoja` = "")
writeData(wb, "Indice", idx, startRow = 5, startCol = 2)
addStyle(wb, "Indice", est$encabezado, rows = 5, cols = 2:6, gridExpand = TRUE)
for (i in seq_along(esc_ids)) {
  writeFormula(wb, "Indice", startRow = 5 + i, startCol = 6,
               x = sprintf('HYPERLINK("#sim%d!A1", "sim%d")', esc_ids[i], esc_ids[i]))
  addStyle(wb, "Indice", est$link, rows = 5 + i, cols = 6)
}
f <- 7 + length(esc_ids)
for (lk in list(c("resumen", "Resumen de escenarios"), c("comparativos", "Comparativos"),
                c("anexo", "Anexo: definiciones"))) {
  writeFormula(wb, "Indice", startRow = f, startCol = 3,
               x = sprintf('HYPERLINK("#%s!A1", "%s")', lk[1], lk[2]))
  addStyle(wb, "Indice", est$link, rows = f, cols = 3)
  f <- f + 1
}
writeData(wb, "Indice", DISCLAIMER, startRow = f + 1, startCol = 2)
addStyle(wb, "Indice", est$nota, rows = f + 1, cols = 2)
if (is.na(macro$pib)) {
  writeData(wb, "Indice", "PENDIENTE: PIB no definido en global_params; las cifras en % del PIB están vacías.",
            startRow = f + 2, startCol = 2)
  addStyle(wb, "Indice", est$aviso, rows = f + 2, cols = 2)
}
setColWidths(wb, "Indice", cols = 2:6, widths = c(12, 60, 18, 18, 10))

# 4. Hoja resumen -----------------------------------------------------------------
message("  hoja resumen")

addWorksheet(wb, "resumen", gridLines = FALSE)
writeData(wb, "resumen", "Resumen de escenarios de subsidio eléctrico", startRow = 1, startCol = 1)
addStyle(wb, "resumen", est$titulo, rows = 1, cols = 1)
link_indice(wb, "resumen", 1, 6)
nres <- ncol(resumen)
fres <- list("3" = "#,##0", "7" = "#,##0.00", "8" = "0.0000", "9" = "0.0000",
             "10" = "0.0000", "11" = "0.0000", "16" = "0.000", "17" = "#,##0",
             "18" = "#,##0")
fin_res <- escribir_tabla(wb, "resumen", 3, 1, "Indicadores principales", resumen,
                          fmt = "#,##0.00", fmt_filas = fres)
writeData(wb, "resumen", DISCLAIMER, startRow = fin_res - 2, startCol = 1)
addStyle(wb, "resumen", est$nota, rows = fin_res - 2, cols = 1)
setColWidths(wb, "resumen", cols = 1:(nres + 1), widths = c(52, rep(18, nres)))

graf_res <- list(
  graf_resumen(1,  "Pobreza general",                        "% personas"),
  graf_resumen(4,  "Pobreza extrema",                        "% personas"),
  graf_resumen(6,  "Pobreza, línea internacional",           "% personas"),
  graf_resumen(7,  "Brecha de pobreza (hogares pobres base)", "Q por hogar al mes", 0),
  graf_resumen(8,  "Desigualdad (Gini)",                     "Coeficiente de Gini", 4),
  graf_resumen(14, "Costo fiscal total",                     "Millones de Q al año", 0)
)
for (i in seq_along(graf_res)) {
  fila <- fin_res + 1 + ((i - 1) %/% 2) * FILAS_GRAFICO
  col  <- if (i %% 2 == 1) 1 else 4
  insertar(wb, "resumen", graf_res[[i]], fila, col)
}

# 5. Hojas por escenario -----------------------------------------------------------
message("  hojas por escenario")

FMT_POV <- list("3" = "#,##0", "4" = "0.00", "5" = "0.00", "9" = "0.000")
FMT_FIS <- list("7" = "#,##0", "8" = "#,##0", "9" = "#,##0",
                "13" = "0.000", "14" = "0.000", "15" = "0.000")
FMT_VISTA <- c(mil = "#,##0.0", inc = "0.00", con = "0.0", pib = "0.0000", cob = "0.0")

ancho_vista <- 1 + 6 + 1          # etiqueta + 6 columnas + espaciador
                                  # (las tablas de pobreza usan las mismas bandas)

for (r in resultados) {
  e    <- r$escenario
  hoja <- paste0("sim", e)
  addWorksheet(wb, hoja, gridLines = FALSE)
  writeData(wb, hoja, paste0("Escenario ", e, ": ", r$des), startRow = 1, startCol = 1)
  addStyle(wb, hoja, est$titulo, rows = 1, cols = 1)
  writeData(wb, hoja, paste("Valores anuales en quetzales salvo indicación.",
                            "Cambios respecto a la base (ys0 = yd). Deciles fijos según yd_pc.",
                            "Positivo = beneficio para el hogar."),
            startRow = 2, startCol = 1)
  addStyle(wb, hoja, est$nota, rows = 2, cols = 1)
  link_indice(wb, hoja, 1, 12)

  cols_pov <- c("Base", "Escenario: cambio en subsidio",
                "Escenario: cambio en subsidio + compensación")
  ## 5.1 Pobreza (3 líneas lado a lado)
  f0 <- 4
  fin <- f0
  for (j in seq_along(c("pov_gral", "pov_ext", "pov_int"))) {
    nm <- c("pov_gral", "pov_ext", "pov_int")[j]
    tit <- c("1. Pobreza (línea nacional general)", "Pobreza extrema (línea nacional)",
             sprintf("Pobreza, línea internacional (US$%.2f PPA/día)", OPC$pline_int))[j]
    M <- r[[nm]]; colnames(M) <- cols_pov
    fin <- escribir_tabla(wb, hoja, f0, 1 + (j - 1) * ancho_vista, tit, M,
                          fmt = "#,##0.00", fmt_filas = FMT_POV)
  }
  ## 5.2 Desigualdad y progresividad
  M <- r$ineq; colnames(M) <- cols_pov
  fin2 <- escribir_tabla(wb, hoja, fin, 1, "2. Desigualdad", M, fmt = "0.0000")
  fin  <- escribir_tabla(wb, hoja, fin, 1 + ancho_vista,
                         "Progresividad (índices de concentración y Kakwani, ordenado por yd)",
                         r$kakwani, fmt = "0.0000")
  fin <- max(fin, fin2)

  ## 5.3 Tablas por grupo (vistas lado a lado)
  writeData(wb, hoja, "3. Distribución por grupos de población", startRow = fin, startCol = 1)
  addStyle(wb, hoja, est$titulo, rows = fin, cols = 1)
  fin <- fin + 1
  for (g in names(GRUPOS)) {
    fg <- fin
    for (v in seq_along(VISTAS)) {
      vn <- names(VISTAS)[v]
      df <- r$grupos[[g]][[vn]]
      names(df)[1] <- GRUPOS[[g]]$titulo
      fg <- escribir_tabla(wb, hoja, fin, 1 + (v - 1) * ancho_vista,
                           paste0(GRUPOS[[g]]$titulo, " - ", VISTAS[[vn]]), df,
                           fmt = FMT_VISTA[[vn]], negrita_total = TRUE)
    }
    fin <- fg
  }

  ## 5.4 Movilidad, costo fiscal
  fin <- escribir_tabla(wb, hoja, fin, 1,
                        "4. Distribución de la población por estrato (% de personas)",
                        r$mov, fmt = "0.0", negrita_total = TRUE)
  fis <- matrix(r$fis, ncol = 1, dimnames = list(names(r$fis), "Valor"))
  f_fis <- fin
  fin <- escribir_tabla(wb, hoja, fin, 1, "5. Costo fiscal y beneficiarios", fis,
                        fmt = "#,##0.0", fmt_filas = FMT_FIS)
  if (is.na(macro$pib)) {
    writeData(wb, hoja, "PENDIENTE: definir el PIB en la hoja global_params",
              startRow = f_fis + 14, startCol = 3)
    addStyle(wb, hoja, est$aviso, rows = f_fis + 14, cols = 3)
  }

  ## 5.5 Tablas tarifarias de referencia
  tar_fmt <- function(tb) {
    tb %>% transmute(Distribuidora = distribuidora, Tramo = tramo,
                     `Desde (kWh/mes, >)` = desde, `Hasta (kWh/mes, <=)` = hasta,
                     `Cargo fijo (Q/mes)` = cargo_fijo, `Tarifa plena (Q/kWh)` = plena,
                     `Tarifa tope (Q/kWh)` = tope, `Tarifa objetivo (Q/kWh)` = objetivo,
                     `Aporte / subsidio (Q/kWh)` = aporte,
                     `Paga el usuario (Q/kWh)` = paga)
  }
  writeData(wb, hoja, "6. Parámetros tarifarios de referencia", startRow = fin, startCol = 1)
  addStyle(wb, hoja, est$titulo, rows = fin, cols = 1)
  fin <- escribir_tabla(wb, hoja, fin + 1, 1, "a. Base: esquema vigente (esquema 0)",
                        tar_fmt(r$tar_base), fmt = "0.00000")
  if (r$sim_sub != 0) {
    fin <- escribir_tabla(wb, hoja, fin, 1, paste("b. Escenario: esquema", r$sim_sub),
                          tar_fmt(r$tar_esc), fmt = "0.00000")
  } else {
    writeData(wb, hoja, "b. Escenario: mantiene el esquema vigente (esquema 0)",
              startRow = fin, startCol = 1)
  }

  setColWidths(wb, hoja, cols = 1:(ancho_vista * length(VISTAS)),
               widths = rep(c(30, rep(14, 6), 3), length(VISTAS)))
}

# 6. Comparativos ------------------------------------------------------------------
message("  hoja comparativos")

#' Matriz grupo x escenario de una columna de una vista (sin fila Total)
comp_mat <- function(g, vista, columna, incluir_base = FALSE) {
  sin_total <- function(df, columna) head(df[[columna]], -1)
  cols <- lapply(resultados, function(r) sin_total(r$grupos[[g]][[vista]], columna))
  M <- do.call(cbind, cols)
  colnames(M) <- esc_cortos
  df0 <- resultados[[1]]$grupos[[g]][[vista]]
  if (incluir_base) {
    M <- cbind(Base = sin_total(df0, COLS_GRUPO[["sub_base"]]), M)
  }
  rownames(M) <- head(df0$grupo, -1)
  M
}

COMPARATIVOS <- list(
  list(vista = "inc", col = COLS_GRUPO[["neto"]], base = FALSE,
       titulo = "Efecto neto (▲ subsidio + compensación), % del ingreso disponible"),
  list(vista = "inc", col = COLS_GRUPO[["d_sub"]], base = FALSE,
       titulo = "Cambio en el subsidio, % del ingreso disponible"),
  list(vista = "inc", col = COLS_GRUPO[["comp"]], base = FALSE,
       titulo = "Compensación, % del ingreso disponible"),
  list(vista = "inc", col = COLS_GRUPO[["sub_esc"]], base = TRUE,
       titulo = "Subsidio (nivel), % del ingreso disponible"),
  list(vista = "con", col = COLS_GRUPO[["sub_esc"]], base = TRUE,
       titulo = "Concentración del subsidio (% del total)"),
  list(vista = "mil", col = COLS_GRUPO[["neto"]], base = FALSE,
       titulo = "Efecto neto, millones de Q por año"),
  list(vista = "cob", col = COLS_COB[["pierde"]], base = FALSE,
       titulo = "Personas que pierden subsidio (% del grupo)")
)

hoja <- "comparativos"
addWorksheet(wb, hoja, gridLines = FALSE)
writeData(wb, hoja, "Comparativo de escenarios por grupos de población", startRow = 1, startCol = 2)
addStyle(wb, hoja, est$titulo, rows = 1, cols = 2)
link_indice(wb, hoja, 1, 8)
col_graf <- length(esc_cortos) + 5
fila <- 3
insumos_comp <- list()
for (cp in COMPARATIVOS) {
  writeData(wb, hoja, cp$titulo, startRow = fila, startCol = 2)
  addStyle(wb, hoja, est$titulo, rows = fila, cols = 2)
  fila <- fila + 1
  insumos_comp[[cp$titulo]] <- list()
  for (g in names(GRUPOS)) {
    M <- comp_mat(g, cp$vista, cp$col, cp$base)
    insumos_comp[[cp$titulo]][[g]] <- M
    escribir_tabla(wb, hoja, fila, 2, GRUPOS[[g]]$titulo, M,
                   fmt = FMT_VISTA[[cp$vista]], etiqueta = GRUPOS[[g]]$titulo)
    insertar(wb, hoja, graf_grupo(M, rownames(M), colnames(M), cp$titulo,
                                  GRUPOS[[g]]$titulo),
             fila, col_graf)
    fila <- fila + FILAS_GRAFICO + 1
  }
}
setColWidths(wb, hoja, cols = 2:(length(esc_cortos) + 3),
             widths = c(30, rep(16, length(esc_cortos) + 1)))

## 6.1 Mapa por región (opcional, requiere shapefile de departamentos) -------------
if (file.exists(OPC$shp_deptos)) {
  mapa <- tryCatch({
    M <- comp_mat("reg", "inc", COLS_GRUPO[["neto"]])
    reg_df <- as_tibble(M, rownames = "region_nom") %>%
      pivot_longer(-region_nom, names_to = "Escenario", values_to = "neto")
    shp <- sf::st_read(OPC$shp_deptos, quiet = TRUE) %>%
      mutate(region = region_de_depto(as.integer(.data[[OPC$shp_codcol]]))) %>%
      filter(!is.na(region)) %>%
      group_by(region) %>% summarise(.groups = "drop") %>%
      mutate(region_nom = unname(REGIONES[as.character(region)]))
    shp %>%
      inner_join(reg_df, by = "region_nom", relationship = "many-to-many") %>%
      mutate(Escenario = factor(Escenario, levels = esc_cortos)) %>%
      ggplot() +
      geom_sf(aes(fill = neto), colour = "white") +
      facet_wrap(~ Escenario) +
      scale_fill_gradient2(low = "firebrick", mid = "grey95", high = "steelblue",
                           midpoint = 0, name = "% ingreso") +
      labs(title = "Efecto neto por región (% del ingreso disponible)") +
      theme_void() + theme(legend.position = "bottom")
  }, error = function(err) {
    warning("No se pudo construir el mapa: ", conditionMessage(err)); NULL
  })
  if (!is.null(mapa)) {
    writeData(wb, hoja, "Mapa: efecto neto por región", startRow = fila, startCol = 2)
    addStyle(wb, hoja, est$titulo, rows = fila, cols = 2)
    insertar(wb, hoja, mapa, fila + 1, 2, ancho = 9,
             alto = 3 + 3 * ceiling(length(esc_cortos) / 3))
  }
} else {
  message("  (sin mapa: no existe ", OPC$shp_deptos, ")")
}

# 7. Anexo -------------------------------------------------------------------------

hoja <- "anexo"
addWorksheet(wb, hoja, gridLines = FALSE)
link_indice(wb, hoja, 1, 5)
esc_tab <- escenarios %>%
  transmute(No. = escenario, `Descripción corta` = des_corto,
            `Descripción` = des_escenario,
            `Esquema (sim_sub)` = sim_sub, `Compensación (sim_com)` = sim_com)
fa <- escribir_tabla(wb, hoja, 2, 2, "Descripción de escenarios", esc_tab, fmt = "0")

umbr <- umbrales_estrato()
DEFS <- tribble(
  ~Indicador, ~`Descripción`,
  "yd", "Ingreso disponible per cápita observado (refleja el subsidio vigente), en Q anuales reales.",
  "ys<e>", "yd + (subsidio del escenario e - subsidio vigente). Efecto del cambio en el subsidio.",
  "yt<e>", "ys<e> + compensación (transferencia) del escenario e.",
  "Pobreza", "Porcentaje de personas con ingreso per cápita inferior a la línea de pobreza.",
  "Nuevos pobres", "Diferencia en personas pobres respecto a la base (negativo = salen de la pobreza).",
  "FGT1 / FGT2", "Brecha y severidad de la pobreza (Foster-Greer-Thorbecke), % de la línea.",
  "Brecha mensual por hogar", "Suma de brechas per cápita del hogar / 12, promedio entre hogares.",
  "Brecha de nuevos pobres (% PIB)", "Ingreso anual necesario para que los nuevos pobres alcancen la línea, como % del PIB.",
  "Gini / Theil / Palma", "Desigualdad del ingreso; Palma = participación del 10% más rico / 40% más pobre.",
  "Concentración / Kakwani", "Índice de concentración ordenado por yd; Kakwani de una transferencia = Gini(yd) - concentración (positivo = progresiva).",
  "% del ingreso disponible", "Monto del instrumento en el grupo / ingreso disponible total del grupo.",
  "% del total", "Monto del instrumento en el grupo / monto nacional del instrumento.",
  "% del PIB", paste0("Monto / PIB (", comma(macro$pib), " millones de Q, hoja global_params)."),
  "Cobertura", paste0("% de personas del grupo con subsidio / compensación mayor a Q", OPC$tol_recibe, " anuales per cápita."),
  "Deciles", "Diez grupos de igual población ordenados por yd per cápita (fijos entre escenarios).",
  "Estratos", sprintf("Pobre extremo (< US$%.2f PPA/día), Pobre (< %.2f), Vulnerable (< %.2f), Clase media o más. Umbrales anuales: Q%s, Q%s, Q%s por persona.",
                      OPC$est_pext, OPC$est_pob, OPC$est_vul, comma(umbr[1]), comma(umbr[2]), comma(umbr[3])),
  "Regiones", "Regiones administrativas: Metropolitana, Norte, Nororiente, Suroriente, Central, Suroccidente, Noroccidente, Petén.",
  "Distribuidora", "Aproximada por departamento (EEGSA, DEOCSA, DEORSA). PLACEHOLDER: cooperativas no identificadas.",
  "Tarifas", "aporte = max(0, min(plena, tope) - objetivo); paga el usuario = plena - aporte (hoja sim_sub)."
)
fa <- escribir_tabla(wb, hoja, fa, 2, "Descripción de indicadores", DEFS)
addStyle(wb, hoja, est$texto, rows = (fa - 2 - nrow(DEFS)):(fa - 3), cols = 3,
         gridExpand = TRUE)
setColWidths(wb, hoja, cols = 2:6, widths = c(30, 90, 50, 16, 16))

# 8. Guardar -----------------------------------------------------------------------

insumos <- list(resumen = resumen, comparativos = insumos_comp,
                escenarios = lapply(resultados, function(r) r[setdiff(names(r), "grupos")]),
                grupos = lapply(resultados, `[[`, "grupos"))
saveRDS(insumos, paste0(fdbmod, "GTM_insumos.rds"))

nombre <- paste0(fresults, "Resultados_GTM26SUB_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
saveWorkbook(wb, nombre, overwrite = TRUE)
message("Resultados exportados a: ", nombre)

if (isTRUE(OPC$abrir_excel)) {
  if (.Platform$OS.type == "windows") shell.exec(nombre) else system2("open", nombre)
}
