# Vistas de resultados: funciones puras que consumen solo el objeto de resultados
# (salida de combinar_resultados()). Sin Shiny y sin llamar al motor.

VISTAS_ETIQ <- c(
  mil = "Millones de Q por año",
  inc = "% del ingreso disponible del grupo",
  con = "% del total (concentración)",
  pib = "% del PIB",
  cob = "% de personas del grupo"
)

MEDIDAS_MONTO <- c(
  "Efecto neto (cambio en subsidio + compensación)" = "neto",
  "Cambio en subsidio"                              = "d_sub",
  "Compensación"                                    = "comp",
  "Subsidio del escenario"                          = "sub_esc",
  "Subsidio base"                                   = "sub_base",
  "Subsidio + compensación"                         = "sub_comp"
)
MEDIDAS_COBERTURA <- c(
  "Recibe subsidio en el escenario" = "rec_esc",
  "Recibe subsidio en la base"      = "rec_base",
  "Pierde subsidio"                 = "pierde",
  "Gana subsidio"                   = "gana",
  "Recibe compensación"             = "rec_comp"
)

TIPOS_POBREZA <- c("Pobreza general" = "general", "Pobreza extrema" = "extrema",
                   "Línea internacional" = "internacional")

GRUPOS_ETIQ <- c(dec = "Deciles", estr = "Estrato", urb = "Área", sex = "Sexo del jefe de hogar",
                 cat = "Categoría de hogar", reg = "Regiones")

# --- Tablas ---------------------------------------------------------------------

#' Formatea una fila de la matriz de indicadores según su unidad.
formato_fila <- function(nombre, x) {
  dig <- ifelse(grepl("Nuevos pobres|Hogares", nombre), 0,
                ifelse(grepl("Gini|Palma|Theil", nombre), 3, 2))
  mapply(\(v, d) if (is.na(v)) "" else format(round(v, d), big.mark = ",", nsmall = d, trim = TRUE),
         x, dig, USE.NAMES = FALSE)
}

tabla_resumen <- function(res) {
  m <- res$resumen
  out <- as.data.frame(lapply(seq_len(ncol(m)), \(j) formato_fila(rownames(m), m[, j])),
                       stringsAsFactors = FALSE)
  names(out) <- colnames(m)
  cbind(Indicador = rownames(m), out, stringsAsFactors = FALSE)
}

#' Pivota un data frame largo a ancho: filas (primera columna) x columnas.
ancho <- function(d, fila, columna, valor = "valor", nombre_fila = "Indicador") {
  filas <- unique(d[[fila]]); cols <- unique(d[[columna]])
  m <- tapply(d[[valor]], list(factor(d[[fila]], filas), factor(d[[columna]], cols)), sum)
  out <- data.frame(filas, as.data.frame(unclass(m), check.names = FALSE), check.names = FALSE,
                    stringsAsFactors = FALSE)
  names(out)[1] <- nombre_fila
  rownames(out) <- NULL
  out
}

tabla_pobreza <- function(res, tipo, claves = CLAVES_POB) {
  d <- res$pobreza[res$pobreza$tipo == tipo & res$pobreza$clave %in% claves, ]
  d <- d[order(match(d$clave, claves)), ]
  ancho(d, "indicador", "serie")
}

tabla_desigualdad <- function(res) {
  d <- res$desigualdad
  d <- d[order(match(d$clave, CLAVES_DESIG)), ]
  ancho(d, "indicador", "serie")
}

tabla_kakwani <- function(res) {
  d <- res$kakwani
  d$col_nombre <- paste0(etiqueta_corta(res, d$escenario), ": ", tolower(d$indicador))
  ancho(d, "instrumento", "col_nombre", nombre_fila = "Instrumento")
}

tabla_fiscal <- function(res) {
  d <- res$fiscal
  d <- d[order(match(d$clave, CLAVES_FIS)), ]
  d$escenario <- etiqueta_corta(res, d$escenario)
  ancho(d, "indicador", "escenario")
}

tabla_grupo <- function(res, grupo, vista) {
  d <- res$grupos[res$grupos$grupo == grupo & res$grupos$vista == vista, ]
  d$col_nombre <- paste0(etiqueta_corta(res, d$escenario), ": ", d$medida)
  ancho(d, "categoria", "col_nombre", nombre_fila = res$grupos_meta[[grupo]]$titulo)
}

tabla_tarifas <- function(res, escenario) {
  d <- res$tarifas[res$tarifas$escenario == escenario, ]
  d$escenario <- NULL
  d
}

# --- Gráficos (plotly) ------------------------------------------------------------
# Ejes y leyendas usan etiquetas cortas ("Base", "Escenario 1", ...); el nombre
# completo, el esquema, la compensación y el valor exacto van en el hover.

BOTONES_PLOTLY_QUITAR <- c(
  "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
  "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines",
  "zoomInGeo", "zoomOutGeo", "hoverClosestGeo", "pan2d")

#' Barra de plotly: solo descarga PNG (alta resolución), sin logo.
config_plotly <- function(p, archivo = "grafico") {
  plotly::config(p, locale = "es", displaylogo = FALSE,
                 modeBarButtonsToRemove = BOTONES_PLOTLY_QUITAR,
                 toImageButtonOptions = list(format = "png", filename = archivo, scale = 2))
}

#' Valor exacto para el hover (hasta 6 cifras significativas, sin notación científica).
fmt_valor <- function(v) {
  vapply(v, \(x) if (is.na(x)) "s/d" else
           format(signif(x, 6), big.mark = ",", scientific = FALSE, trim = TRUE), "")
}

#' Etiqueta corta de cada serie ("Base" se conserva; el resto es "Escenario N").
etiqueta_corta <- function(res, nombres) {
  eq <- res$equivalencias
  et <- eq$etiqueta[match(nombres, eq$nombre)]
  ifelse(nombres == SERIE_BASE | is.na(et), nombres, et)
}

#' Descripción completa (HTML) de cada serie para el hover.
detalle_esc <- function(res, nombres) {
  eq <- res$equivalencias
  i <- match(ifelse(nombres == SERIE_BASE, eq$nombre[1], nombres), eq$nombre)
  esc <- \(x) htmltools::htmlEscape(x)
  ifelse(i == 1L,
         paste0("<b>Base</b>: ", esc(eq$nombre[1]), "<br>Esquema: ", esc(eq$esquema[1]),
                "<br>Compensación: ", esc(eq$compensacion[1])),
         paste0("<b>", eq$etiqueta[i], "</b>: ", esc(eq$nombre[i]),
                "<br>Esquema: ", esc(eq$esquema[i]),
                "<br>Compensación: ", esc(eq$compensacion[i])))
}

#' Tabla "Escenario N = nombre completo (esquema, compensación)" para la interfaz.
tabla_equivalencias <- function(res) {
  eq <- res$equivalencias
  data.frame(`Etiqueta en gráficos` = eq$etiqueta, Escenario = eq$nombre,
             `Esquema tarifario` = eq$esquema, `Compensación` = eq$compensacion,
             check.names = FALSE, stringsAsFactors = FALSE)
}

#' Barras con plotly a partir de un data frame con `x`, `color`, `valor` y `texto`.
barras_plotly <- function(d, ylab, barmode = "group", horizontal = FALSE, archivo = "grafico",
                          leyenda = TRUE, leyenda_siempre = FALSE) {
  d$x <- factor(d$x, levels = unique(d$x))
  d$color <- factor(d$color, levels = unique(d$color))
  p <- if (horizontal) {
    plotly::plot_ly(d, x = ~valor, y = ~x, color = ~color, hovertext = ~texto, type = "bar",
                    orientation = "h", textposition = "none", hoverinfo = "text")
  } else {
    plotly::plot_ly(d, x = ~x, y = ~valor, color = ~color, hovertext = ~texto, type = "bar",
                    textposition = "none", hoverinfo = "text")
  }
  eje_valor <- list(title = ylab, zeroline = TRUE)
  eje_cat <- list(title = "", automargin = TRUE)
  if (horizontal) eje_cat$autorange <- "reversed"
  p |>
    plotly::layout(
      barmode = barmode,
      xaxis = if (horizontal) eje_valor else eje_cat,
      yaxis = if (horizontal) eje_cat else eje_valor,
      showlegend = leyenda && (leyenda_siempre || nlevels(d$color) > 1),
      legend = list(orientation = "h", y = -0.2, title = list(text = "")),
      hoverlabel = list(align = "left"),
      font = list(family = "system-ui, sans-serif", size = 12)) |>
    config_plotly(archivo)
}

#' Barras por escenario (Base, cambio en subsidio, con compensación).
grafico_series <- function(res, d, ylab, archivo = "grafico") {
  d$x <- etiqueta_corta(res, d$escenario)
  d$texto <- paste0(detalle_esc(res, d$escenario), "<br><b>", d$ingreso, "</b><br>",
                    ylab, ": ", fmt_valor(d$valor))
  d$color <- as.character(d$ingreso)
  barras_plotly(d, ylab, archivo = archivo)
}

grafico_pobreza <- function(res, tipo, clave = "tasa") {
  d <- res$pobreza[res$pobreza$tipo == tipo & res$pobreza$clave == clave, ]
  grafico_series(res, d, d$indicador[1], paste0("pobreza_", tipo, "_", clave))
}

grafico_desigualdad <- function(res, clave) {
  d <- res$desigualdad[res$desigualdad$clave == clave, ]
  grafico_series(res, d, d$indicador[1], paste0("desigualdad_", clave))
}

#' Costo fiscal por escenario: subsidio + compensación (millones de Q por año).
grafico_fiscal <- function(res) {
  f <- res$fiscal
  v <- \(k, esc) f$valor[f$clave == k & f$escenario == esc]
  esc <- res$nombres
  d <- rbind(
    data.frame(escenario = SERIE_BASE, componente = "Subsidio", valor = v("costo_base", esc[1])),
    do.call(rbind, lapply(esc, \(e) {
      con <- !isFALSE(res$con_compensacion[[e]])
      data.frame(escenario = e, componente = c("Subsidio", "Compensación")[c(TRUE, con)],
                 valor = c(v("costo_esc", e), v("costo_comp", e))[c(TRUE, con)])
    })))
  d$x <- etiqueta_corta(res, d$escenario)
  d$color <- d$componente
  d$texto <- paste0(detalle_esc(res, d$escenario), "<br><b>", d$componente, "</b><br>",
                    "Millones de Q por año: ", fmt_valor(d$valor))
  barras_plotly(d, "Millones de Q por año", barmode = "stack", archivo = "costo_fiscal")
}

grafico_costo_pib <- function(res) {
  f <- res$fiscal
  d <- rbind(
    data.frame(escenario = SERIE_BASE, valor = f$valor[f$clave == "pib_base" & f$escenario == res$nombres[1]]),
    data.frame(escenario = res$nombres,
               valor = f$valor[f$clave == "pib_total"][match(res$nombres, f$escenario[f$clave == "pib_total"])]))
  d$x <- etiqueta_corta(res, d$escenario)
  d$color <- "Costo"
  d$texto <- paste0(detalle_esc(res, d$escenario), "<br>% del PIB: ", fmt_valor(d$valor))
  barras_plotly(d, "% del PIB", archivo = "costo_pib")
}

#' Incidencia por grupo (deciles, estrato, área, ...) para una vista y una medida.
grafico_grupo <- function(res, grupo, vista, clave, con_total = FALSE) {
  d <- res$grupos[res$grupos$grupo == grupo & res$grupos$vista == vista & res$grupos$clave == clave, ]
  if (!con_total) d <- d[d$categoria != "Total", ]
  d$x <- d$categoria
  d$color <- etiqueta_corta(res, d$escenario)
  d$texto <- paste0(detalle_esc(res, d$escenario), "<br><b>", d$categoria, "</b><br>",
                    d$medida, ": ", fmt_valor(d$valor), "<br>", VISTAS_ETIQ[[vista]])
  barras_plotly(d, paste0(d$medida[1], " (", VISTAS_ETIQ[[vista]], ")"),
                horizontal = grupo == "reg", archivo = paste0("incidencia_", grupo),
                leyenda_siempre = TRUE)
}

#' Distribución de la población por estrato de ingreso (base, subsidio, + compensación).
grafico_estratos <- function(res) {
  d <- res$estratos
  d <- d[d$clave_col %in% c("base", "esc_sub", "esc_comp") & d$estrato != "Total", ]
  # La columna base es la misma en todos los escenarios: se conserva una vez
  d <- d[d$clave_col != "base" | d$escenario == res$nombres[1], ]
  d$escenario <- ifelse(d$clave_col == "base", SERIE_BASE, d$escenario)
  completo <- c(base = "Base", esc_sub = "Cambio en subsidio", esc_comp = "Con compensación")
  corto <- c(base = "Base", esc_sub = "Subsidio", esc_comp = "+ compensación")
  d$grupo_x <- etiqueta_corta(res, d$escenario)
  d$ingreso <- unname(corto[d$clave_col])
  d$orden_x <- match(d$grupo_x, c(SERIE_BASE, res$equivalencias$etiqueta[-1])) * 10 +
    match(d$clave_col, names(completo))
  d <- d[order(d$orden_x), ]
  d$estrato <- factor(d$estrato, levels = unique(d$estrato))
  d$texto <- paste0(detalle_esc(res, d$escenario), "<br><b>", completo[d$clave_col], "</b><br>",
                    d$estrato, ": ", fmt_valor(d$valor), " % de la población")
  # Eje x de dos niveles: escenario y tipo de ingreso
  plotly::plot_ly(d, x = list(d$grupo_x, d$ingreso), y = ~valor, color = ~estrato, hovertext = ~texto,
                  type = "bar", textposition = "none", hoverinfo = "text") |>
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "", automargin = TRUE),
      yaxis = list(title = "% de la población"),
      legend = list(orientation = "h", y = -0.25, title = list(text = "")),
      hoverlabel = list(align = "left"),
      font = list(family = "system-ui, sans-serif", size = 12)) |>
    config_plotly("estratos")
}
