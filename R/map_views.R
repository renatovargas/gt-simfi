# Mapa coroplético por región (plotly). Función pura: consume el objeto de
# resultados y el geojson regional (data/gis/regiones_guatemala.geojson, WGS84,
# construido por data-raw/build_gis.R). El pipeline entrega 8 regiones, no
# departamentos, así que el mapa es regional (igual que el script 05).

#' Lee el geojson regional como lista (sin depender de `sf` en la app).
leer_regiones_geo <- function(ruta = ruta_app("data", "gis", "regiones_guatemala.geojson")) {
  jsonlite::fromJSON(ruta, simplifyVector = FALSE)
}

#' @param escenario nombre completo del escenario a mapear.
#' @param vista,clave unidad (`mil`, `inc`, ...) y medida (`neto`, `d_sub`, ...) de `res$grupos`.
mapa_region <- function(res, geo, escenario, vista, clave) {
  g <- res$grupos
  d <- g[g$grupo == "reg" & g$vista == vista & g$clave == clave &
           g$escenario == escenario & g$categoria != "Total", ]
  stopifnot("Sin datos regionales para esa selección" = nrow(d) > 0)
  unidad <- VISTAS_ETIQ[[vista]]
  d$texto <- paste0(detalle_esc(res, d$escenario), "<br><b>Región ", d$categoria, "</b><br>",
                    d$medida, ": ", fmt_valor(d$valor), "<br>", unidad)
  hay_neg <- any(d$valor < 0, na.rm = TRUE) && any(d$valor > 0, na.rm = TRUE)
  plotly::plot_ly(
    type = "choropleth", geojson = geo, featureidkey = "properties.region_nom",
    locations = d$categoria, z = d$valor, text = d$texto,
    hovertemplate = "%{text}<extra></extra>",
    colorscale = if (hay_neg) "RdBu" else "Blues",
    zmid = if (hay_neg) 0 else NULL,
    marker = list(line = list(color = "white", width = 1)),
    colorbar = list(title = list(text = unidad, side = "right"), len = 0.8)) |>
    plotly::layout(
      geo = list(fitbounds = "locations", visible = FALSE, bgcolor = "rgba(0,0,0,0)"),
      margin = list(l = 0, r = 0, t = 10, b = 0),
      font = list(family = "system-ui, sans-serif", size = 12)) |>
    config_plotly(paste0("mapa_", clave))
}
