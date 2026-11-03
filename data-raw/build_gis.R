# Construye data/gis/regiones_guatemala.geojson: las 8 regiones administrativas
# (disolviendo los 22 departamentos como hace `05`), en WGS84 para plotly.
# Se corre desde la raíz del repo. No modifica el geojson original.
#
# El geojson original declara EPSG::42500, que PROJ no reconoce, y sus
# coordenadas están en metros. Verificado: coinciden con la proyección
# transversal de Mercator de Guatemala (GTM: meridiano central -90.5, k = 0.9998,
# falso este 500000, falso norte 0, WGS84): la extensión resultante es
# lon -92.24 a -88.22, lat 13.74 a 17.82 y el centroide de Guatemala cae en la
# capital. UTM 15N o CRTM05 (Costa Rica) desplazan el mapa.

suppressPackageStartupMessages({library(sf); library(dplyr)})

origen  <- "data/gis/departamentos_guatemala.json"
destino <- "data/gis/regiones_guatemala.geojson"
crs_gtm <- "+proj=tmerc +lat_0=0 +lon_0=-90.5 +k=0.9998 +x_0=500000 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Funciones del colega (región de cada departamento y nombres), sin editarlas
env <- new.env(parent = globalenv())
sys.source("reference/scripts_R/x_GTM26SUB_funciones.R", envir = env)

deptos <- suppressWarnings(st_read(origen, quiet = TRUE))
st_crs(deptos) <- NA   # se descarta el WGS84 supuesto por sf

reg <- deptos |>
  st_set_crs(crs_gtm) |>
  mutate(cod_reg = env$region_de_depto(as.integer(cod_dep))) |>
  filter(!is.na(cod_reg)) |>
  st_make_valid() |>
  group_by(cod_reg) |>
  summarise(n_deptos = n(), .groups = "drop") |>
  st_make_valid() |>
  # La unión puede devolver una colección con restos lineales: se conservan solo polígonos
  st_collection_extract("POLYGON") |>
  # Simplificación en metros (500 m) para aligerar el archivo
  st_simplify(dTolerance = 500, preserveTopology = TRUE) |>
  mutate(region_nom = unname(env$REGIONES[as.character(cod_reg)])) |>
  st_transform(4326) |>
  select(cod_reg, region_nom, n_deptos)

stopifnot(nrow(reg) == 8, sum(reg$n_deptos) == 22, !anyNA(reg$region_nom))
bb <- st_bbox(reg)
stopifnot(bb[["xmin"]] > -92.5, bb[["xmax"]] < -88, bb[["ymin"]] > 13.5, bb[["ymax"]] < 18)

if (file.exists(destino)) file.remove(destino)
st_write(reg, destino, driver = "GeoJSON", quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=4", "RFC7946=NO"))
# plotly (d3-geo) exige anillos exteriores en sentido horario y huecos en
# antihorario; con el sentido contrario el relleno cubre todo el globo menos el
# polígono. GDAL no lo garantiza, así que se reorientan los anillos aquí.
geo <- jsonlite::fromJSON(destino, simplifyVector = FALSE)
area_firmada <- \(r) {
  x <- vapply(r, `[[`, 0, 1); y <- vapply(r, `[[`, 0, 2)
  sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y) / 2
}
orientar <- \(anillos) Map(\(r, i) {
  horario <- area_firmada(r) < 0
  if (horario == (i == 1)) r else rev(r)
}, anillos, seq_along(anillos))
for (k in seq_along(geo$features)) {
  g <- geo$features[[k]]$geometry
  # GDAL escribe algunas uniones como colección (polígono + restos de líneas y
  # puntos entre departamentos); solo interesan los polígonos
  partes <- if (g$type == "GeometryCollection") g$geometries else list(g)
  pols <- unlist(lapply(partes, \(p) switch(p$type, Polygon = list(p$coordinates),
                                              MultiPolygon = p$coordinates, NULL)), recursive = FALSE)
  stopifnot(length(pols) >= 1)
  geo$features[[k]]$geometry <- list(type = "MultiPolygon", coordinates = lapply(pols, orientar))
}
writeLines(jsonlite::toJSON(geo, auto_unbox = TRUE, digits = NA), destino)
geo <- jsonlite::fromJSON(destino, simplifyVector = FALSE)
for (f in geo$features) for (pol in f$geometry$coordinates) stopifnot(area_firmada(pol[[1]]) < 0)
message("regiones_guatemala.geojson escrito (", round(file.size(destino) / 1024), " KB).")
