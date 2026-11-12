# Interruptores permanentes de la app. Espejo de `OPC` del Master del colega
# (mismos nombres, para que sus scripts los encuentren sin cambios).
# Se descartan los campos que no aplican a la app: `adm_file` (solo calibración)
# y `abrir_excel` (solo el script 05).

OPC <- list(
  run_diag   = 1,        # 1 = diagnósticos en consola (el adaptador los captura)
  semilla    = 20231,    # sorteos de kWh (solo auditoría; los sorteos están congelados)
  M          = 10,       # número de imputaciones de kWh
  ppp_fac    = 4.88,     # factor PPA (VERIFICAR con el colega, igual que en su Master)
  est_pext   = 2.15,     # umbrales de estratos (USD PPA por persona por día)
  est_pob    = 6.85,
  est_vul    = 14,
  pline_int  = 6.85,     # línea internacional de la 3a tabla de pobreza
  tol_recibe = 0.01,     # Q anuales: umbral de "recibe / pierde / gana"
  shp_deptos = "data/gis/departamentos_guatemala.json",  # geojson (reemplaza al shapefile)
  shp_codcol = "cod_dep" # código de departamento en el geojson
)

# Límite de escenarios de reforma a comparar (más el escenario 0 de base).
MAX_ESCENARIOS <- 4

# Raíz de la app. Las rutas son relativas a la raíz; las pruebas (que corren en
# tests/testthat/) fijan esta opción desde su helper.
ruta_app <- function(...) {
  file.path(getOption("gtsimfi.raiz", "."), ...)
}

# Paquetes de la app (los del motor del colega están en REF_PAQUETES, adapter_names.R).
# El sitio (index.qmd) documenta esta lista; no se instalan en tiempo de ejecución.
PAQUETES_APP <- c("shiny", "bslib", "DT", "plotly", "htmltools", "jsonlite", "rlang",
                  "readxl", "openxlsx", "janitor", "testthat")
# Solo para construir datos (data-raw/): sf, dplyr.

#' Paquetes necesarios (app y motor) que no están instalados en este equipo.
paquetes_faltantes <- function() {
  todos <- setdiff(unique(c(PAQUETES_APP, if (exists("REF_PAQUETES")) REF_PAQUETES)), "testthat")
  todos[!vapply(todos, requireNamespace, logical(1), quietly = TRUE)]
}
