# App Shiny: impacto fiscal y distributivo del subsidio eléctrico en Guatemala.
# Se lanza desde la raíz del repo con shiny::runApp(). Todas las rutas son relativas.

library(shiny)
library(bslib)

# Capas: configuración -> adaptador (motor de referencia) -> lógica de la app
for (f in c("config.R", "adapter_names.R", "adapter_params.R", "adapter_env.R", "adapter_engine.R",
            "adapter_compose.R", "base_scenario.R", "adapter_results.R", "scenario_store.R",
            "results_views.R", "map_views.R", "ui_helpers.R")) {
  source(file.path("R", f), encoding = "UTF-8", local = FALSE)
}
for (f in c("mod_esquema.R", "mod_compensacion.R", "mod_composer.R", "mod_sesion.R",
            "mod_resultados.R", "mod_simulacion.R")) {
  source(f, encoding = "UTF-8", local = FALSE)
}

PLANTILLA <- tablas_escenario_plantilla()

ui <- page_navbar(
  id = "nav",
  title = tags$span(style = "font-weight:600;", "Microsimulación del subsidio eléctrico · Guatemala"),
  theme = bs_theme(version = 5, bootswatch = "zephyr", primary = "#005A9C", "navbar-bg" = "#0d2137"),
  fillable = FALSE,
  nav_panel("1 · Esquemas tarifarios", icon = icon("bolt"),
            div(class = "container-fluid py-2", mod_esquema_ui("esq"))),
  nav_panel("2 · Compensaciones", icon = icon("hand-holding-dollar"),
            div(class = "container-fluid py-2", mod_compensacion_ui("comp"))),
  nav_panel("3 · Escenarios", icon = icon("layer-group"),
            div(class = "container-fluid py-2", mod_composer_ui("composer"), mod_sesion_ui("sesion"))),
  nav_panel("4 · Simular", icon = icon("chart-line"),
            div(class = "container-fluid py-2", mod_simulacion_ui("sim")))
)

server <- function(input, output, session) {
  raiz <- "."
  guardado <- autosave_leer(raiz)
  estado <- reactiveValues(
    libs        = guardado$libs %||% biblioteca_inicial(PLANTILLA),
    escenarios  = guardado$escenarios %||% list(),
    resultados  = NULL,
    firma       = NULL
  )

  mod_esquema_server("esq", estado, PLANTILLA)
  mod_compensacion_server("comp", estado, PLANTILLA)
  mod_composer_server("composer", estado)
  mod_sesion_server("sesion", estado, raiz)
  mod_simulacion_server("sim", estado, PLANTILLA)

  # Autosave de la biblioteca y los escenarios (con una breve espera para agrupar cambios)
  cambios <- debounce(reactive(list(estado$libs, estado$escenarios)), 1000)
  observeEvent(cambios(), autosave_escribir(raiz, estado$libs, estado$escenarios), ignoreInit = TRUE)
}

shinyApp(ui, server)
