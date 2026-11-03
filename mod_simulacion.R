# Módulo: ejecución de la simulación y contenedor de resultados.
# A futuro la corrida podrá pasar a shiny::ExtendedTask + mirai; hoy es síncrona
# y solo llama a ejecutar_escenarios() (que usa run_scenario()).

mod_simulacion_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Ejecución"),
      layout_columns(
        col_widths = c(4, 8),
        div(actionButton(ns("run"), tagList(icon("play"), " Simular"), class = "btn-primary w-100"),
            p(class = "small text-muted mt-1 mb-0", "Corre los escenarios marcados y la situación base.")),
        div(uiOutput(ns("mensaje")),
            div(class = "small text-muted mt-1", tags$strong("En cola: "), uiOutput(ns("cola"), inline = TRUE)))
      )
    ),
    mod_resultados_ui(ns("res"))
  )
}

mod_simulacion_server <- function(id, estado, plantilla) {
  moduleServer(id, function(input, output, session) {
    cache <- new.env()   # corridas ya calculadas (clave: hash de las tablas del motor)

    seleccion <- reactive(Filter(\(e) isTRUE(e$comparar), estado$escenarios))
    firma_actual <- reactive({
      sel <- seleccion()
      rlang::hash(list(sel, lapply(sel, \(e) list(estado$libs$sub[[normalizar_ref(e$sub)]],
                                                  estado$libs$comp[[normalizar_ref(e$comp)]]))))
    })

    output$cola <- renderUI({
      nm <- vapply(seleccion(), `[[`, "", "name")
      if (length(nm)) paste(nm, collapse = ", ") else tags$em("ningún escenario marcado")
    })

    output$mensaje <- renderUI({
      if (is.null(estado$resultados)) return(NULL)
      if (!identical(estado$firma, firma_actual())) {
        tags$span(class = "badge text-bg-warning",
                  "Los resultados no corresponden a la configuración actual; vuelva a simular.")
      } else {
        tags$span(class = "badge text-bg-success", "Resultados al día")
      }
    })

    observeEvent(input$run, {
      sel <- seleccion()
      if (!length(sel)) {
        return(showNotification("Marque al menos un escenario en la pestaña «Escenarios».", type = "warning"))
      }
      err <- validar_escenarios(sel, estado$libs)
      if (length(err)) return(showNotification(paste(err, collapse = " "), type = "error"))

      # Evita que se vean resultados viejos durante y después de una corrida fallida
      estado$resultados <- NULL
      estado$firma <- NULL
      firma <- firma_actual()

      res <- withProgress(message = "Simulando…", value = 0, {
        tryCatch(
          ejecutar_escenarios(sel, estado$libs, plantilla, cache = cache,
            progreso = \(i, n, nombre) setProgress((i - 1) / n, detail = sprintf("%s (%d de %d)", nombre, i, n))),
          error = \(e) { message("Error de simulación: ", conditionMessage(e)); NULL })
      })
      if (is.null(res)) {
        showNotification("No se pudo completar la simulación. Revise los valores del esquema y la compensación.",
                         type = "error", duration = 10)
      } else {
        estado$resultados <- res
        estado$firma <- firma
      }
    })

    mod_resultados_server("res", reactive(estado$resultados))
  })
}
