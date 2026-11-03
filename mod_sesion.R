# Módulo: guardar y compartir (Excel), importar y nueva sesión.

mod_sesion_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Guardar y compartir"),
    p(class = "text-muted small mb-2",
      "El archivo Excel contiene una hoja con los esquemas, otra con las compensaciones y otra con los ",
      "escenarios; puede editarlo y volver a cargarlo. El trabajo también se guarda automáticamente en este equipo."),
    layout_columns(
      col_widths = c(6, 6),
      downloadButton(ns("exportar"), "Exportar biblioteca y escenarios (Excel)", class = "w-100 btn-outline-secondary"),
      fileInput(ns("importar"), "Importar (.xlsx)", accept = ".xlsx", width = "100%",
                buttonLabel = "Examinar…", placeholder = "Ningún archivo")),
    hr(class = "my-2"),
    actionButton(ns("nueva"), tagList(icon("trash"), " Nueva sesión (borrar todo)"), class = "btn-outline-danger"),
    tags$span(class = "small text-muted ms-2", "Borra esquemas, compensaciones, escenarios y resultados.")
  )
}

mod_sesion_server <- function(id, estado, raiz = ".") {
  moduleServer(id, function(input, output, session) {
    output$exportar <- downloadHandler(
      filename = \() paste0("biblioteca_subsidio_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
      content = \(file) guardar_xlsx(file, estado$libs, estado$escenarios)
    )

    observeEvent(input$importar, {
      r <- tryCatch(leer_xlsx(input$importar$datapath), error = \(e) e)
      if (inherits(r, "error")) {
        return(showNotification(conditionMessage(r), type = "error"))
      }
      # Se descartan escenarios que citan componentes inexistentes
      ok <- vapply(r$escenarios, \(e) length(validar_escenarios(list(e), r$libs)) == 0, logical(1))
      if (any(!ok)) showNotification(sprintf("Se omitieron %d escenario(s) con componentes inexistentes.", sum(!ok)),
                                     type = "warning")
      esc <- r$escenarios[ok]
      marcados <- 0L
      for (i in seq_along(esc)) {
        esc[[i]]$comparar <- esc[[i]]$comparar && marcados < MAX_ESCENARIOS
        marcados <- marcados + esc[[i]]$comparar
      }
      estado$libs <- r$libs
      estado$escenarios <- esc
      estado$resultados <- NULL
      showNotification(sprintf("Importados %d esquemas, %d compensaciones y %d escenarios.",
        length(r$libs$sub), length(r$libs$comp), length(esc)), type = "message")
    })

    observeEvent(input$nueva, {
      showModal(modalDialog(
        title = "Nueva sesión",
        "Se borrarán los esquemas, compensaciones, escenarios y resultados. ¿Desea continuar?",
        footer = tagList(modalButton("Cancelar"),
                         actionButton(session$ns("confirmar_nueva"), "Borrar todo", class = "btn-danger"))))
    })

    observeEvent(input$confirmar_nueva, {
      removeModal()
      estado$libs <- list(sub = list(), comp = list())
      estado$escenarios <- list()
      estado$resultados <- NULL
      estado$firma <- NULL
      autosave_borrar(raiz)
      showNotification("Sesión nueva.", type = "message")
    })
  })
}
