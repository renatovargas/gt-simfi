# Módulo: composer de escenarios nombrados y lista de escenarios guardados.

mod_composer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Componer un escenario"),
      p(class = "text-muted small mb-2",
        "Un escenario combina un esquema tarifario y una compensación de las que guardó. ",
        "Deje “situación actual” o “sin compensación” para no cambiar esa parte."),
      textInput(ns("nombre"), "Nombre del escenario", placeholder = "p. ej. Reforma tarifaria con bono", width = "100%"),
      layout_columns(
        col_widths = c(6, 6),
        selectizeInput(ns("sub"), "Esquema tarifario",
                    choices = c("Tarifa vigente (situación actual)" = REF_SENTINELA), width = "100%",
                    options = list(dropdownParent = "body")),
        selectizeInput(ns("comp"), "Compensación",
                    choices = c("Sin compensación" = REF_SENTINELA), width = "100%",
                    options = list(dropdownParent = "body"))),
      actionButton(ns("agregar"), tagList(icon("plus"), " Agregar escenario a la lista"), class = "btn-primary")
    ),
    card(
      card_header(div(class = "d-flex justify-content-between align-items-center",
                      "Escenarios guardados", uiOutput(ns("contador"), inline = TRUE))),
      p(class = "text-muted small",
        sprintf("Marque hasta %d escenarios para comparar en la simulación. La situación base se incluye siempre.",
                MAX_ESCENARIOS)),
      uiOutput(ns("lista")),
      checkboxGroupInput(ns("comparar"), "Escenarios a comparar", choices = character()),
      layout_columns(
        col_widths = c(8, 4),
        selectizeInput(ns("a_eliminar"), "Eliminar escenario", choices = character(), width = "100%",
                       options = list(dropdownParent = "body")),
        div(class = "pt-4", actionButton(ns("eliminar"), tagList(icon("trash"), " Eliminar"),
                                          class = "btn-outline-danger")))
    )
  )
}

mod_composer_server <- function(id, estado) {
  moduleServer(id, function(input, output, session) {
    nombres <- reactive(vapply(estado$escenarios, `[[`, "", "name"))

    observe({
      s <- isolate(input$sub) %||% REF_SENTINELA
      k <- isolate(input$comp) %||% REF_SENTINELA
      nm_s <- names(estado$libs$sub); nm_k <- names(estado$libs$comp)
      updateSelectInput(session, "sub", selected = if (s %in% c(REF_SENTINELA, nm_s)) s else REF_SENTINELA,
        choices = c("Tarifa vigente (situación actual)" = REF_SENTINELA, setNames(nm_s, nm_s)))
      updateSelectInput(session, "comp", selected = if (k %in% c(REF_SENTINELA, nm_k)) k else REF_SENTINELA,
        choices = c("Sin compensación" = REF_SENTINELA, setNames(nm_k, nm_k)))
    })

    # Mantiene la casilla de comparación y el selector de borrado alineados con el estado
    observe({
      nm <- nombres()
      marcados <- nm[vapply(estado$escenarios, \(e) isTRUE(e$comparar), logical(1))]
      updateCheckboxGroupInput(session, "comparar", choices = nm, selected = marcados)
      updateSelectInput(session, "a_eliminar", choices = nm)
    })

    output$contador <- renderUI({
      n <- sum(vapply(estado$escenarios, \(e) isTRUE(e$comparar), logical(1)))
      tags$span(class = "small text-muted", sprintf("%d de %d marcados para comparar", n, MAX_ESCENARIOS))
    })

    output$lista <- renderUI({
      if (!length(estado$escenarios)) {
        return(p(class = "text-muted fst-italic", "Aún no hay escenarios."))
      }
      filas <- lapply(estado$escenarios, \(e) tags$tr(
        tags$td(e$name),
        tags$td(if (nzchar(normalizar_ref(e$sub))) e$sub else "Situación actual"),
        tags$td(if (nzchar(normalizar_ref(e$comp))) e$comp else "Sin compensación")))
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th("Escenario"), tags$th("Esquema tarifario"), tags$th("Compensación"))),
        tags$tbody(filas))
    })

    observeEvent(input$agregar, {
      nm <- trimws(input$nombre %||% "")
      sub <- normalizar_ref(input$sub); comp <- normalizar_ref(input$comp)
      if (!nzchar(nm)) return(showNotification("Escriba un nombre para el escenario.", type = "warning"))
      if (nm %in% nombres()) return(showNotification("Ya existe un escenario con ese nombre.", type = "warning"))
      if (!nzchar(sub) && !nzchar(comp)) {
        return(showNotification("Elija al menos un esquema o una compensación; de lo contrario sería igual a la base.",
                                type = "warning"))
      }
      n_marcados <- sum(vapply(estado$escenarios, \(e) isTRUE(e$comparar), logical(1)))
      estado$escenarios[[length(estado$escenarios) + 1L]] <-
        list(name = nm, sub = sub, comp = comp, comparar = n_marcados < MAX_ESCENARIOS)
      updateTextInput(session, "nombre", value = "")
      updateSelectInput(session, "sub", selected = REF_SENTINELA)
      updateSelectInput(session, "comp", selected = REF_SENTINELA)
    })

    observeEvent(input$comparar, ignoreNULL = FALSE, {
      sel <- input$comparar %||% character()
      if (length(sel) > MAX_ESCENARIOS) {
        showNotification(sprintf("Se pueden comparar como máximo %d escenarios.", MAX_ESCENARIOS), type = "warning")
        sel <- head(sel, MAX_ESCENARIOS)
        updateCheckboxGroupInput(session, "comparar", selected = sel)
      }
      actual <- vapply(estado$escenarios, \(e) isTRUE(e$comparar), logical(1))
      nuevo <- nombres() %in% sel
      if (length(nuevo) && !identical(unname(actual), nuevo)) {
        for (i in seq_along(estado$escenarios)) estado$escenarios[[i]]$comparar <- nuevo[i]
      }
    })

    observeEvent(input$eliminar, {
      nm <- input$a_eliminar %||% ""
      i <- match(nm, nombres())
      if (is.na(i)) return()
      estado$escenarios[[i]] <- NULL
    })
  })
}
