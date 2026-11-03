# Módulo: biblioteca de compensaciones (componente `comp`).

# Opción por defecto: sin compensación (situación de referencia)
COMP_GRUPOS_UI <- c("Sin compensación" = 0L, COMP_GRUPOS)

mod_compensacion_ui <- function(id) {
  ns <- NS(id)
  cond <- \(campo, valor) sprintf("input['%s'] == '%s'", ns(campo), valor)
  card(
    card_header(div(class = "d-flex justify-content-between align-items-center",
                    "Transferencia compensatoria", uiOutput(ns("insignia"), inline = TRUE))),
    p(class = "text-muted small",
      "Defina a quién se compensa y con qué monto. Los montos son por hogar (se pagan al jefe de hogar)."),
    barra_biblioteca_ui(ns, "Sin compensación", "p. ej. Bono a los 3 primeros deciles",
                        "Nombre de la compensación"),
    selectizeInput(ns("grupo_com"), "¿A quién se compensa?", choices = COMP_GRUPOS_UI,
                   selected = 0L, width = "100%", options = list(dropdownParent = "body")),
    conditionalPanel(cond("grupo_com", "1"),
      layout_columns(col_widths = c(6, 6),
        selectizeInput(ns("decil_com"), "Hasta el decil de ingreso", choices = 1:10, selected = 2,
                      options = list(dropdownParent = "body")),
        numericInput(ns("valor_com_1"), "Monto mensual por hogar (Q)", 500, min = 0, step = 10))),
    conditionalPanel(cond("grupo_com", "2"),
      radioButtons(ns("metodo_com"), "Cálculo del monto", choices = COMP_METODOS_BONO, inline = TRUE),
      conditionalPanel(cond("metodo_com", "1"),
        numericInput(ns("valor_com_2"), "Monto mensual por hogar (Q)", 500, min = 0, step = 10)),
      conditionalPanel(cond("metodo_com", "2"),
        numericInput(ns("pct_com"), "Porcentaje de la mediana anual del bono", 100, min = 0, step = 5))),
    conditionalPanel(cond("grupo_com", "3"),
      layout_columns(col_widths = c(6, 6),
        numericInput(ns("elect_com"), "Consumo menor a (kWh por mes)", 50, min = 1, step = 1),
        numericInput(ns("valor_com_3"), "Monto mensual por hogar (Q)", 500, min = 0, step = 10)))
  )
}

mod_compensacion_server <- function(id, estado, plantilla) {
  moduleServer(id, function(input, output, session) {
    # El monto tiene una entrada por grupo (ids distintos); el payload guarda uno solo.
    actual <- reactive({
      g <- suppressWarnings(as.numeric(input$grupo_com %||% 0))
      num <- \(x) suppressWarnings(as.numeric(x %||% NA_real_))
      p <- list(
        grupo_com = g,
        metodo_com = if (g == 2) num(input$metodo_com) else NA_real_,
        valor_com = num(input[[paste0("valor_com_", g)]]),
        pct_com = num(input$pct_com),
        decil_com = num(input$decil_com),
        elect_com = num(input$elect_com))
      normalizar_comp(p)
    })

    observe({
      sel <- isolate(input$cargar) %||% REF_SENTINELA
      updateSelectInput(session, "cargar", selected = sel,
        choices = c("Sin compensación" = REF_SENTINELA, setNames(names(estado$libs$comp), names(estado$libs$comp))))
    })

    # "Sin compensación" no es un componente guardado: no hay nada que eliminar
    observe({
      updateActionButton(session, "eliminar",
                         disabled = !nzchar(normalizar_ref(input$cargar %||% REF_SENTINELA)))
    })

    observeEvent(input$cargar, {
      sel <- normalizar_ref(input$cargar)
      if (!identical(sel, nombre())) updateTextInput(session, "nombre", value = "")
      if (!nzchar(sel)) return(updateSelectInput(session, "grupo_com", selected = 0L))
      p <- estado$libs$comp[[sel]]
      if (is.null(p)) return()
      g <- p$grupo_com
      updateSelectInput(session, "grupo_com", selected = g)
      if (!is.na(p$metodo_com)) updateRadioButtons(session, "metodo_com", selected = p$metodo_com)
      if (!is.na(p$valor_com)) updateNumericInput(session, paste0("valor_com_", g), value = p$valor_com)
      if (!is.na(p$pct_com)) updateNumericInput(session, "pct_com", value = p$pct_com)
      if (!is.na(p$decil_com)) updateSelectInput(session, "decil_com", selected = p$decil_com)
      if (!is.na(p$elect_com)) updateNumericInput(session, "elect_com", value = p$elect_com)
    })

    nombre <- reactive(trimws(input$nombre %||% ""))

    output$insignia <- renderUI({
      guardado <- estado$libs$comp[[nombre()]]
      est <- if (!nzchar(nombre()) || is.null(guardado)) "sin guardar" else
        if (isTRUE(all.equal(unlist(actual()), unlist(guardado[COMP_CAMPOS]), tolerance = 1e-9,
                             check.attributes = FALSE))) "guardado" else "cambios sin guardar"
      insignia_estado(est)
    })

    observeEvent(input$guardar, {
      nm <- nombre(); p <- actual()
      if (!nzchar(nm)) return(showNotification("Escriba un nombre para la compensación.", type = "warning"))
      if (identical(p$grupo_com, 0)) {
        return(showNotification("Elija a quién se compensa; «Sin compensación» ya existe como referencia.",
                                type = "warning"))
      }
      necesarios <- switch(as.character(p$grupo_com),
        "1" = c("valor_com", "decil_com"),
        "2" = c("metodo_com", if (identical(p$metodo_com, 2)) "pct_com" else "valor_com"),
        "3" = c("valor_com", "elect_com"))
      if (anyNA(unlist(p[necesarios])) || any(unlist(p[necesarios]) < 0)) {
        return(showNotification("Complete los valores de la compensación (no pueden ser negativos).", type = "warning"))
      }
      if (identical(p$grupo_com, 1) && !p$decil_com %in% 1:10) {
        return(showNotification("El decil debe ser un entero entre 1 y 10.", type = "warning"))
      }
      if (!is.null(estado$libs$comp[[nm]])) {
        pendiente(p)
        return(confirmar_reemplazo(session$ns, nm, "una compensación llamada"))
      }
      guardar_comp(nm, p)
    })

    pendiente <- reactiveVal(NULL)
    guardar_comp <- function(nm, p) {
      estado$libs$comp[[nm]] <- p
      updateSelectInput(session, "cargar", selected = nm)
      showNotification(sprintf("Compensación «%s» guardada.", nm), type = "message")
    }
    observeEvent(input$confirmar_reemplazo, {
      removeModal()
      if (!is.null(pendiente())) guardar_comp(nombre(), pendiente())
      pendiente(NULL)
    })

    observeEvent(input$eliminar, {
      nm <- normalizar_ref(input$cargar)
      if (is.null(estado$libs$comp[[nm]])) return(showNotification("Elija en «Cargar guardado» la compensación que desea eliminar.", type = "warning"))
      usados <- vapply(estado$escenarios, \(e) identical(normalizar_ref(e$comp), nm), logical(1))
      if (any(usados)) {
        return(showNotification(sprintf("No se puede eliminar: la usan los escenarios %s.",
          paste(vapply(estado$escenarios[usados], `[[`, "", "name"), collapse = ", ")), type = "warning"))
      }
      estado$libs$comp[[nm]] <- NULL
      updateSelectInput(session, "cargar", selected = REF_SENTINELA)
      showNotification(sprintf("Compensación «%s» eliminada.", nm), type = "message")
    })
  })
}
