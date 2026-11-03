# Módulo: biblioteca de esquemas tarifarios (componente `sub`).

esq_num <- function(ns, id, step = 0.01) {
  numericInput(ns(id), NULL, value = NA, min = 0, step = step, width = "100%")
}

esq_panel_dist <- function(ns, emp, nombre) {
  cam <- sub_campos_dist(emp)
  encabezado <- layout_columns(
    col_widths = c(2, 3, 3, 4), class = "fw-semibold small",
    div("Tramo"), div("Tarifa plena (Q/kWh)"), div("Tarifa objetivo (Q/kWh)"), div("Cargo fijo (Q/mes)"))
  filas <- lapply(seq_len(SUB_N_TRAMOS), \(k) layout_columns(
    col_widths = c(2, 3, 3, 4), class = "mb-0",
    div(class = "pt-2", k),
    esq_num(ns, cam$plena[k], 0.0001), esq_num(ns, cam$objetivo[k], 0.0001),
    esq_num(ns, cam$cargo[k], 0.01)))
  nav_panel(
    nombre,
    p(class = "text-muted small mt-2",
      "El subsidio por kWh de cada tramo es la tarifa plena (limitada por el tope) menos la ",
      "tarifa objetivo. Con la tarifa objetivo vacía, el tramo no recibe subsidio."),
    encabezado, filas,
    layout_columns(
      col_widths = c(3, 3, 6), class = "mt-2",
      numericInput(ns(cam$tope), "Tarifa tope (Q/kWh)", NA, min = 0, step = 0.0001),
      numericInput(ns(cam$plena_gral), "Tarifa plena general (Q/kWh)", NA, min = 0, step = 0.0001),
      div())
  )
}

mod_esquema_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(div(class = "d-flex justify-content-between align-items-center",
                    "Esquema tarifario", uiOutput(ns("insignia"), inline = TRUE))),
    p(class = "text-muted small",
      "Defina los tramos de consumo y las tarifas por distribuidora. Parta de la tarifa vigente ",
      "o cargue un esquema guardado, modifique los valores y guárdelo con un nombre."),
    barra_biblioteca_ui(ns, "Tarifa vigente (situación actual)",
                        "p. ej. Tarifa social hasta 120 kWh", "Nombre del esquema"),
    card(
      card_header("Tramos de consumo (límite inferior, kWh por mes)"),
      div(style = "max-width: 260px;",
          lapply(seq_len(SUB_N_TRAMOS), \(k) layout_columns(
            col_widths = c(5, 7), class = "align-items-center mb-1",
            tags$label(class = "fw-semibold mb-0", `for` = ns(SUB_CAMPOS_LIMITE[k]),
                       paste("Tramo", k)),
            numericInput(ns(SUB_CAMPOS_LIMITE[k]), NULL, NA, min = 0, step = 1, width = "100%"))))
    ),
    do.call(navset_pill, c(
      list(id = ns("dist")),
      unname(Map(\(e, n) esq_panel_dist(ns, e, n), names(SUB_DISTRIBUIDORAS), SUB_DISTRIBUIDORAS))))
  )
}

mod_esquema_server <- function(id, estado, plantilla) {
  moduleServer(id, function(input, output, session) {
    base <- payload_sub_desde_fila(plantilla$sim_sub[plantilla$sim_sub$sim_sub == 0, ])

    actual <- reactive({
      setNames(lapply(SUB_CAMPOS, \(c) suppressWarnings(as.numeric(input[[c]] %||% NA_real_))), SUB_CAMPOS)
    })

    observe({
      sel <- isolate(input$cargar) %||% REF_SENTINELA
      updateSelectInput(session, "cargar", selected = sel,
        choices = c("Tarifa vigente (situación actual)" = REF_SENTINELA, setNames(names(estado$libs$sub), names(estado$libs$sub))))
    })

    # La tarifa vigente es la referencia fija: no se puede eliminar
    observe({
      updateActionButton(session, "eliminar",
                         disabled = !nzchar(normalizar_ref(input$cargar %||% REF_SENTINELA)))
    })

    cargar_payload <- function(p) {
      for (c in SUB_CAMPOS) {
        v <- p[[c]] %||% NA_real_
        updateNumericInput(session, c, value = if (is.na(v)) "" else v)
      }
    }

    observeEvent(input$cargar, {
      sel <- normalizar_ref(input$cargar)
      if (!nzchar(sel)) {
        cargar_payload(base)
      } else if (!is.null(estado$libs$sub[[sel]])) {
        cargar_payload(estado$libs$sub[[sel]])
      }
      if (!identical(sel, nombre())) updateTextInput(session, "nombre", value = "")
    })

    nombre <- reactive(trimws(input$nombre %||% ""))

    output$insignia <- renderUI({
      guardado <- estado$libs$sub[[nombre()]]
      est <- if (!nzchar(nombre()) || is.null(guardado)) "sin guardar" else
        if (isTRUE(all.equal(unlist(actual()), unlist(guardado[SUB_CAMPOS]), tolerance = 1e-9,
                             check.attributes = FALSE))) "guardado" else "cambios sin guardar"
      insignia_estado(est)
    })

    observeEvent(input$guardar, {
      nm <- nombre(); p <- actual()
      if (!nzchar(nm)) return(showNotification("Escriba un nombre para el esquema.", type = "warning"))
      lim <- unlist(p[SUB_CAMPOS_LIMITE]); lim <- lim[!is.na(lim)]
      if (!length(lim) || lim[1] != 0 || is.unsorted(lim, strictly = TRUE)) {
        return(showNotification(
          "Los tramos deben empezar en 0 kWh y crecer en orden, sin dejar huecos.", type = "warning"))
      }
      if (!is.null(estado$libs$sub[[nm]])) {
        pendiente(p)
        return(confirmar_reemplazo(session$ns, nm, "un esquema llamado"))
      }
      guardar_esquema(nm, p)
    })

    pendiente <- reactiveVal(NULL)
    guardar_esquema <- function(nm, p) {
      estado$libs$sub[[nm]] <- p
      updateSelectInput(session, "cargar", selected = nm)
      showNotification(sprintf("Esquema «%s» guardado.", nm), type = "message")
    }
    observeEvent(input$confirmar_reemplazo, {
      removeModal()
      if (!is.null(pendiente())) guardar_esquema(nombre(), pendiente())
      pendiente(NULL)
    })

    observeEvent(input$eliminar, {
      nm <- normalizar_ref(input$cargar)
      if (is.null(estado$libs$sub[[nm]])) return(showNotification("Elija en «Cargar guardado» el esquema que desea eliminar.", type = "warning"))
      usados <- vapply(estado$escenarios, \(e) identical(normalizar_ref(e$sub), nm), logical(1))
      if (any(usados)) {
        return(showNotification(sprintf("No se puede eliminar: lo usan los escenarios %s.",
          paste(vapply(estado$escenarios[usados], `[[`, "", "name"), collapse = ", ")), type = "warning"))
      }
      estado$libs$sub[[nm]] <- NULL
      updateSelectInput(session, "cargar", selected = REF_SENTINELA)
      showNotification(sprintf("Esquema «%s» eliminado.", nm), type = "message")
    })
  })
}
