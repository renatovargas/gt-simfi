# Módulo: biblioteca de esquemas tarifarios (componente `sub`).

esq_num <- function(ns, id, step = 0.01) {
  numericInput(ns(id), NULL, value = NA, min = 0, step = step, width = "100%")
}

# Los límites de tramo son comunes a las tres distribuidoras (columnas `block*`),
# pero se muestran en la tabla de cada pestaña: un campo por distribuidora,
# sincronizados entre sí (ver mod_esquema_server()).
esq_id_limite <- function(k, emp) paste0(SUB_CAMPOS_LIMITE[k], "_", emp)

esq_panel_dist <- function(ns, emp, nombre) {
  cam <- sub_campos_dist(emp)
  celda <- \(id, step) tags$td(esq_num(ns, id, step))
  filas <- lapply(seq_len(SUB_N_TRAMOS), \(k) tags$tr(
    tags$td(class = "fw-semibold", k),
    celda(esq_id_limite(k, emp), 1),
    celda(cam$plena[k], 0.0001), celda(cam$objetivo[k], 0.0001), celda(cam$cargo[k], 0.01)))
  nav_panel(
    nombre,
    p(class = "text-muted small mt-2 mb-1",
      "El subsidio por kWh de cada tramo es la tarifa plena (limitada por el tope) menos la ",
      "tarifa objetivo. Con la tarifa objetivo vacía, el tramo no recibe subsidio. ",
      "Los límites de consumo son los mismos para las tres distribuidoras."),
    div(class = "table-responsive",
      tags$table(class = "table table-sm align-middle tabla-esquema mb-2",
        tags$thead(tags$tr(
          tags$th("Tramo"),
          tags$th("Límite de consumo", tags$br(), tags$span(class = "fw-normal", "(desde, kWh por mes)")),
          tags$th("Tarifa plena (Q/kWh)"), tags$th("Tarifa objetivo (Q/kWh)"),
          tags$th("Cargo fijo (Q/mes)"))),
        tags$tbody(filas))),
    layout_columns(
      col_widths = c(3, 3, 6),
      numericInput(ns(cam$tope), "Tarifa tope (Q/kWh)", NA, min = 0, step = 0.0001),
      numericInput(ns(cam$plena_gral), "Tarifa plena general (Q/kWh)", NA, min = 0, step = 0.0001),
      div())
  )
}

mod_esquema_ui <- function(id) {
  ns <- NS(id)
  tagList(
  tags$style(HTML(
    ".tabla-esquema .shiny-input-container { margin-bottom: 0; }
     .tabla-esquema th { vertical-align: bottom; font-size: .875rem; text-transform: none; }
     .tabla-esquema td:first-child { width: 4rem; }")),
  card(
    card_header(div(class = "d-flex justify-content-between align-items-center",
                    "Esquema tarifario", uiOutput(ns("insignia"), inline = TRUE))),
    p(class = "text-muted small",
      "Defina los tramos de consumo y las tarifas por distribuidora. Parta de la tarifa vigente ",
      "o cargue un esquema guardado, modifique los valores y guárdelo con un nombre."),
    barra_biblioteca_ui(ns, "Tarifa vigente (situación actual)",
                        "p. ej. Tarifa social hasta 120 kWh", "Nombre del esquema"),
    do.call(navset_pill, c(
      list(id = ns("dist")),
      unname(Map(\(e, n) esq_panel_dist(ns, e, n), names(SUB_DISTRIBUIDORAS), SUB_DISTRIBUIDORAS))))
  ))
}

mod_esquema_server <- function(id, estado, plantilla) {
  moduleServer(id, function(input, output, session) {
    base <- payload_sub_desde_fila(plantilla$sim_sub[plantilla$sim_sub$sim_sub == 0, ])

    num <- \(id) suppressWarnings(as.numeric(input[[id]] %||% NA_real_))
    emps <- names(SUB_DISTRIBUIDORAS)
    # Los límites se leen de la pestaña visible (las demás se sincronizan con ella)
    emp_visible <- reactive({
      e <- emps[match(input$dist %||% SUB_DISTRIBUIDORAS[1], SUB_DISTRIBUIDORAS)]
      if (is.na(e)) emps[1] else e
    })
    actual <- reactive({
      v <- setNames(lapply(SUB_CAMPOS, num), SUB_CAMPOS)
      for (k in seq_len(SUB_N_TRAMOS)) v[[SUB_CAMPOS_LIMITE[k]]] <- num(esq_id_limite(k, emp_visible()))
      v
    })

    # Un cambio de límite en la pestaña visible se copia a las otras dos. Solo se
    # propaga desde la visible para que una actualización en curso no rebote.
    for (k in seq_len(SUB_N_TRAMOS)) for (e in emps) local({
      k <- k; e <- e
      observeEvent(input[[esq_id_limite(k, e)]], {
        if (!identical(emp_visible(), e)) return()
        v <- num(esq_id_limite(k, e))
        for (o in setdiff(emps, e)) if (!identical(num(esq_id_limite(k, o)), v)) {
          updateNumericInput(session, esq_id_limite(k, o), value = if (is.na(v)) "" else v)
        }
      }, ignoreInit = TRUE, ignoreNULL = FALSE)
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
        ids <- if (c %in% SUB_CAMPOS_LIMITE) esq_id_limite(match(c, SUB_CAMPOS_LIMITE), emps) else c
        for (id in ids) updateNumericInput(session, id, value = if (is.na(v)) "" else v)
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
