# Módulo: biblioteca de esquemas tarifarios (componente `sub`).

esq_num <- function(ns, id, step = 0.01) {
  numericInput(ns(id), NULL, value = NA, min = 0, step = step, width = "100%")
}

# Los límites de tramo son comunes a las tres distribuidoras (columnas `block*`),
# pero se muestran en la tabla de cada pestaña: un campo por distribuidora,
# sincronizados entre sí (ver mod_esquema_server()).
esq_id_limite <- function(k, emp) paste0(SUB_CAMPOS_LIMITE[k], "_", emp)
esq_id_dif <- function(k, emp) paste0("dif_", emp, "_", k)

#' Diferencia tope - objetivo para mostrar (vacía si falta alguno de los dos).
esq_fmt_dif <- function(tope, objetivo) {
  if (is.na(tope) || is.na(objetivo)) return("")
  sub("\\.?0+$", "", sprintf("%.6f", tope - objetivo))
}

esq_panel_dist <- function(ns, emp, nombre) {
  cam <- sub_campos_dist(emp)
  celda <- \(id, step) tags$td(esq_num(ns, id, step))
  filas <- lapply(seq_len(SUB_N_TRAMOS), \(k) tags$tr(
    tags$td(class = "fw-semibold", k),
    celda(esq_id_limite(k, emp), 1),
    celda(cam$plena[k], 0.0001), celda(cam$objetivo[k], 0.0001),
    tags$td(class = "text-muted text-end", textOutput(ns(esq_id_dif(k, emp)), inline = TRUE)),
    celda(cam$cargo[k], 0.01)))
  nav_panel(
    nombre,
    p(class = "text-muted small mt-2 mb-1",
      "Aporte social por kWh de cada tramo = tarifa plena (o la tarifa tope, si es menor) \u2212 tarifa objetivo. ",
      "El hogar paga por kWh la tarifa plena menos ese aporte social."),
    tags$ul(class = "text-muted small mb-2",
      tags$li("Si la tarifa objetivo queda vacía, o es igual o mayor que la tarifa plena, el tramo no ",
              "recibe aporte social y paga la tarifa plena."),
      tags$li("Vacío no es lo mismo que cero: una tarifa objetivo de 0 da al tramo el aporte social máximo ",
              "(la tarifa plena, o la tarifa tope si es menor)."),
      tags$li("La columna «Aporte social» (tope \u2212 objetivo) se calcula sola y es solo de referencia: es el aporte por kWh ",
              "cuando la tarifa plena es igual o mayor que la tope."),
      tags$li("Los límites de consumo son los mismos para las tres distribuidoras.")),
    div(class = "table-responsive",
      tags$table(class = "table table-sm align-middle tabla-esquema mb-2",
        tags$thead(tags$tr(
          tags$th("Tramo"),
          tags$th("Límite de consumo", tags$br(), tags$span(class = "fw-normal", "(desde, kWh por mes)")),
          tags$th("Tarifa plena (Q/kWh)"), tags$th("Tarifa objetivo (Q/kWh)"),
          tags$th(class = "text-end", "Aporte social", tags$br(),
                  tags$span(class = "fw-normal", "Tope \u2212 objetivo", tags$br(), "(Q/kWh, referencia)")),
          tags$th("Cargo fijo (Q/mes)"))),
        tags$tbody(filas))),
    layout_columns(
      col_widths = c(3, 9),
      numericInput(ns(cam$tope), "Tarifa tope (Q/kWh)", NA, min = 0, step = 0.0001),
      p(class = "text-muted small pt-4 mb-0",
        "Límite máximo del aporte social por kWh en todos los tramos de esta distribuidora."))
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
      "Defina los tramos de consumo y las tarifas por distribuidora. Cargue la tarifa vigente o un esquema ",
      "guardado y modifique los valores. «Actualizar» guarda los cambios en el esquema cargado; ",
      "«Guardar como nuevo» crea otro esquema con el nombre que escriba."),
    barra_biblioteca_ui(ns, "Tarifa vigente (situación actual)",
                        "p. ej. Tarifa social hasta 120 kWh", "Nombre del nuevo esquema"),
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
    # La tarifa plena general no se edita en pantalla: se conserva la del
    # esquema cargado (la tarifa vigente por defecto).
    campos_ocultos <- vapply(names(SUB_DISTRIBUIDORAS), \(e) sub_campos_dist(e)$plena_gral, "")
    ocultos <- reactiveVal(base[campos_ocultos])

    actual <- reactive({
      v <- setNames(lapply(SUB_CAMPOS, num), SUB_CAMPOS)
      v[campos_ocultos] <- ocultos()
      for (k in seq_len(SUB_N_TRAMOS)) v[[SUB_CAMPOS_LIMITE[k]]] <- num(esq_id_limite(k, emp_visible()))
      v
    })

    # Diferencia tope - objetivo por tramo (solo referencia, no editable)
    for (k in seq_len(SUB_N_TRAMOS)) for (e in emps) local({
      k <- k; e <- e; cam <- sub_campos_dist(e)
      output[[esq_id_dif(k, e)]] <- renderText(esq_fmt_dif(num(cam$tope), num(cam$objetivo[k])))
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

    # Selección que debe quedar tras guardar o eliminar. Se aplica junto con la
    # nueva lista de opciones para que la actualización de la lista no la pise.
    a_seleccionar <- reactiveVal(NULL)
    seleccionar <- function(nm) { a_seleccionar(nm); updateSelectInput(session, "cargar", selected = nm) }

    observe({
      sel <- isolate(a_seleccionar()) %||% isolate(input$cargar) %||% REF_SENTINELA
      isolate(a_seleccionar(NULL))
      updateSelectInput(session, "cargar", selected = sel,
        choices = c("Tarifa vigente (situación actual)" = REF_SENTINELA, setNames(names(estado$libs$sub), names(estado$libs$sub))))
    })

    # La tarifa vigente es la referencia fija: no se puede eliminar
    observe({
      ref <- !nzchar(normalizar_ref(input$cargar %||% REF_SENTINELA))
      updateActionButton(session, "eliminar", disabled = ref)
      updateActionButton(session, "actualizar", disabled = ref)
    })

    cargar_payload <- function(p) {
      ocultos(lapply(setNames(campos_ocultos, campos_ocultos), \(c) p[[c]] %||% base[[c]]))
      for (c in setdiff(SUB_CAMPOS, campos_ocultos)) {
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
      updateTextInput(session, "nombre", value = "")
    })

    nombre <- reactive(trimws(input$nombre %||% ""))

    output$insignia <- renderUI({
      ref <- if (nzchar(nombre())) nombre() else normalizar_ref(input$cargar)
      guardado <- if (nzchar(ref)) estado$libs$sub[[ref]]
      est <- if (is.null(guardado)) "sin guardar" else
        if (isTRUE(all.equal(unlist(actual()), unlist(guardado[SUB_CAMPOS]), tolerance = 1e-9,
                             check.attributes = FALSE))) "guardado" else "cambios sin guardar"
      insignia_estado(est)
    })

    observeEvent(input$guardar, {
      nm <- nombre(); p <- actual()
      if (!nzchar(nm)) return(showNotification("Escriba un nombre para el esquema.", type = "warning"))
      err <- validar_esquema(p)
      if (length(err)) return(showNotification(paste(err, collapse = " "), type = "warning", duration = 8))
      if (!is.null(estado$libs$sub[[nm]])) {
        pendiente(p)
        return(confirmar_reemplazo(session$ns, nm, "un esquema llamado"))
      }
      guardar_esquema(nm, p)
    })

    pendiente <- reactiveVal(NULL)
    guardar_esquema <- function(nm, p) {
      estado$libs$sub[[nm]] <- p
      seleccionar(nm)
      showNotification(sprintf("Esquema «%s» guardado.", nm), type = "message")
    }
    observeEvent(input$confirmar_reemplazo, {
      removeModal()
      if (!is.null(pendiente())) guardar_esquema(nombre(), pendiente())
      pendiente(NULL)
    })

    observeEvent(input$actualizar, {
      nm <- normalizar_ref(input$cargar); p <- actual()
      if (is.null(estado$libs$sub[[nm]])) {
        return(showNotification("Cargue primero el esquema guardado que desea actualizar.", type = "warning"))
      }
      err <- validar_esquema(p)
      if (length(err)) return(showNotification(paste(err, collapse = " "), type = "warning", duration = 8))
      estado$libs$sub[[nm]] <- p
      showNotification(sprintf("Esquema «%s» actualizado.", nm), type = "message")
    })

    observeEvent(input$eliminar, {
      nm <- normalizar_ref(input$cargar)
      if (is.null(estado$libs$sub[[nm]])) return(showNotification("Cargue primero el esquema guardado que desea eliminar.", type = "warning"))
      usados <- vapply(estado$escenarios, \(e) identical(normalizar_ref(e$sub), nm), logical(1))
      if (any(usados)) {
        return(showNotification(sprintf("No se puede eliminar: lo usan los escenarios %s.",
          paste(vapply(estado$escenarios[usados], `[[`, "", "name"), collapse = ", ")), type = "warning"))
      }
      estado$libs$sub[[nm]] <- NULL
      seleccionar(REF_SENTINELA)
      showNotification(sprintf("Esquema «%s» eliminado.", nm), type = "message")
    })
  })
}
