# Módulo: vistas de resultados. Consume solo el objeto de resultados (reactivo).

mod_resultados_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("contenido"))
}

mod_resultados_server <- function(id, res) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$contenido <- renderUI({
      if (is.null(res())) {
        return(aviso_vacio("Aún no hay resultados. Marque escenarios en la pestaña «Escenarios» y presione «Simular»."))
      }
      tagList(
      aviso_base(res()),
      accordion(
        open = "equiv",
        accordion_panel("Leyenda de los escenarios", value = "equiv", icon = icon("tags"),
          DT::DTOutput(ns("tbl_equiv")))),
      navset_card_pill(
        id = ns("principal"),
        nav_panel("Resumen",
          card(card_header("Indicadores principales"), DT::DTOutput(ns("tbl_resumen"))),
          p(class = "small text-muted",
            "«Cambio en subsidio» muestra solo el efecto de la reforma tarifaria; «+ compensación» suma además la transferencia. ",
            "Pobreza y desigualdad se calculan sobre el ingreso per cápita del hogar en cada caso.")),
        nav_panel("Pobreza",
          selectInput(ns("tipo_pob"), "Medida de pobreza", choices = TIPOS_POBREZA),
          layout_columns(col_widths = c(6, 6),
            card(card_header("Tasa de pobreza (% de personas)"), plotly::plotlyOutput(ns("g_pob_tasa"), height = "340px")),
            card(card_header("Brecha de pobreza FGT1 (%)"), plotly::plotlyOutput(ns("g_pob_fgt1"), height = "340px"))),
          card(card_header("Cuadro"), DT::DTOutput(ns("tbl_pob")))),
        nav_panel("Desigualdad",
          layout_columns(col_widths = c(6, 6),
            card(card_header("Coeficiente de Gini"), plotly::plotlyOutput(ns("g_gini"), height = "320px")),
            card(card_header("Índice de Palma"), plotly::plotlyOutput(ns("g_palma"), height = "320px"))),
          card(card_header("Indicadores de desigualdad"), DT::DTOutput(ns("tbl_desig"))),
          card(card_header("Concentración y Kakwani por instrumento"), DT::DTOutput(ns("tbl_kak")))),
        nav_panel("Costo fiscal",
          layout_columns(col_widths = c(6, 6),
            card(card_header("Costo anual (millones de Q)"), plotly::plotlyOutput(ns("g_fiscal"), height = "340px")),
            card(card_header("Costo total (% del PIB)"), plotly::plotlyOutput(ns("g_fiscal_pib"), height = "340px"))),
          card(card_header("Cuadro fiscal"), DT::DTOutput(ns("tbl_fiscal")))),
        nav_panel("Incidencia",
          layout_columns(col_widths = c(5, 7),
            radioButtons(ns("vista"), "Unidad", choices = setNames(names(VISTAS_ETIQ), unname(VISTAS_ETIQ))),
            uiOutput(ns("sel_medida"))),
          do.call(navset_pill, c(list(id = ns("grupo")), unname(lapply(names(GRUPOS_ETIQ), \(g) nav_panel(
            GRUPOS_ETIQ[[g]],
            if (g == "reg") card(
              card_header("Mapa por región"),
              selectInput(ns("esc_mapa"), "Escenario del mapa",
                          choices = res()$nombres, selected = res()$nombres[1]),
              plotly::plotlyOutput(ns("mapa_reg"), height = "460px")),
            plotly::plotlyOutput(ns(paste0("g_", g)), height = "380px"),
            DT::DTOutput(ns(paste0("t_", g))))))))),
        nav_panel("Estratos",
          p(class = "small text-muted",
            "Distribución de la población por estrato de ingreso según el ingreso per cápita del hogar."),
          plotly::plotlyOutput(ns("g_estratos"), height = "420px")),
        nav_panel("Tarifas",
          selectInput(ns("esc_tar"), "Escenario", choices = c(SERIE_BASE, res()$nombres), selected = res()$nombres[1]),
          DT::DTOutput(ns("tbl_tar")))
      ))
    })

    # Cuadros: server = FALSE para que se reinicialicen al cambiar el número de columnas
    output$tbl_equiv <- DT::renderDT({ req(res()); dt_tabla(tabla_equivalencias(res())) }, server = FALSE)
    output$tbl_resumen <- DT::renderDT({ req(res()); dt_tabla(tabla_resumen(res())) }, server = FALSE)
    output$tbl_pob <- DT::renderDT({ req(res(), input$tipo_pob); dt_tabla(tabla_pobreza(res(), input$tipo_pob), 3) }, server = FALSE)
    output$tbl_desig <- DT::renderDT({ req(res()); dt_tabla(tabla_desigualdad(res()), 4) }, server = FALSE)
    output$tbl_kak <- DT::renderDT({ req(res()); dt_tabla(tabla_kakwani(res()), 3) }, server = FALSE)
    output$tbl_fiscal <- DT::renderDT({ req(res()); dt_tabla(tabla_fiscal(res()), 2) }, server = FALSE)
    output$tbl_tar <- DT::renderDT({ req(res(), input$esc_tar); dt_tabla(tabla_tarifas(res(), input$esc_tar), 4) }, server = FALSE)

    output$g_pob_tasa <- plotly::renderPlotly({ req(res(), input$tipo_pob); grafico_pobreza(res(), input$tipo_pob, "tasa") })
    output$g_pob_fgt1 <- plotly::renderPlotly({ req(res(), input$tipo_pob); grafico_pobreza(res(), input$tipo_pob, "fgt1") })
    output$g_gini  <- plotly::renderPlotly({ req(res()); grafico_desigualdad(res(), "gini") })
    output$g_palma <- plotly::renderPlotly({ req(res()); grafico_desigualdad(res(), "palma") })
    output$g_fiscal <- plotly::renderPlotly({ req(res()); grafico_fiscal(res()) })
    output$g_fiscal_pib <- plotly::renderPlotly({ req(res()); grafico_costo_pib(res()) })
    output$g_estratos <- plotly::renderPlotly({ req(res()); grafico_estratos(res()) })

    # Incidencia: la medida disponible depende de la unidad elegida
    output$sel_medida <- renderUI({
      cob <- identical(input$vista, "cob")
      selectInput(ns("medida"), "Medida", choices = if (cob) MEDIDAS_COBERTURA else MEDIDAS_MONTO)
    })
    geo_regiones <- NULL
    output$mapa_reg <- plotly::renderPlotly({
      req(res(), input$vista, input$medida, input$esc_mapa)
      if (is.null(geo_regiones)) geo_regiones <<- leer_regiones_geo()
      mapa_region(res(), geo_regiones, input$esc_mapa, input$vista, input$medida)
    })
    for (g in names(GRUPOS_ETIQ)) local({
      g <- g
      output[[paste0("g_", g)]] <- plotly::renderPlotly({
        req(res(), input$vista, input$medida)
        grafico_grupo(res(), g, input$vista, input$medida)
      })
      output[[paste0("t_", g)]] <- DT::renderDT({
        req(res(), input$vista)
        dt_tabla(tabla_grupo(res(), g, input$vista), 2)
      }, server = FALSE)
    })
  })
}
