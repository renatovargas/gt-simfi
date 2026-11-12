# Pestaña «Instructivo»: cómo se usa la app y cómo se arma un escenario.
# Los diagramas (www/img/) se generan con data-raw/build_instructivo_img.R.

instructivo_ui <- function() {
  div(class = "container-fluid py-2", style = "max-width:1100px;",
    card(
      card_header("Cómo usar el simulador"),
      p(HTML(paste0(
        "La app permite <strong>construir y comparar escenarios del subsidio eléctrico</strong> en ",
        "Guatemala: cambios en el esquema tarifario y transferencias compensatorias, y su efecto en ",
        "la pobreza, la desigualdad y el costo fiscal. El trabajo se organiza en cuatro pantallas."))),
      div(class = "alert alert-primary small mb-3", role = "note",
        HTML(paste0(
          "<strong>Idea central:</strong> un escenario es una <strong>combinación de componentes</strong>. ",
          "Cada esquema tarifario o compensación que usted guarda con un nombre es un ",
          "<strong>componente</strong>; un escenario toma un esquema y una compensación."))),
      tags$img(src = "img/pantallas.png", class = "img-fluid mx-auto d-block", style = "max-height:220px;",
               alt = paste("Cuatro pantallas en secuencia: Esquemas tarifarios y Compensaciones definen",
                           "componentes; Escenarios los combina; Simular muestra los resultados.")),
      tags$ul(class = "list-unstyled mt-3",
        tags$li(tags$strong("1 · Esquemas tarifarios: "),
                "límites de consumo por tramo y tarifas plena y objetivo por distribuidora (EEGSA, DEOCSA, ",
                "DEORSA). Parta de la tarifa vigente, cambie los valores y guarde el esquema con un nombre."),
        tags$li(tags$strong("2 · Compensaciones: "),
                "a quién se compensa (hogares hasta cierto decil, beneficiarios del bono social u hogares ",
                "de bajo consumo) y con qué monto. Es opcional."),
        tags$li(tags$strong("3 · Escenarios: "),
                "cada escenario combina un esquema y una compensación guardados. Marque «Comparar» en los ",
                sprintf("que quiera simular (hasta %d) y use la papelera para eliminar los que no necesite. ", MAX_ESCENARIOS),
                "Aquí también se exporta e importa el trabajo en Excel."),
        tags$li(tags$strong("4 · Simular: "),
                "corre los escenarios marcados junto con la situación base y compara pobreza, desigualdad, ",
                "costo fiscal, incidencia por grupos y tarifas.")),
      p(class = "small text-muted mb-0",
        "Si un escenario no cambia el esquema tarifario («Tarifa vigente») o no tiene compensación ",
        "(«Sin compensación»), esa parte queda como en la situación actual. El trabajo se guarda ",
        "automáticamente en este equipo y se restaura al abrir la app de nuevo.")
    ),
    card(
      class = "mt-3",
      card_header("Cómo se arma un escenario"),
      tags$img(src = "img/escenario.png", class = "img-fluid mx-auto d-block", style = "max-height:420px;",
               alt = paste("Dos columnas de componentes guardados: esquemas tarifarios y compensaciones.",
                           "Dos escenarios eligen un componente distinto en cada columna.")),
      p(class = "small text-muted mt-2 mb-0",
        "Un escenario toma un componente de cada pantalla, y cualquier combinación es válida. ",
        "Los nombres del diagrama son ilustrativos.")
    ),
    card(
      class = "mt-3",
      card_header("Esquema tarifario: cómo se calcula el subsidio"),
      tags$ul(class = "small mb-0",
        tags$li("Subsidio por kWh de cada tramo = tarifa plena (o la tarifa tope, si es menor) − tarifa objetivo."),
        tags$li("Si la tarifa objetivo queda vacía, o es igual o mayor que la tarifa plena, el tramo no recibe ",
                "subsidio y paga la tarifa plena."),
        tags$li("Una tarifa objetivo de 0 no es lo mismo que vacía: da al tramo el subsidio máximo."),
        tags$li("Los montos se expresan en quetzales de 2023, con la población de la ENCOVI 2023."))
    ),
    p(class = "small mt-3",
      "Instructivo completo, instalación y descarga: ",
      tags$a(href = "https://renatovargas.github.io/gt-simfi/instructivo.html", target = "_blank",
             rel = "noopener", "renatovargas.github.io/gt-simfi")),
    div(class = "d-flex justify-content-end pt-3 mt-2 border-top",
        actionButton("comenzar", HTML("Comenzar: 1 · Esquemas tarifarios →"), class = "btn-primary"))
  )
}
