# Utilidades de interfaz compartidas por los módulos.

#' Tabla DT de resultados. Se usa con `renderDT(..., server = FALSE)` para que se
#' reinicialice por completo cuando cambia el número de columnas.
dt_tabla <- function(df, digitos = 2, altura = NULL) {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  t <- DT::datatable(df, rownames = FALSE, class = "compact stripe hover",
                     options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                                    scrollY = altura, ordering = FALSE,
                                    language = list(emptyTable = "Sin datos")))
  if (length(num)) t <- DT::formatRound(t, num, digitos)
  t
}

#' Insignia de estado de un componente: "sin guardar", "guardado", "cambios sin guardar".
insignia_estado <- function(estado) {
  clase <- switch(estado, "guardado" = "text-bg-success", "cambios sin guardar" = "text-bg-warning",
                  "text-bg-secondary")
  tags$span(class = paste("badge", clase), estado)
}

#' Mensaje de "aún no hay resultados".
aviso_vacio <- function(texto) {
  div(class = "text-muted text-center py-5", icon("chart-line", class = "fa-2x mb-2"), tags$br(), texto)
}

#' Aviso cuando la situación base no salió del archivo precalculado vigente.
aviso_base <- function(res) {
  if (!isFALSE(res$base_estado$vigente)) return(NULL)
  div(class = "alert alert-warning py-2 small",
      "La situación base se calculó en esta corrida porque la base precalculada falta o está ",
      "desactualizada. Los resultados son válidos; conviene reconstruir la base para ahorrar tiempo.")
}

#' Barra de biblioteca de un componente. «Cargar» trae los valores de un componente
#' guardado (o de la situación actual); «Actualizar» guarda los cambios sobre el
#' componente cargado y «Guardar como nuevo» crea otro con el nombre que se escriba.
barra_biblioteca_ui <- function(ns, texto_base, ejemplo, etiqueta_nombre) {
  tagList(
    layout_columns(
      col_widths = c(8, 4),
      selectizeInput(ns("cargar"), "Cargar", choices = setNames(REF_SENTINELA, texto_base), width = "100%",
                     options = list(dropdownParent = "body")),
      div(class = "pt-4 d-flex gap-2 justify-content-end",
          actionButton(ns("actualizar"), tagList(icon("rotate"), " Actualizar"), class = "btn-primary",
                       title = "Guardar los cambios en el componente cargado"),
          actionButton(ns("eliminar"), icon("trash"), class = "btn-outline-danger",
                       title = "Eliminar el componente cargado"))),
    layout_columns(
      col_widths = c(8, 4),
      textInput(ns("nombre"), etiqueta_nombre, placeholder = ejemplo, width = "100%"),
      div(class = "pt-4 text-end", actionButton(ns("guardar"), tagList(icon("floppy-disk"), " Guardar como nuevo"),
                                                class = "btn-outline-primary"))))
}

#' Pide confirmación antes de reemplazar un componente guardado con el mismo nombre.
confirmar_reemplazo <- function(ns, nombre, tipo) {
  showModal(modalDialog(
    title = "Reemplazar componente",
    sprintf("Ya existe %s «%s». ¿Desea reemplazarlo con los valores actuales?", tipo, nombre),
    footer = tagList(modalButton("Cancelar"),
                     actionButton(ns("confirmar_reemplazo"), "Reemplazar", class = "btn-primary")),
    easyClose = TRUE))
}

#' Aviso de paquetes faltantes con la instrucción para instalarlos.
aviso_paquetes <- function(faltan) {
  cmd <- sprintf("install.packages(c(%s))", paste0('"', faltan, '"', collapse = ", "))
  modalDialog(
    title = "Faltan paquetes de R",
    p("La simulación no puede correr porque estos paquetes no están instalados:"),
    tags$p(tags$strong(paste(faltan, collapse = ", "))),
    p("Cierre la app, ejecute en la consola de R la instrucción siguiente y vuelva a abrirla:"),
    tags$pre(cmd),
    easyClose = TRUE, footer = modalButton("Entendido"))
}
