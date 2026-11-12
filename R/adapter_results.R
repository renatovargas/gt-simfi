# Resultados del motor -> objeto de resultados de la app (data frames largos).
# Es el único lugar que conoce la forma de `GTM_resumen` (posiciones de filas y
# columnas); las vistas trabajan solo con las claves que se asignan aquí.

ING_ETIQUETA <- c("Base", "Cambio en subsidio", "Con compensación")

CLAVES_POB    <- c("tasa", "cambio", "nuevos", "fgt1", "fgt2", "brecha_todos",
                   "brecha_pobres", "brecha_nuevos", "brecha_nuevos_pib")
CLAVES_DESIG  <- c("gini", "d_gini", "palma", "theil")
CLAVES_FIS    <- c("costo_base", "costo_esc", "d_costo", "costo_comp", "costo_total",
                   "d_costo_total", "hog_base", "hog_esc", "hog_comp", "comp_prom",
                   "sub_prom_base", "sub_prom_esc", "pib_base", "pib_esc", "pib_total",
                   "aporte_inde", "ratio_inde")
CLAVES_INSTR  <- c("subsidio", "compensacion", "total")
CLAVES_KAK    <- c("ic_base", "kw_base", "ic_esc", "kw_esc")
CLAVES_GRUPO  <- c("sub_base", "sub_esc", "comp", "sub_comp", "d_sub", "neto")
CLAVES_COB    <- c("rec_base", "rec_esc", "pierde", "gana", "rec_comp", "poblacion")
CLAVES_EST    <- c("base", "esc_sub", "esc_comp", "d_sub", "d_comp")
VISTAS_GRUPO  <- c("mil", "inc", "con", "pib", "cob")

SERIE_BASE <- "Base"

#' Matriz con dimnames -> data frame largo con claves.
largo_matriz <- function(m, claves_fila, claves_col = NULL) {
  stopifnot(nrow(m) == length(claves_fila))
  n <- nrow(m); p <- ncol(m)
  d <- data.frame(
    clave     = rep(claves_fila, times = p),
    indicador = rep(rownames(m) %||% claves_fila, times = p),
    col       = rep(seq_len(p), each = n),
    valor     = as.vector(m),
    stringsAsFactors = FALSE
  )
  if (!is.null(claves_col)) d$clave_col <- claves_col[d$col]
  d
}

#' Combina los `resumen$escenario_1` de corridas independientes en el objeto de
#' resultados de la app.
#'
#' @param corridas lista de `list(resumen = <GTM_resumen de un escenario>)`.
#' @param nombres nombres de escenario (únicos), en el mismo orden.
#' @param base corrida de la situación base (`leer_base_scenario()`); de ahí salen
#'   las columnas "Base". Si es NULL se usan las de la primera corrida.
#' @param descripciones data frame con `esquema` y `compensacion` (una fila por
#'   escenario) para la tabla de equivalencias.
#' @param con_compensacion lógico, uno por escenario. En los escenarios sin
#'   compensación se omiten las columnas y series «Con compensación» (repetirían
#'   «Cambio en subsidio»). Por defecto se conservan todas.
combinar_resultados <- function(corridas, nombres, base = NULL, descripciones = NULL,
                                con_compensacion = rep(TRUE, length(nombres))) {
  stopifnot(length(corridas) == length(nombres), !anyDuplicated(nombres),
            length(con_compensacion) == length(nombres))
  sin_comp <- nombres[!con_compensacion]
  es <- lapply(corridas, \(c) c$resumen$escenario_1)
  r1 <- corridas[[1]]$resumen
  corrida_base <- base %||% corridas[[1]]
  es_base <- corrida_base$resumen$escenario_1

  serie <- \(nombre, col) ifelse(col == 1L, SERIE_BASE,
                                 paste0("Escenario ", match(nombre, nombres), ": ", tolower(ING_ETIQUETA[col])))
  # Apila por escenario; la columna "Base" sale de la corrida de la base.
  apilar <- \(extractor, con_ingreso = TRUE) {
    d <- do.call(rbind, lapply(seq_along(es), \(i) {
      x <- extractor(es[[i]])
      x$escenario <- nombres[i]
      x$orden <- i
      x
    }))
    if (con_ingreso) {
      b <- extractor(es_base)
      b$escenario <- nombres[1]
      b$orden <- 1L
      d <- rbind(b[b$col == 1L, , drop = FALSE], d[d$col != 1L, , drop = FALSE])
      d <- d[!(d$col == 3L & d$escenario %in% sin_comp), , drop = FALSE]
      d$ingreso <- factor(ING_ETIQUETA[d$col], levels = ING_ETIQUETA)
      d$serie <- serie(d$escenario, d$col)
      d$escenario <- ifelse(d$col == 1L, SERIE_BASE, d$escenario)
    }
    rownames(d) <- NULL
    d
  }

  pobreza <- do.call(rbind, Map(\(tipo, campo) {
    d <- apilar(\(e) largo_matriz(e[[campo]], CLAVES_POB))
    d$tipo <- tipo
    d
  }, c("general", "extrema", "internacional"), c("pov_gral", "pov_ext", "pov_int")))
  rownames(pobreza) <- NULL

  desigualdad <- apilar(\(e) largo_matriz(e$ineq, CLAVES_DESIG))

  kakwani <- apilar(\(e) {
    d <- largo_matriz(t(e$kakwani), CLAVES_KAK)   # filas = columnas originales
    d$clave_instr <- CLAVES_INSTR[d$col]
    d$instrumento <- rownames(e$kakwani)[d$col]
    d
  }, con_ingreso = FALSE)
  kakwani <- kakwani[!(kakwani$clave_instr == "compensacion" & kakwani$escenario %in% sin_comp), ]

  fiscal <- apilar(\(e) data.frame(
    clave = CLAVES_FIS, indicador = names(e$fis), valor = unname(e$fis),
    stringsAsFactors = FALSE), con_ingreso = FALSE)

  estratos <- apilar(\(e) {
    d <- largo_matriz(e$mov, c("pobre_ext", "pobre", "vulnerable", "clase_media", "total"))
    d$estrato <- d$indicador
    d$clave_col <- CLAVES_EST[d$col]
    d$columna <- colnames(e$mov)[d$col]
    d
  }, con_ingreso = FALSE)
  estratos <- estratos[!(estratos$clave_col %in% c("esc_comp", "d_comp") &
                           estratos$escenario %in% sin_comp), ]

  # Grupos: una fila por escenario x grupo x vista x categoría x medida
  grupos <- apilar(\(e) {
    do.call(rbind, lapply(names(e$grupos), \(g) {
      do.call(rbind, lapply(VISTAS_GRUPO, \(v) {
        t <- as.data.frame(e$grupos[[g]][[v]])
        claves <- if (v == "cob") CLAVES_COB else CLAVES_GRUPO
        medidas <- names(t)[-1]
        stopifnot(length(medidas) == length(claves))
        data.frame(
          grupo = g, vista = v,
          categoria = rep(t[[1]], times = length(claves)),
          clave = rep(claves, each = nrow(t)),
          medida = rep(medidas, each = nrow(t)),
          valor = unlist(t[-1], use.names = FALSE),
          stringsAsFactors = FALSE)
      }))
    }))
  }, con_ingreso = FALSE)

  # Sexo del jefe: los datos codifican 0 = Hombre, 1 = Mujer, pero el pipeline etiqueta
  # c("1" = "Hombre", "2" = "Mujer"). Se corrige aquí, sin tocar reference/.
  grupos <- corregir_sexo_jefe(grupos)

  tar_cols <- c(distribuidora = "Distribuidora", tramo = "Tramo", desde = "Desde (kWh)",
                hasta = "Hasta (kWh)", cargo_fijo = "Cargo fijo (Q/mes)",
                plena = "Tarifa plena (Q/kWh)", tope = "Tarifa tope (Q/kWh)",
                objetivo = "Tarifa objetivo (Q/kWh)", aporte = "Subsidio (Q/kWh)",
                paga = "Paga el hogar (Q/kWh)")
  tar <- \(t, nombre) {
    t <- as.data.frame(t)[, names(tar_cols), drop = FALSE]
    names(t) <- unname(tar_cols)
    cbind(escenario = nombre, t)
  }
  tarifas <- do.call(rbind, c(
    list(tar(es_base$tar_base, SERIE_BASE)),
    lapply(seq_along(es), \(i) tar(es[[i]]$tar_esc, nombres[i]))))

  # Matriz de indicadores principales: Base + (subsidio, + compensación) por escenario
  mats <- lapply(corridas, \(c) attr(c$resumen, "resumen"))
  mat_base <- attr(corrida_base$resumen, "resumen")
  cols_esc <- \(i) if (con_compensacion[i]) 2:3 else 2L
  resumen <- do.call(cbind, c(list(mat_base[, 1, drop = FALSE]),
                              lapply(seq_along(mats), \(i) mats[[i]][, cols_esc(i), drop = FALSE])))
  colnames(resumen) <- c("Base (subsidio vigente)", unlist(lapply(seq_along(nombres), \(i)
    paste0("Escenario ", i, c(": cambio en subsidio", ": + compensación")[cols_esc(i) - 1L]))))

  meta_g <- attr(r1, "grupos")
  etiquetas <- paste("Escenario", seq_along(nombres))
  nd <- \(x) if (is.null(x)) rep(NA_character_, length(nombres)) else x
  equivalencias <- data.frame(
    etiqueta     = c(SERIE_BASE, etiquetas),
    nombre       = c("Situación vigente", nombres),
    esquema      = c("Tarifas vigentes", nd(descripciones$esquema)),
    compensacion = c("Sin compensación", nd(descripciones$compensacion)),
    stringsAsFactors = FALSE)
  list(
    nombres     = nombres,
    con_compensacion = setNames(con_compensacion, nombres),
    equivalencias = equivalencias,
    resumen     = resumen,
    pobreza     = pobreza,
    desigualdad = desigualdad,
    kakwani     = kakwani,
    fiscal      = fiscal,
    estratos    = estratos,
    grupos      = grupos,
    grupos_meta = lapply(meta_g, \(g) list(titulo = g$titulo, etiquetas = unname(g$etiquetas))),
    tarifas     = tarifas,
    macro       = attr(r1, "macro")
  )
}

#' Ejecuta cada escenario de forma aislada (corrida propia del motor) y combina.
#'
#' La columna "Base" sale de `data/base_scenario.rds` si está vigente; si falta o
#' está desactualizada se avisa y se usa la de la primera corrida (comportamiento
#' anterior). `base = FALSE` ignora el archivo; `base` también puede ser una lista
#' con la forma de `leer_base_scenario()` (se usa en las pruebas).
#'
#' @param cache entorno opcional para reutilizar corridas con las mismas tablas.
#' @param progreso función opcional `function(i, n, nombre)` llamada antes de cada corrida.
ejecutar_escenarios <- function(escenarios, libs, plantilla = tablas_escenario_plantilla(),
                                cache = NULL, progreso = NULL, base = "auto") {
  n <- length(escenarios)
  nombres <- vapply(escenarios, \(e) trimws(e$name), "")
  corridas <- vector("list", n)
  for (i in seq_len(n)) {
    if (!is.null(progreso)) progreso(i, n, nombres[i])
    tablas <- compose_scenario_inputs(escenarios[i], libs, plantilla)
    clave <- rlang::hash(tablas)
    if (!is.null(cache) && !is.null(cache[[clave]])) {
      corridas[[i]] <- cache[[clave]]
    } else {
      corridas[[i]] <- tryCatch(
        list(resumen = run_scenario(tablas)$resumen),
        error = \(e) stop(sprintf("Escenario «%s»: %s", nombres[i], conditionMessage(e)), call. = FALSE))
      if (!is.null(cache)) cache[[clave]] <- corridas[[i]]
    }
  }

  corrida_base <- NULL
  estado_base <- list(vigente = NA, motivos = character())
  if (!isFALSE(base)) {
    b <- if (is.list(base)) base else leer_base_scenario()
    estado_base <- base_vigente(b)
    if (estado_base$vigente) corrida_base <- b else warning(estado_base$motivos, call. = FALSE)
  }

  nom_ref <- \(x, vacio) if (normalizar_ref(x) == "") vacio else trimws(x)
  descripciones <- data.frame(
    esquema      = vapply(escenarios, \(e) nom_ref(e$sub, "Tarifas vigentes"), ""),
    compensacion = vapply(escenarios, \(e) nom_ref(e$comp, "Sin compensación"), ""),
    stringsAsFactors = FALSE)
  res <- combinar_resultados(corridas, nombres, base = corrida_base, descripciones = descripciones,
                             con_compensacion = vapply(escenarios, tiene_compensacion, logical(1), libs = libs))
  res$base_desde_archivo <- !is.null(corrida_base)
  res$base_estado <- estado_base
  res
}

#' ¿El escenario tiene una compensación efectiva? (componente elegido y con grupo 1-3).
tiene_compensacion <- function(escenario, libs) {
  k <- normalizar_ref(escenario$comp)
  nzchar(k) && isTRUE(as.numeric(libs$comp[[k]]$grupo_com %||% 0) %in% 1:3)
}

#' Corrige las etiquetas de sexo del jefe de hogar en `grupos` (ver combinar_resultados()).
#' Solo actúa si las categorías llegan como c("0", "Hombre"), es decir, con el desfase
#' conocido del pipeline; si el colega corrige su etiquetado, no hace nada.
corregir_sexo_jefe <- function(grupos) {
  i <- grupos$grupo == "sex"
  cats <- setdiff(unique(grupos$categoria[i]), "Total")
  if (setequal(cats, c("0", "Hombre"))) {
    grupos$categoria[i] <- c("0" = "Hombre", "Hombre" = "Mujer",
                             "Total" = "Total")[grupos$categoria[i]]
  }
  grupos
}
