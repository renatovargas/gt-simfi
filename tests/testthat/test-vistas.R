# Gráficos plotly y mapa: etiquetas cortas, hover con el detalle y geojson regional.

for (f in c("results_views.R", "map_views.R")) source(file.path(raiz, "R", f), encoding = "UTF-8")
source(file.path(raiz, "R", "adapter_compose.R"), encoding = "UTF-8")
plantilla <- tablas_escenario_plantilla()
libs <- biblioteca_inicial(plantilla)
e <- list(list(name = "Reforma larga con un nombre muy extenso <b>", sub = names(libs$sub)[2],
               comp = names(libs$comp)[1]),
          list(name = "Solo tarifa", sub = names(libs$sub)[1], comp = REF_SENTINELA))
res <- ejecutar_escenarios(e, libs, plantilla)
geo <- leer_regiones_geo()

datos_trazas <- \(p) plotly::plotly_build(p)$x$data
unir <- \(p, campo) unlist(lapply(datos_trazas(p), `[[`, campo))

test_that("combinar_resultados expone la tabla de equivalencias", {
  eq <- res$equivalencias
  expect_equal(eq$etiqueta, c("Base", "Escenario 1", "Escenario 2"))
  expect_equal(eq$nombre[2], e[[1]]$name)
  expect_equal(eq$esquema[2], e[[1]]$sub)
  expect_equal(eq$compensacion[3], "Sin compensación")
  expect_equal(names(tabla_equivalencias(res)),
               c("Etiqueta en gráficos", "Escenario", "Esquema tarifario", "Compensación"))
})

test_that("los gráficos usan etiquetas cortas y el hover lleva el detalle completo", {
  graficos <- list(
    grafico_pobreza(res, "general"), grafico_desigualdad(res, "gini"),
    grafico_fiscal(res), grafico_costo_pib(res),
    grafico_grupo(res, "dec", "inc", "neto"), grafico_grupo(res, "reg", "mil", "neto"))
  for (g in graficos) {
    expect_s3_class(g, "plotly")
    textos <- unir(g, "hovertext")
    expect_true(any(grepl("Esquema:", textos)) && any(grepl("Compensación:", textos)))
    expect_false(any(grepl(e[[1]]$name, unlist(lapply(datos_trazas(g), \(t) c(t$x, t$y, t$name))),
                           fixed = TRUE)))
    expect_true(all(unir(g, "hoverinfo") == "text"))
    expect_true(all(unir(g, "textposition") == "none"))
  }
  # El nombre del escenario se escapa en el hover (HTML)
  expect_true(any(grepl("&lt;b&gt;", unir(graficos[[1]], "hovertext"), fixed = TRUE)))
  # Eje con "Base" y "Escenario N"
  expect_setequal(unique(unir(graficos[[1]], "x")), c("Base", "Escenario 1", "Escenario 2"))
  expect_s3_class(grafico_estratos(res), "plotly")
})

test_that("ninguna tabla de resultados usa el nombre del usuario como encabezado de columna", {
  nombres_usuario <- vapply(e, `[[`, "", "name")
  sin_nombres <- \(tabla, que) {
    enc <- names(tabla)
    for (n in nombres_usuario) expect_false(any(grepl(n, enc, fixed = TRUE)), info = que)
    expect_true(any(grepl("Escenario 2", enc, fixed = TRUE)) || ncol(tabla) < 3, info = que)
  }
  sin_nombres(tabla_resumen(res), "resumen")
  sin_nombres(tabla_desigualdad(res), "desigualdad")
  sin_nombres(tabla_kakwani(res), "kakwani")
  sin_nombres(tabla_fiscal(res), "fiscal")
  for (tp in TIPOS_POBREZA) sin_nombres(tabla_pobreza(res, tp), paste("pobreza", tp))
  for (g in names(GRUPOS_ETIQ)) for (v in names(VISTAS_ETIQ)) {
    sin_nombres(tabla_grupo(res, g, v), paste("grupo", g, v))
  }
})

test_that("el sexo del jefe se etiqueta Hombre/Mujer y la corrección es condicional", {
  cats <- unique(res$grupos$categoria[res$grupos$grupo == "sex"])
  expect_setequal(cats, c("Hombre", "Mujer", "Total"))
  # Si el pipeline ya viniera bien, no se toca
  g <- data.frame(grupo = "sex", categoria = c("Hombre", "Mujer", "Total"))
  expect_identical(corregir_sexo_jefe(g), g)
  # Con el desfase conocido, se corrige
  g2 <- data.frame(grupo = "sex", categoria = c("0", "Hombre", "Total"))
  expect_equal(corregir_sexo_jefe(g2)$categoria, c("Hombre", "Mujer", "Total"))
})

test_that("la barra de plotly deja solo la descarga PNG", {
  b <- plotly::plotly_build(grafico_fiscal(res))$x$config
  expect_false(b$displaylogo)
  expect_equal(b$toImageButtonOptions$format, "png")
  expect_true(all(c("zoom2d", "pan2d", "select2d", "lasso2d") %in% unlist(b$modeBarButtonsToRemove)))
})

test_that("el geojson regional está en WGS84, es liviano y casa con el pipeline", {
  expect_lt(file.size(file.path(raiz, "data", "gis", "regiones_guatemala.geojson")), 25e6)
  nombres <- vapply(geo$features, \(f) f$properties$region_nom, "")
  expect_equal(length(nombres), 8)
  reg <- unique(res$grupos$categoria[res$grupos$grupo == "reg"])
  expect_setequal(reg[reg != "Total"], nombres)
  xy <- unlist(lapply(geo$features, \(f) f$geometry$coordinates))
  lon <- xy[seq(1, length(xy), by = 2)]; lat <- xy[seq(2, length(xy), by = 2)]
  expect_true(all(lon > -92.5 & lon < -88) && all(lat > 13.5 & lat < 18))
})

test_that("los anillos exteriores del geojson van en sentido horario (requisito de plotly)", {
  area <- \(r) {
    x <- vapply(r, `[[`, 0, 1); y <- vapply(r, `[[`, 0, 2)
    sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y) / 2
  }
  for (f in geo$features) {
    expect_equal(f$geometry$type, "MultiPolygon")
    for (pol in f$geometry$coordinates) expect_lt(area(pol[[1]]), 0)
  }
})

test_that("el mapa regional es una función pura con hover", {
  m <- mapa_region(res, geo, res$nombres[1], "mil", "neto")
  tr <- datos_trazas(m)[[1]]
  expect_equal(tr$type, "choropleth")
  expect_equal(tr$featureidkey, "properties.region_nom")
  expect_equal(length(tr$locations), 8)
  expect_true(all(grepl("Región", tr$text)))
  expect_error(mapa_region(res, geo, "no existe", "mil", "neto"), "Sin datos")
  # Medida de cobertura también se puede mapear
  expect_s3_class(mapa_region(res, geo, res$nombres[2], "cob", "rec_esc"), "plotly")
})
