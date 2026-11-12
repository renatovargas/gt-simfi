# Diagramas de la pestaña «Instructivo» de la app (www/img/).
# Se corre desde la raíz del repo: Rscript data-raw/build_instructivo_img.R
# Los nombres de los componentes del diagrama son ilustrativos.

library(ggplot2)

azul <- "#005A9C"; naranja <- "#e99002"; verde <- "#43ac6a"; rojo <- "#f04124"

guardar <- function(p, archivo, ancho, alto) {
  dir.create("www/img", recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path("www/img", archivo), p, width = ancho, height = alto, dpi = 150, bg = "white")
}

# --- Las cuatro pantallas ---------------------------------------------------------

pantallas <- data.frame(
  n = 1:4,
  nombre = c("Esquemas\ntarifarios", "Compensaciones", "Escenarios", "Simular"),
  tipo = factor(c("Define componentes", "Define componentes", "Compone escenarios", "Resultados"),
                levels = c("Define componentes", "Compone escenarios", "Resultados"))
)

p_pantallas <- ggplot(pantallas, aes(n, 0)) +
  annotate("segment", x = 1, xend = 4, y = 0, yend = 0, colour = "grey70", linewidth = 1.5) +
  geom_point(aes(colour = tipo), size = 20) +
  geom_text(aes(label = n), colour = "white", size = 8, fontface = "bold") +
  geom_text(aes(y = -0.4, label = nombre), size = 5, lineheight = 0.9, vjust = 1) +
  scale_colour_manual(values = c(azul, naranja, verde), name = NULL) +
  scale_x_continuous(limits = c(0.5, 4.5)) +
  scale_y_continuous(limits = c(-0.85, 0.45)) +
  guides(colour = guide_legend(override.aes = list(size = 6))) +
  theme_void(base_size = 16) +
  theme(legend.position = "bottom")
guardar(p_pantallas, "pantallas.png", 9, 3.2)

# --- Cómo se arma un escenario ------------------------------------------------------

comp <- data.frame(
  pantalla = rep(1:2, times = c(3, 3)),
  etiqueta = c("Tarifa\nvigente", "Tarifa social\nampliada", "Descuento\nfocalizado",
               "Sin\ncompensación", "Bono a los\ndeciles 1 y 2", "Bono por\nbajo consumo")
)
comp$k <- ave(comp$pantalla, comp$pantalla, FUN = seq_along)
comp$y <- 4 - comp$k

titulos <- data.frame(pantalla = 1:2, texto = c("1 · Esquemas tarifarios", "2 · Compensaciones"))

esc <- data.frame(escenario = rep(c("Escenario A", "Escenario B"), each = 2),
                  pantalla = rep(1:2, 2), k = c(2, 1, 3, 2))
esc <- merge(esc, comp, by = c("pantalla", "k"))
esc <- esc[order(esc$escenario, esc$pantalla), ]
seg <- do.call(rbind, lapply(split(esc, esc$escenario), \(d) data.frame(
  escenario = d$escenario[1], x = d$pantalla[1], y = d$y[1], xend = d$pantalla[2], yend = d$y[2])))
seg$off <- ifelse(seg$escenario == "Escenario A", 0.05, -0.05)

p_escenario <- ggplot() +
  geom_tile(data = comp, aes(pantalla, y), width = 0.42, height = 0.75, fill = "grey95", colour = "grey60") +
  geom_segment(data = seg, aes(x = x + 0.21, xend = xend - 0.21, y = y + off, yend = yend + off,
                               colour = escenario), linewidth = 1.6, alpha = 0.9) +
  geom_tile(data = esc, aes(pantalla, y, colour = escenario), width = 0.42, height = 0.75,
            fill = NA, linewidth = 1.4) +
  geom_text(data = comp, aes(pantalla, y, label = etiqueta), size = 4, lineheight = 0.9) +
  geom_label(data = titulos, aes(pantalla, 3.75, label = texto), fill = azul, colour = "white",
             size = 5, fontface = "bold") +
  annotate("text", x = 1.5, y = 0.25, size = 4.3, fontface = "italic",
           label = "Un componente de cada pantalla por escenario; cualquier combinación es válida") +
  scale_colour_manual(values = c("Escenario A" = rojo, "Escenario B" = verde), name = NULL) +
  scale_x_continuous(limits = c(0.6, 2.4)) +
  scale_y_continuous(limits = c(0, 4.05)) +
  theme_void(base_size = 15) +
  theme(legend.position = "bottom")
guardar(p_escenario, "escenario.png", 9, 5.2)
