# Construye data/params_base.rds: hoja `global_params` del libro de referencia,
# con nombres de columna limpios y las etiquetas exactas (las usa `04` por regex).
# Se corre desde la raíz del repo. OPC no se duplica aquí: vive en R/config.R.

plantilla <- "reference/params/GTM23SUB_parametros.xlsx"
destino   <- "data/params_base.rds"

params_base <- readxl::read_excel(plantilla, sheet = "global_params") |>
  janitor::clean_names()

stopifnot(identical(names(params_base), c("descripcion", "valor", "observaciones")))
saveRDS(params_base, destino, compress = "xz")

# Registrar en MANIFEST (texto y RDS), reemplazando una entrada previa
fila <- data.frame(
  origen  = paste0(plantilla, " [hoja global_params]"),
  destino = destino,
  md5     = unname(tools::md5sum(destino)),
  bytes   = file.size(destino),
  fecha   = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  commit  = readRDS("reference/.manifest.rds")$commit[1],
  origen_con_cambios_sin_commit = NA
)
man <- readRDS("data/MANIFEST.rds")
man <- rbind(man[man$destino != destino, ], fila)
saveRDS(man, "data/MANIFEST.rds")

lineas <- readLines("data/MANIFEST")
lineas <- lineas[!grepl(destino, lineas, fixed = TRUE)]
writeLines(c(lineas, paste(fila$md5, destino, "<-", fila$origen, sep = "  ")),
           "data/MANIFEST")
message("params_base.rds escrito.")
