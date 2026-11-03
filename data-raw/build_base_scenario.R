# Construye data/base_scenario.rds: resultados de la situación base (escenario 0)
# corriendo el motor de referencia una vez. Se corre desde la raíz del repo:
#   source("data-raw/build_base_scenario.R", encoding = "UTF-8")
#
# `04` exige al menos un escenario de reforma, así que se corre un escenario
# "identidad" (esquema 0 sin compensación) y se conserva su resumen completo; la
# columna Base es la misma sea cual sea la reforma. Reconstruir cada vez que
# cambien reference/ o los datos (base_vigente() avisa).

for (f in c("config.R", "adapter_names.R", "adapter_params.R", "adapter_env.R",
            "adapter_engine.R", "adapter_compose.R", "base_scenario.R")) {
  source(file.path("R", f), encoding = "UTF-8")
}

plantilla <- tablas_escenario_plantilla()
libs <- biblioteca_inicial(plantilla)
identidad <- list(name = "Identidad", sub = REF_SENTINELA, comp = REF_SENTINELA)
salida <- run_scenario(compose_scenario_inputs(list(identidad), libs, plantilla))

ref_manifest <- readRDS("reference/.manifest.rds")
base <- list(
  resumen = salida$resumen,
  insumos = insumos_base(),
  commit  = ref_manifest$commit[1],
  fecha   = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
)
destino <- file.path("data", BASE_ARCHIVO)
saveRDS(base, destino, compress = "xz")

# Registro en MANIFEST (RDS y texto), reemplazando una entrada previa
fila <- data.frame(
  origen  = "run_scenario() con escenario identidad (esquema 0, sin compensación)",
  destino = destino,
  md5     = unname(tools::md5sum(destino)),
  bytes   = file.size(destino),
  fecha   = base$fecha,
  commit  = base$commit,
  origen_con_cambios_sin_commit = NA
)
man <- readRDS("data/MANIFEST.rds")
saveRDS(rbind(man[man$destino != destino, ], fila), "data/MANIFEST.rds")

lineas <- readLines("data/MANIFEST", encoding = "UTF-8")
ini <- grep("^# insumos de data/base_scenario.rds", lineas)
if (length(ini)) lineas <- lineas[seq_len(ini - 1)]
lineas <- lineas[!grepl(destino, lineas, fixed = TRUE)]
writeLines(c(
  lineas,
  paste(fila$md5, destino, "<-", fila$origen, sep = "  "),
  paste0("# insumos de data/base_scenario.rds (md5), construida ", base$fecha,
         ", commit de referencia ", base$commit),
  paste(base$insumos, names(base$insumos), sep = "  ")
), "data/MANIFEST", useBytes = TRUE)
message("base_scenario.rds escrito (", round(file.size(destino) / 1024), " KB).")
