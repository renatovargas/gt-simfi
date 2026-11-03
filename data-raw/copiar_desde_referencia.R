# Copia inicial verbatim desde ../GTM_ELEC (solo lectura en el origen).
# Se corre desde la raíz del repo. Verifica md5 y registra los manifiestos.
# Si falta un archivo o el md5 no coincide, se detiene.

origen <- "../GTM_ELEC"
stopifnot(dir.exists(origen))

commit <- system2("git", c("-C", origen, "rev-parse", "HEAD"), stdout = TRUE)
sucio  <- length(system2("git", c("-C", origen, "status", "--porcelain"),
                         stdout = TRUE)) > 0

sc <- "02_codigo/scripts_R"
scripts <- c("00_GTM26SUB_Master.R", "x_GTM26SUB_funciones.R",
             "01_GTM26SUB_BaseIngreso.R", "02_GTM26SUB_Subsidio.R",
             "03_GTM26SUB_Transferencia.R", "04_GTM26SUB_Consolida.R",
             "05_GTM26SUB_Reportes.R", "06_GTM26SUB_BaseIngreso.R",
             "06_GTM26SUB_BaseIngreso_fix.R")
fix <- c("GTM_insumos", "GTM_resumen", "GTM_simcomp",
         "GTM_simulaciones_subsidio", "GTM_subeleh", "GTM_tarifas")

manifiesto <- rbind(
  data.frame(origen = file.path(sc, scripts),
             destino = file.path("reference/scripts_R", scripts)),
  data.frame(origen = "01_datos/input/GTM23SUB_parametros.xlsx",
             destino = "reference/params/GTM23SUB_parametros.xlsx"),
  data.frame(origen = c("01_datos/r_presim/disp_income.rds",
                        "01_datos/r_presim/energia_ajustado.rds",
                        "01_datos/r_presim/DispIncome.RDS"),
             destino = c("data/disp_income.rds", "data/energia_ajustado.rds",
                         "data/dispincome_raw.rds")),
  data.frame(origen = file.path("01_datos/r_mod", paste0(fix, ".rds")),
             destino = file.path("tests/fixtures", paste0(fix, ".rds"))),
  data.frame(origen = "02_codigo/do_files/ados/GTM26SUB_resultados_26_Sep_2026.xlsx",
             destino = "tests/fixtures/GTM26SUB_resultados_26_Sep_2026.xlsx")
)

ruta_o <- file.path(origen, manifiesto$origen)
faltan <- manifiesto$origen[!file.exists(ruta_o)]
if (length(faltan)) stop("Faltan en el origen: ", paste(faltan, collapse = ", "))

md5_o <- unname(tools::md5sum(ruta_o))
for (d in unique(dirname(manifiesto$destino))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
ok <- file.copy(ruta_o, manifiesto$destino, overwrite = FALSE)
if (!all(ok)) stop("No se copió (¿ya existe?): ",
                   paste(manifiesto$destino[!ok], collapse = ", "))

md5_d <- unname(tools::md5sum(manifiesto$destino))
if (!identical(md5_o, md5_d)) {
  stop("md5 distinto en: ",
       paste(manifiesto$destino[md5_o != md5_d], collapse = ", "))
}

manifiesto$md5      <- md5_d
manifiesto$bytes    <- file.size(manifiesto$destino)
manifiesto$fecha    <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
manifiesto$commit   <- commit
manifiesto$origen_con_cambios_sin_commit <- sucio

ref <- manifiesto[startsWith(manifiesto$destino, "reference/"), ]
saveRDS(ref, "reference/.manifest.rds")

dat <- manifiesto[startsWith(manifiesto$destino, "data/"), ]
saveRDS(dat, "data/MANIFEST.rds")
writeLines(c(
  "# MANIFEST de data/ (copia inicial desde ../GTM_ELEC)",
  paste0("commit_origen: ", commit,
         if (sucio) " (el árbol de origen tenía cambios sin commit)" else ""),
  paste0("fecha: ", dat$fecha[1]),
  "",
  paste(dat$md5, dat$destino, "<-", dat$origen, sep = "  ")
), "data/MANIFEST")

message("Copiados y verificados: ", nrow(manifiesto), " archivos.")
