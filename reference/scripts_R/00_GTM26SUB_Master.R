## =============================================================================
## Banco Mundial / INDE
## Herramienta de Microsimulación de Subsidios a la Electricidad
## Country: Guatemala 2023
## Authors: Maynor Cabrera
## 00_GTM26SUB_Master.R   (versión R de 00_GTM26SUB_master.do)
## E-mail:  mynorvc@gmail.com
##
## Secuencia:
##   01_GTM26SUB_BaseIngreso.R    consumo de electricidad calibrado + ingreso base
##   02_GTM26SUB_Subsidio.R       subsidio por esquema tarifario (hoja sim_sub)
##   03_GTM26SUB_Transferencia.R  compensaciones (hoja sim_extra)
##   04_GTM26SUB_Consolida.R      ingresos ys/yt, pobreza, desigualdad, grupos
##   05_GTM26SUB_Reportes.R       Excel de resultados (formato alineado con RD)
##
## Insumos (.rds convertidos desde los .dta de Stata; ver README):
##   data/raw/encovi_2023/2_encovi_2023_hogares.rds
##   data/raw/encovi_2023/3_encovi_2023_c01se_fuentes.rds
##   data/raw/encovi_2023/35_encovi_2023_agregado_consumo.rds
##   data/raw/kwh_agooct23.xlsx                    (usuarios por kWh)
##   data/input/DispIncome.rds                     (salida de 05_*_DispIncome.do)
##   data/input/GTM23SUB_parametros.xlsx
##   data/input/shp/<departamentos>.shp            (opcional, para el mapa)
##
## Fecha de creación:   26 sep 2026
## Versión:             1.0
## =============================================================================

rm(list = ls())

## 0. Paquetes -----------------------------------------------------------------
list_libraries <- c(
  "readxl", "tidyverse", "janitor", "openxlsx", "data.table", "statar",
  "scales", "collapse", "rlang", "ragg", "sf", "haven"
)

install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    require(package, character.only = TRUE)
  }
}
invisible(lapply(list_libraries, install_if_missing))

## 1. Folders ------------------------------------------------------------------

path_o <- case_when(
  Sys.getenv("USERNAME") == "mynor" ~
    "C:/Users/mynor/Dropbox (Personal)/Dropbox/zotros/BM/GTM_elec/SIM/",
  TRUE ~ "D:/BM/GTM_elec/"                       # <-- CAMBIAR
)
setwd(path_o)

finput   <- paste0(path_o, "01_datos/input/")
fpresim  <- paste0(path_o, "01_datos/r_presim/")
fdbmod   <- paste0(path_o, "01_datos/r_mod/")
fresults <- paste0(path_o, "03_resultados/")
fencovi  <- paste0(path_o, "01_datos/raw/encovi_2023/")
fscripts <- paste0(path_o, "02_codigo/scripts_R/")
fparams  <- paste0(finput, "GTM23SUB_parametros.xlsx")

for (d in c(fpresim, fdbmod, fresults)) dir.create(d, showWarnings = FALSE,
                                                   recursive = TRUE)

## 2. Opciones generales -------------------------------------------------------

OPC <- list(
  run_diag   = 1,     # 1 = diagnósticos de 06 (tablas en consola)
  adm_file   = paste0(fpresim, "kwh_agooct23.xlsx"),
  semilla    = 20231, # sorteos de kWh (no reproduce los de Stata número a número)
  M          = 10,    # número de imputaciones de kWh
  ## Umbrales de estratos (USD PPA por persona por día) y factor PPA.
  ## VERIFICAR: PPA 2017 -> 2.15 / 6.85; PPA 2021 -> 3.00 / 8.30.
  ppp_fac    = 4.88,
  est_pext   = 2.15,
  est_pob    = 6.85,
  est_vul    = 14,
  pline_int  = 6.85,  # línea internacional usada en la 3a tabla de pobreza
  tol_recibe = 0.01,  # Q anuales: umbral para "recibe / pierde / gana"
  ## Mapa: shapefile de departamentos con código INE 1-22 (se agrega a región)
  shp_deptos = paste0(finput, "shp/departamentos_gtm.shp"),
  shp_codcol = "cod_depto",
  abrir_excel = TRUE
)

## 3. Funciones compartidas y parámetros ---------------------------------------

source(paste0(fscripts, "x_GTM26SUB_funciones.R"))

parm_glob <- read_excel(fparams, sheet = "global_params") %>%
  janitor::clean_names()

escenarios <- read_excel(fparams, sheet = "escenarios") %>%
  janitor::clean_names() %>%
  filter(activo == 1) %>%
  arrange(escenario)

if (!"des_corto" %in% names(escenarios) ||
    all(is.na(escenarios$des_corto))) {
  escenarios$des_corto <- escenarios$des_escenario
}
escenarios <- escenarios %>%
  mutate(des_corto = coalesce(des_corto, des_escenario,
                              paste("Escenario", escenario)))

stopifnot("El escenario 0 (base) debe estar activo" = 0 %in% escenarios$escenario,
          "Hay escenarios repetidos"                 = !anyDuplicated(escenarios$escenario))

## 4. Ejecutar scripts ---------------------------------------------------------

ptm0 <- proc.time()
source(paste0(fscripts, "01_GTM26SUB_BaseIngreso.R"))
print(proc.time() - ptm0)
source(paste0(fscripts, "02_GTM26SUB_Subsidio.R"))
source(paste0(fscripts, "03_GTM26SUB_Transferencia.R"))
source(paste0(fscripts, "04_GTM26SUB_Consolida.R"))
source(paste0(fscripts, "05_GTM26SUB_Reportes.R"))
print(proc.time() - ptm0)

message("Herramienta de subsidios a la electricidad: fin de la secuencia.")
