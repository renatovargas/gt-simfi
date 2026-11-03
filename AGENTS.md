# AGENTS.md — App Shiny de impacto fiscal: subsidio eléctrico en Guatemala

Copiar este archivo como `AGENTS.md` en la raíz del repo nuevo y ajustar el nombre del repo y las rutas si cambian.

## Qué es este proyecto

App R Shiny (bslib) de microsimulación para el subsidio a la electricidad en Guatemala (GTM26SUB). Muestra cómo cambian pobreza, desigualdad y costo fiscal al modificar esquemas tarifarios y transferencias compensatorias, sobre microdatos de ENCOVI 2023.

El repo es **nuevo y separado**. Reutiliza dos repos hermanos como referencia, que **no se editan desde aquí**:

| Repo (ruta relativa) | Rol | Acceso |
|---|---|---|
| `../GTM_ELEC` | Pipeline de referencia del colega (R traducido de Stata): especificación y fuente de verdad de los cálculos | Solo lectura; de aquí se copian los archivos del manifiesto |
| `../simfi-rd` | App análoga ya terminada para República Dominicana: modelo de patrones de app, UI y lecciones aprendidas | Solo lectura |

## Reglas de uso de los repos de referencia

- No modificar, mover ni borrar nada en `../GTM_ELEC` ni en `../simfi-rd`. Si hace falta un cambio allí, avisar a la persona usuaria.
- Leer solo rutas concretas. No hacer `grep`/`ls` recursivos sobre `../GTM_ELEC/01_datos` (1.2 GB, mayormente Stata y ENCOVI cruda que no se necesita). Desde los repos de referencia solo se copian los archivos del manifiesto de la sección siguiente.
- Consultar `handover.md` (en la raíz de este repo; si aún no existe, copiarlo primero desde `../GTM_ELEC/handover.md`) antes de diseñar o cambiar la arquitectura. Es el documento de diseño de esta app (capas, adaptador, `sync_reference()`, pruebas de paridad). Si este archivo y el handover discrepan, el handover manda en arquitectura y este archivo manda en convenciones de trabajo.
- Usar `../simfi-rd` para **patrones** (cómo se estructura `app.R`, biblioteca de componentes, composer, gráficos, tablas, autosave), no para **lógica económica**. La economía de GTM viene solo del pipeline del colega.
- No copiar código de `simfi-rd` palabra por palabra sin adaptarlo: sus scripts están acoplados al flujo de República Dominicana (ITBIS, ISR, `yz_pc`, etc.).
- Cuando algo del repo de referencia sea relevante y estable, resumirlo aquí o en la documentación del repo nuevo en lugar de depender de leerlo cada vez.

## Mapa de `../GTM_ELEC` (referencia)

```
01_datos/          1.2 GB, ignorado por git. Dos árboles paralelos:
  r_presim/          RDS usados por el pipeline R (DispIncome.RDS, disp_income.rds, energia_ajustado.rds, ENCOVI)
  r_mod/             Resultados del colega: fixtures de oro (GTM_resumen, GTM_subeleh, GTM_simcomp,
                     GTM_simulaciones_subsidio, GTM_tarifas, GTM_insumos)
  input/             GTM23SUB_parametros.xlsx (parámetros), shp/ (shapefile incompleto: solo .shp, cambiar por geojson en data/gis/departamentos_guatemala.json), adm/ (kWh administrativos)
  raw/ presim/ dbmod/ temp/   Árbol de Stata (.dta). No leer: es el flujo antiguo.
02_codigo/
  scripts_R/         Pipeline R. Cadena activa: 00 Master -> 01 BaseIngreso -> 02 Subsidio -> 03 Transferencia
                     -> 04 Consolida -> 05 Reportes; x_GTM26SUB_funciones.R (funciones puras).
                     06_..._fix.R = calibración re-traducida del .do (usar esta, no la 06 original rota).
                     Los archivos 07-10 son copias antiguas de 02-05: ignorarlos.
  do_files/          Originales de Stata (referencia última, solo para auditoría; nunca se ejecutan)
03_resultados/       Excel generados (no versionar los nuevos)
handover.md          Diseño de la app: lectura obligatoria
```

Puntos clave del pipeline de referencia (detalle en `handover.md`):

- Modelo de escenarios: la hoja `escenarios` asigna cada escenario a un esquema tarifario (`sim_sub`) y a una compensación (`sim_com`/`sim_extra`). El escenario 0 es siempre la base y usa el esquema 0.
- Los scripts dependen del entorno global (`OPC`, `escenarios`, `parm_glob`, carpetas `fpresim`, `fdbmod`...) y se comunican por archivos. Nunca se hace `source("00_..._Master.R")` ni `setwd`.
- No ejecutar `00_Master.R` en sitio: sobrescribe los fixtures de oro en `01_datos/r_mod/`. Correr siempre en una copia en scratch.
- `energia_ajustado.rds` y `disp_income.rds` son artefactos de datos congelados. No sobrescribir `energia_ajustado.rds` con la salida del script `06_fix` (los sorteos aleatorios difieren de Stata).
- Los resultados dependen de los sorteos guardados (`celq`), no de volver a correr `06`.
- **El sorteo de kWh es un artefacto congelado, no un bloqueo.** La ENCOVI no mide bien los kWh por hogar, así que `06` imputa: calcula para cada hogar una distribución de consumo (1-300 kWh), calibrada por *raking* a los registros administrativos de cada distribuidora, y saca `M` sorteos (`celq_1`..`celq_M`). El paso `02` usa solo uno (`celq`) para asignar tramo tarifario y subsidio. Los sorteos de `energia_ajustado.rds` vienen del generador de Stata y R no puede reproducirlos aunque se use la misma semilla. Con ese archivo congelado, `01`-`05` reproducen exactamente los resultados del colega, así que no se necesita su semilla para construir ni correr la app. Un sorteo distinto mueve el costo base ~0.4 % (1,053.0 vs 1,048.5 millones de Q) y deja igual la pobreza base (55.70 %). La semilla importa solo para auditoría y reproducibilidad de la calibración. No tratarla como prerrequisito ni recalibrar sin que se pida.
- Limitación metodológica (para discutir con el colega, no corregir por iniciativa propia): `02` usa un solo sorteo aunque hay `M` disponibles. Promediar sobre los `M` reduciría el ruido, sobre todo en cifras por hogar o grupos pequeños.

## Principios de arquitectura (de `handover.md`)

1. Los scripts del colega se copian **tal cual** a `reference/` y no se editan nunca. Los cambios entran por reemplazo de archivos.
2. Todo el acoplamiento con su código (nombres de archivos, globales, columnas) vive en `R/adapter_*.R`. Nada fuera del adaptador conoce esos nombres.
3. Capas separadas: datos base (RDS en `data/`) -> procesamiento (funciones puras de datos + parámetros) -> vistas (módulos Shiny, exportaciones).
4. Los scripts de referencia se ejecutan con `sys.source()` en un entorno hijo nuevo, con globales simuladas (shim) y un directorio temporal por sesión. Nunca leer de `.GlobalEnv`.
5. Interruptores permanentes (`OPC`: PPP, líneas de pobreza, `tol_recibe`, `M`, semilla) en un solo archivo, `R/config.R`, con los mismos nombres que `OPC`. No exponerlos en la UI salvo que se pida.
6. Los módulos Shiny solo consumen objetos de resultados (formas de `GTM_resumen`/`GTM_insumos`). Ningún módulo llama a un script de referencia. Los gráficos se reconstruyen desde datos, no se extraen del Excel.
7. No recalcular las capas A/B ni el escenario 0 en cada corrida de usuario.
8. Solo se publican RDS en `data/`. Los insumos crudos (`.dta`, `.xlsx`, `.shp`) quedan fuera del repo o en `data-raw/` ignorado por git.

**Los scripts de `reference/` son el motor en producción**, no solo material de prueba: la app los ejecuta a través del adaptador (`sys.source` en un entorno hijo con globales simuladas y carpeta temporal por corrida). **No se crea una carpeta `pipeline/` ni `scripts/` propia con copias modificadas** de esos scripts: todo lo propio de la app vive en `R/adapter_*.R`, `R/engine_*.R` (fase 2) y `mod_*.R`. Si hace falta cambiar un cálculo, el cambio lo hace el colega en su pipeline y entra por reemplazo de archivos en `reference/` más `sync_reference()`. El colega seguirá haciendo cambios según lo que se pida; por eso el motor no se bifurca.

**El agente es quien copia `reference/`, `data/` y `tests/fixtures/` desde `../GTM_ELEC`** (la persona usuaria no copia nada a mano). Es lectura en el origen y escritura solo en este repo. Reglas de la copia:

- Copiar solo el manifiesto de abajo, con `file.copy()` o `cp` (sin transformar contenido), conservando los nombres originales en `reference/` y usando los nombres destino indicados en `data/`.
- Verificar cada copia comparando md5 con el original y registrarlo (origen, md5, fecha, commit de `../GTM_ELEC`) en `reference/.manifest.rds` y en `data/MANIFEST`.
- Después de la copia inicial, `reference/` es inmutable. Una actualización posterior solo se hace cuando la persona usuaria avisa que el colega entregó cambios: el agente vuelve a copiar los archivos desde `../GTM_ELEC` (o desde la ruta que se indique) y corre `sync_reference()`.
- Si algún archivo del manifiesto no existe o su md5 no coincide con lo esperado, detenerse y avisar; no sustituir por otro.

Manifiesto de copia inicial (rutas relativas a `../GTM_ELEC`):

| Origen | Destino |
|---|---|
| `02_codigo/scripts_R/00_GTM26SUB_Master.R`, `x_GTM26SUB_funciones.R`, `01_GTM26SUB_BaseIngreso.R`, `02_GTM26SUB_Subsidio.R`, `03_GTM26SUB_Transferencia.R`, `04_GTM26SUB_Consolida.R`, `05_GTM26SUB_Reportes.R`, `06_GTM26SUB_BaseIngreso.R`, `06_GTM26SUB_BaseIngreso_fix.R` | `reference/scripts_R/` (mismos nombres) |
| `01_datos/input/GTM23SUB_parametros.xlsx` | `reference/params/` |
| `01_datos/r_presim/disp_income.rds` | `data/disp_income.rds` |
| `01_datos/r_presim/energia_ajustado.rds` | `data/energia_ajustado.rds` |
| `01_datos/r_presim/DispIncome.RDS` | `data/dispincome_raw.rds` |
| `01_datos/r_mod/*.rds` (6 archivos) | `tests/fixtures/` |
| `02_codigo/do_files/ados/GTM26SUB_resultados_26_Sep_2026.xlsx` | `tests/fixtures/` |
| `handover.md` | raíz (si aún no está) |

No copiar: scripts `07`-`10`, `.do` de Stata, ENCOVI cruda, `kwh_*.xlsx`, `GTM23SUB_parametros - Copy.xlsx` ni el shapefile incompleto (se agrega cuando llegue completo).

Estructura objetivo del repo (detalle en `handover.md` sección 5.4):

```
app.R        punto de entrada de la app Shiny
index.qmd    página principal del sitio Quarto (guía de descarga y manual de usuario); _quarto.yml en la raíz
handover.md  diseño de la app (no se renderiza; ver sección "Sitio Quarto")
reference/   copia verbatim del pipeline del colega (zona de entrega, inmutable)
R/           config.R, adapter_*.R, data_load.R, sync_reference.R, pipeline_map.R
mod_*.R      módulos Shiny
data-raw/    constructores offline de los RDS y MANIFEST
data/        RDS (capas A/B) + MANIFEST
tests/       pruebas de paridad + tests/fixtures/ (copias de 01_datos/r_mod)
renv.lock
```

## Sitio Quarto (GitHub Pages) en el mismo repo

El repo contiene la app (`app.R`) y un sitio Quarto con la guía de descarga y el manual de usuario. Conviven sin problema, con estas reglas:

- `_quarto.yml` debe limitar lo que se renderiza: `project: render: ["*.qmd"]` (o una lista explícita). Así `handover.md`, `AGENTS.md`, `README.md`, `reference/`, `tests/` y `data-raw/` nunca se convierten en páginas del sitio.
- Definir `output-dir: docs` (o publicar con `quarto publish gh-pages`). Los RDS de `data/` no son recursos del sitio y no deben listarse en `resources:`.
- Las páginas del sitio no deben ejecutar la app ni leer `data/`. Si necesitan código R, usar `freeze: auto` para que el render no dependa de datos que quizá no estén en el entorno de CI.
- GitHub Pages es estático: no ejecuta Shiny. La app se despliega aparte (Posit Connect, shinyapps.io o un servidor propio) y el sitio solo enlaza a ella o explica cómo descargarla y correrla localmente.
- Todo texto del sitio en español, mismo criterio de UI que la app.

## Despliegue: local primero, servidor como opción futura

La forma principal de uso es **correr la app localmente** (por el costo de los cálculos); el sitio explica la descarga y la instalación (`renv`). Hospedarla en un servidor con `mirai` se considera una opción a explorar más adelante. No implementarla sin que se pida, pero diseñar para no cerrar esa puerta:

- Un único punto de entrada al motor (por ejemplo `run_scenario(tablas, session_dir)`) que no use `.GlobalEnv`, `setwd` ni rutas fijas y que escriba solo en un directorio temporal propio. La app solo llama a esa función.
- Si se pasa a servidor: `shiny::ExtendedTask` + `mirai`, con los datos de `data/` cargados una vez por daemon (`mirai::everywhere()`), un directorio temporal por corrida y límite de memoria y de daemons acorde al servidor. Verificar con la prueba de aislamiento entre sesiones.

## Patrones reutilizables de `simfi-rd` (adaptar, no copiar)

- **Biblioteca de componentes + composer.** El usuario guarda componentes con nombre por instrumento (aquí: esquema tarifario y compensación) y arma escenarios nombrados eligiendo un componente de cada biblioteca. Una función `compose_scenario_inputs()` convierte el registro del escenario en las tres tablas del motor (`escenarios`, `sim_sub`, `sim_extra`).
- **Valor centinela.** Los desplegables del composer usan `"__ref__"` (no `""`) para "sin componente / situación actual". `selectize.js` trata `""` como placeholder y no permite volver a seleccionarlo. Normalizar ambos antes de buscar en la biblioteca.
- **Autosave y restauración.** Estado de la sesión en RDS (`app_state/autosave.rds`, ignorado por git) más importación/exportación a Excel de las bibliotecas. Un botón "Nueva sesión" limpia todo, incluida la biblioteca, el autosave y los resultados.
- **Resultados obsoletos.** Poner el reactivo de resultados en `NULL` al iniciar cada corrida para que no se vean resultados viejos.
- **Corridas independientes.** Cada escenario se ejecuta de forma aislada (entorno propio). Límite de escenarios a comparar: **4** (más el escenario 0 de base). Definirlo en una sola constante en `R/config.R` y no repetir el número en el código.
- **Tablas `DT`.** Usar `server = FALSE` en las tablas de resultados de la pestaña de simulación, para que se reinicialicen por completo cuando cambia el número de columnas de escenarios.
- **Insumos por instrumento.** Una función construye el bloque de incidencia de cada instrumento a partir de los resultados, para que la UI no conozca la estructura interna.
- **Jerarquía de ingreso (verificar antes de usar).** En `simfi-rd`, pobreza y desigualdad usan el ingreso final tras todos los instrumentos (`yz_pc`), no los intermedios. En GTM el equivalente son los ingresos por escenario `ys`/`yt` de `04_GTM26SUB_Consolida.R`. Confirmar en el código de `04` cuál se usa para pobreza y desigualdad antes de construir las vistas, y documentarlo aquí.

## Rutas y directorio de trabajo

- El proyecto se usa con **Positron > Open Folder** sobre la raíz del repo. Todas las rutas son **relativas a la raíz** (`data/...`, `R/...`, `reference/...`). No usar `setwd()`, `here::here()` ni rutas absolutas ni nada que dependa de la máquina.
- `_quarto.yml` fija `execute-dir: project` para que los cálculos de los `.qmd` también corran desde la raíz.
- La app se lanza desde la raíz (`shiny::runApp()`), así que `app.R` y los módulos usan las mismas rutas relativas.
- Excepciones a tener presentes: `testthat` corre con el directorio de trabajo en `tests/testthat/`, así que en las pruebas armar las rutas con `testthat::test_path()` o con un helper que suba a la raíz; y los scripts de `reference/` no se ejecutan con sus rutas originales, sino a través del adaptador (entorno con carpetas simuladas).
- Los scripts de `data-raw/` también se corren desde la raíz con rutas relativas.

## Convenciones de UI y código

- Interfaz y textos de cara al usuario en español. No mostrar nombres internos de variables, mecánica del pipeline ni etiquetas como "analista".
- Usar `bslib` para la interfaz y el tema.
- Escribir las letras acentuadas y la puntuación española (á, é, í, ó, ú, ñ, ü, ¿, ¡) como caracteres UTF-8 literales en el código. Para símbolos que no son letras (flechas, punto medio, raya, comillas angulares, puntos suspensivos, delta) se aceptan escapes `\uXXXX` en cadenas de R.
- Codificación: todos los archivos en UTF-8. Al editar, verificar que no se corrompan los caracteres multibyte (`file -I`, o releer el archivo tras escribir). Con las herramientas actuales de edición de Positron esto no suele ser un problema, pero es un riesgo conocido de este proyecto.
- Pipe nativo `|>`. Sin `install.packages()` en tiempo de ejecución; dependencias con `renv`.
- Insignia de estado de componentes: "sin guardar" / "guardado" / "cambios sin guardar".

## Trampas conocidas del pipeline de referencia

- Los parámetros en `04` se buscan por **expresión regular sobre el texto de descripción** (`param_glob(patron)`). Cambiar una etiqueta en el Excel rompe el cálculo en silencio. El adaptador construye `parm_glob` con las etiquetas exactas y un test verifica que cada búsqueda resuelve.
- Inconsistencias de nombres que el adaptador debe absorber: `DispIncome.RDS` vs `disp_income.rds`, `GTM_resumen.RDS` vs `.rds` (importa en Linux/Connect), `indice` vs `indice_ipc_encovi`, `finput` vs `fpresim`.
- Los scripts `01`-`05` no llaman a `library()`; los paquetes salen de la lista del Master (líneas 32-43).
- Hoja `sim_sub` tiene 3 esquemas (0, 1, 2). `GTM23SUB_parametros - Copy.xlsx` es un duplicado idéntico: ignorar. Las descripciones que dicen que el escenario 3 está inactivo están desactualizadas.
- En el esquema 2, `todeoc3`/`todeor3` son NA: DEOCSA/DEORSA pagan tarifa plena en el tramo 3; EEGSA mantiene su subsidio. Parece intencional.
- Shapefile incompleto (solo `.shp`): el mapa falla en silencio hasta recibir `.shx`, `.dbf`, `.prj`.
- Pendientes con el colega, por prioridad: (1) **shapefile completo** (`.shx`, `.dbf`, `.prj`, `.cpg`), lo único que bloquea una funcionalidad (el mapa); (2) secundario: confirmar `06_GTM26SUB_BaseIngreso_fix.R` (o recibir su propia versión) y la semilla/procedimiento de los sorteos guardados. Esto último solo afecta la auditoría de la calibración, no la app.

## Tamaño de archivos versionados (sin Git LFS)

GitHub avisa a partir de 50 MiB por archivo y rechaza el push a partir de 100 MiB; además, un archivo grande que se commitea queda en el historial aunque se borre después. Para no necesitar nunca Git LFS:

- Ningún archivo versionado del repo debe superar **25 MB** (margen de seguridad). Hoy el más grande es `data/dispincome_raw.rds` (~11 MB) y todo `data/` suma ~15 MB (y el repo completo con fixtures y scripts, ~19 MB).
- Antes de agregar cualquier dato o resultado nuevo, revisar su tamaño. Si un archivo se acerca al límite, reducirlo (solo las columnas que usa el motor, `compress = "xz"` en `saveRDS`) o dejarlo fuera del repo (`.gitignore`) y regenerarlo con `data-raw/`.
- Nunca commitear insumos crudos grandes (`.dta`, ENCOVI sin procesar, `kwh_*.xlsx`) ni salidas generadas (`docs/` con el sitio renderizado puede crecer; revisarlo).
- Existe una prueba (`tests/testthat/test-tamano-archivos.R`) que falla si algún archivo rastreado por git pasa de 25 MB. Mantenerla y correrla antes de cada commit con datos nuevos.

## Pruebas y verificación

- Antes de cambiar el motor, congelar los fixtures de oro (`tests/fixtures/`, copiados de `01_datos/r_mod/`) y comprobar paridad con `all.equal` (tolerancia `1e-8`; `1e-6` tras calibración).
- Pruebas obligatorias tras cada sincronización: corrida de oro, caché del escenario base, ida y vuelta de parámetros, verificación de agregados contra Stata (laxa) y aislamiento entre sesiones.
- Sincronizar con el colega mediante `sync_reference()`: comparar md5 de `reference/`, clasificar el impacto y tocar solo el adaptador o módulo que el reporte indique.

## Reglas de trabajo para el agente

- Lo no solicitado no se hace. En particular, no emprender refactors, reimplementaciones nativas del motor ni cambios de arquitectura sin que se pidan. La reimplementación nativa (`engine = "native"`) es fase 2, solo con pruebas de paridad ya existentes.
- Responder y comentar en español, de forma concisa.
- Ante una discrepancia entre el pipeline de referencia y lo que parece correcto económicamente, no corregir el script: reportarla. La fuente de verdad de los cálculos es el pipeline del colega.
- Los datos (ENCOVI 2023 y los insumos derivados) son públicos o compartibles, así que `data/` puede versionarse y el repo puede ser público. No hay restricción de confidencialidad, pero tampoco hace falta imprimir filas de hogares en las respuestas.
- Preferir pasos reversibles (mover o renombrar en lugar de borrar).
