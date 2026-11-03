# Handover: from the reference pipeline to a general Shiny app

Audience: agents (and people) building the Shiny fiscal-impact app from this repository.
Scope of this file: describe the reference pipeline, name its chokepoints, and define a
repo layout and mapping layer so the colleague's pipeline stays **untouched** and the app
absorbs his changes by dropping files into a reference folder.

Nothing in the reference pipeline was modified to produce this document.

---

## 1. Operating principle

The colleague's R pipeline (translated from Stata) is the **specification and the source of
truth for calculations**. The app is a **separate repo** that:

1. Never edits his files. They are copied verbatim into `reference/`.
2. Reads everything through an **adapter** (`R/adapter_*.R`) that is the only place that knows
   his file names, globals and column names.
3. Keeps three layers apart: **base data** (RDS, built once) -> **processing** (pure
   functions of data + parameters) -> **views/outputs** (Shiny modules, exports).

Workflow when he adds a feature: he changes his pipeline -> sends the files -> you drop them
into `reference/` -> run `sync_reference()` (section 7) -> it reports what changed and which
adapter entry is affected -> fix only the adapter/module that the report points to -> run the
parity tests (section 8).

---

## 1b. Verification results (pipeline executed 2026-11-02)

After `01_datos/` was populated, the pipeline was run end to end in a **scratch copy**
(paths in `00_Master.R` rewritten, `abrir_excel = FALSE`; the repo itself was not written to).
Findings that supersede earlier assumptions in this file:

| Check | Result |
|---|---|
| `00_Master.R` chain (`01`-`05`) runs with the data now in `01_datos/` | Yes, no errors. About **10 s** in total (`01`: 1.3 s). |
| Outputs vs the colleague's files in `01_datos/r_mod/` | **Identical** (`all.equal`, tol 1e-8) for all six: `GTM_insumos`, `GTM_resumen`, `GTM_simcomp`, `GTM_simulaciones_subsidio`, `GTM_subeleh`, `GTM_tarifas`. Use these as the golden fixtures (section 8); no new reference run is needed. |
| `01` rebuilds `r_presim/disp_income.rds` | Identical to the stored file. |
| Inputs actually needed by `01`-`05` | `r_presim/DispIncome.RDS`, `r_presim/energia_ajustado.rds`, `input/GTM23SUB_parametros.xlsx` (+ optional shapefile). **The raw ENCOVI files are not needed** to run `01`-`05`. |
| Script `06` (calibration) runs as shipped | **No.** (a) `pobre = mean_st(pobre)` (l.73) fails: the ENCOVI column is `pobreza`. (b) Part 2 then fails: it reads `leer_base(finput/"DispIncome")` but the file is in `r_presim/`. |
| Does the shipped `06` reproduce the stored `r_presim/energia_ajustado.rds`? | **No.** Even with (a) patched, it differs: the stored file has 32 columns (adds `celq_esp`, `cele_esp`), different household classes and different draws. The shipped R `06` is an older translation than the Stata `06` (3 tariff tiers instead of 6, different rounding windows, 25 raking iterations instead of up to 500). |
| **`06_GTM26SUB_BaseIngreso_fix.R`** (re-translated from the Stata `06`, now in `scripts_R/`; the shipped `06` is left untouched) | **Works, and is the script to use as the calibration step.** Verified in a scratch copy (about 4 s). Against the stored `energia_ajustado.rds`: identical columns and `hhid`/`empresa_elec`/`depto`/`clase` (all 7 class counts match exactly); `p_sub` within 3e-6, `celq_esp` within 1e-4 kWh, `cele_esp` within 3e-4 Q (raking tolerance). Not reproducible: the random draws (`celq`, `cele`, `celq_1..10`, and hence `tarifa_social`), because R's RNG differs from Stata's. Run in the full chain (`fix` then `01`-`05`), headline results move slightly: base subsidy cost 1,053.0 vs 1,048.5 million Q (+0.4%), base poverty unchanged at 55.70%; mean relative difference over the whole `resumen` matrix 0.2%. Scenarios are consistent with each other because all use the same draws. |
| `02` uses which kWh column | `celq` (a single draw), not `celq_esp`. Results therefore depend on the stored draws, not on re-running `06`. |
| Shapefile | `01_datos/input/shp/` contains **only** `departamentos_gtm.shp`; `.shx`, `.dbf`, `.prj` are missing. `05` emits a GDAL error and skips the map. |
| Parameter workbook | `sim_sub` has **3** schemes (0, 1, 2), not 4. `GTM23SUB_parametros - Copy.xlsx` is byte-identical (same md5): ignore it. The `escenarios` sheet has all four scenarios active; the descriptions saying scenario 3 is "inactive" are stale text. |
| NA in `sim_sub` scheme 2 (`todeoc3`, `todeor3`) | Consistent with the outputs: DEOCSA/DEORSA tier 3 pays the full tariff (no subsidy) in scheme 2, EEGSA keeps its tier-3 subsidy. It looks intentional; confirm only if convenient. |

Practical consequences:
- **Do not run `00_Master.R` in place**: it overwrites `01_datos/r_mod/` (the golden files),
  `01_datos/r_presim/disp_income.rds`, and creates a new timestamped workbook in
  `03_resultados/`. Always run in a scratch copy or with redirected folders.
- Speed is not the driver for separating layers: the whole chain takes seconds. The reasons
  are cleanliness, reproducibility and isolation (sections 3-4). Base-scenario caching
  (`base_scenario.rds`) is therefore **optional**; keep it only if profiling the app shows a need.
- `energia_ajustado.rds` and `disp_income.rds` are treated as **frozen data artifacts** (layer
  B), taken from `r_presim/`. The golden `r_mod/` results depend on the stored draws, so
  keep the stored file; do **not** overwrite it with the output of the fix script. The fix
  script exists so the calibration can be audited and re-run if needed, and its results
  are statistically equivalent (differences come from the random draws only).
- Provisional status of the fix: it was written from the Stata `06` without the colleague's
  involvement. When he is available, ask him to confirm it (or to send his own version) and
  to say which seed/RNG produced the stored draws. The mapping section (5.1) lists it as
  `06_GTM26SUB_BaseIngreso_fix.R`; when his file arrives, it replaces the fix and the
  comparison above is repeated.

---

## 2. What the reference pipeline is today

Location in this repo: `02_codigo/scripts_R/` (active chain is `00`-`05`; Stata originals
in `02_codigo/do_files/` are the ultimate reference). Parameters:
`01_datos/input/GTM23SUB_parametros.xlsx`.

| Step | File | Role | Reads | Writes |
|---|---|---|---|---|
| 0 | `00_GTM26SUB_Master.R` | Paths, packages, `OPC` options, scenario table, runs 01-05 | params xlsx (`global_params`, `escenarios`) | creates folders |
| fn | `x_GTM26SUB_funciones.R` | Stata-semantics helpers, weighted stats, FGT/Gini/Theil/Lorenz/concentration, tariff-stratum helpers, distributor/region maps | nothing | nothing (pure functions, sourced once) |
| 1 | `01_GTM26SUB_BaseIngreso.R` | Builds household disposable-income base (`disp_income.rds`), adds electricity vars, deflates, deciles/strata | `DispIncome.RDS`, `energia_ajustado.rds` | `disp_income.rds` |
| 1-full | `06_GTM26SUB_BaseIngreso.R` | **Not sourced by master.** Full ENCOVI kWh calibration (raking, M imputations, seed) + same income base | 3 ENCOVI files, admin kWh xlsx, `DispIncome` | `energia_ajustado.rds`, `disp_income.rds` |
| 2 | `02_GTM26SUB_Subsidio.R` | Tariff schemes -> payments and subsidy per household per scheme | sheet `sim_sub`, `energia_ajustado.rds` | `GTM_subeleh.rds`, `GTM_tarifas.rds` |
| 3 | `03_GTM26SUB_Transferencia.R` | Compensation transfers per `sim_com` (decile / bono social / low-consumption) | sheet `sim_extra`, `disp_income.rds` | `GTM_simcomp.rds` |
| 4 | `04_GTM26SUB_Consolida.R` | Per-scenario income (`ys`, `yt`), poverty, inequality, Kakwani, group tables, mobility, fiscal cost | outputs of 1-3, `global_params` | `GTM_simulaciones_subsidio.rds`, `GTM_resumen.rds` |
| 5 | `05_GTM26SUB_Reportes.R` | Excel workbook, ggplot charts, optional map, `GTM_insumos.rds` | `GTM_resumen`, optional shapefile | `Resultados_GTM26SUB_<ts>.xlsx`, `GTM_insumos.rds` |

Files `07`-`10` are older copies of `02`-`05` (differences: section labels, `indice` vs
`indice_ipc_encovi`, fonts). **Ignore them; do not map them.** Confirm with the colleague
which set is canonical before the first sync.

Scenario model: sheet `escenarios` maps `escenario` -> (`sim_sub` tariff scheme, `sim_com`
compensation). Scenario 0 is always the base and always uses scheme 0. Schemes and
compensations are computed once per unique id, then combined per scenario in step 4.

Parameter workbook (`sim_sub` has 3 schemes: 0, 1, 2) sheets actually used by R: `global_params`, `escenarios`, `sim_sub`,
`sim_extra`. Sheets `tarifa_plena`, `tarifa_bloques`, `alumbrado_publico`, `resumen` are not
read by any R script.

`OPC` (defined in the master) holds options: seed (`semilla`), imputations (`M`), PPP factor
and the three poverty lines, `tol_recibe`, kWh admin file path, shapefile path/column,
`run_diag`, `abrir_excel`.

---

## 3. Chokepoints (where the current design does not suit a general tool)

| # | Chokepoint | Where | Why it matters for the app | Resolution |
|---|---|---|---|---|
| 1 | Master mixes paths, `setwd`, package install, options, scenario table, run order | `00_Master.R` l.29-118 | App cannot call it: `rm(list=ls())`, `setwd`, hard-coded user paths (`C:/Users/mynor/...`, `D:/BM/GTM_elec/`), `install.packages` at runtime | Never source it. Adapter reproduces only what is needed (`OPC`, folder globals, `escenarios`) |
| 2 | Parameter workbook mixes **scenario definitions** (`escenarios`, `sim_sub`, `sim_extra`) with **permanent constants** (`global_params`: PIB, consumption, Tarifa Social amount) | xlsx | Constants must be versioned data; scenarios must be user-editable in the UI | Split (section 4): constants -> `data/params_base.rds`; scenarios -> app state, serialized to the same sheet shape on demand |
| 3 | Invariant base-scenario work reruns every time: `disp_income` build (01), scheme 0 in 02, `ys0`/`yt0`, deciles, strata, `umbr` in 04; calibration (06) is a separate offline step | 01, 06, 02, 04 | Measured: cheap (whole chain ~10 s). Matters for cleanliness and to guarantee the base never drifts, not for speed | Freeze `disp_income.rds` and `energia_ajustado.rds` as data (section 4, layer B). Scenario-0 caching is optional |
| 4 | Global state: scripts rely on `OPC`, `escenarios`, `parm_glob`, `fpresim`, `fdbmod`, `seccion`... existing in `.GlobalEnv`; each ends with `rm(...)` of temporaries (some never created) | all | Not reentrant; two Shiny sessions would collide | Adapter runs each script in a **fresh child environment** (`sys.source(file, envir = new.env(parent = ...))`) pre-populated with shimmed globals |
| 5 | Scripts communicate through files on disk in fixed folders | 01-05 | Concurrency and stale-file risk in a multi-user app | Adapter points `fdbmod` etc. to a per-session `tempfile` dir, or intercepts with in-memory objects where the script structure allows |
| 6 | Randomness set inside the script (`set.seed(OPC$semilla)`, 06 l.426) | 06 | Result depends on call order | Calibration is offline only; the seed is recorded in the data manifest |
| 7 | Parameters hard-coded in code: tariff table `TAR` (06), `cal` list, distributor/region maps (`x_funciones.R` l.110-136; cooperatives are a PLACEHOLDER), `ppp_fac` ("VERIFICAR") | 06, funciones, master | Silent drift between his files and the app | Treat as reference data; the manifest records them; do not copy into app code |
| 8 | Parameters found by **regex on a description text** (`param_glob(patron)` in 04 l.39-47) | 04 | Renaming a label in Excel silently breaks it | Adapter builds `parm_glob` from `params_base.rds` with the exact labels his code expects, and a test asserts the lookups succeed |
| 9 | Output step mixes presentation with data export: timestamped filenames, optionally opens Excel, charts exist only as images embedded in the workbook | 05 | App needs the data behind charts, not the images | The app's views consume `GTM_resumen`/`GTM_insumos` objects directly; 05 is used only for the "Download Excel" button |
| 10 | Naming inconsistencies: `DispIncome.RDS` vs `disp_income.rds`, `GTM_resumen.RDS` vs `.rds` (case matters on Linux), `indice` vs `indice_ipc_encovi`, `r_presim`/`r_mod` vs `presim`/`dbmod`, `finput` vs `fpresim` | various | Breaks on Linux deployment (Connect) | Adapter normalizes file names; do not edit his scripts |
| 11 | No `library()` calls in 01-05 (depend on master), no lockfile | all | Scripts fail when run standalone | App has its own `renv.lock`; adapter attaches the package list taken from master l.32-43 |
| 12 | Data are git-ignored (`*.dta`, `*.rds`, `*.log`), so a fresh clone cannot run; `01_datos/` is now populated locally but untracked. Stray Excel lock files (`~$...xlsx`) and a duplicate `... - Copy.xlsx` appear | repo root | The app repo needs its own data delivery (RDS in `data/`, section 4) | Never rely on the colleague's repo for data; copy the RDS per section 5; never commit lock files |
| 13 | `06` is out of sync with the Stata `06` and with the data it supposedly produced: fails on `pobre` (column is `pobreza`), reads `DispIncome` from `finput` instead of `fpresim`, has 3 tariff tiers instead of 6, and does not write `celq_esp`/`cele_esp` | 06 vs `r_presim/` | The calibration cannot be regenerated or audited from the shipped code | Use `06_GTM26SUB_BaseIngreso_fix.R` (re-translated from the `.do`, verified in 1b); freeze the stored RDS; confirm with the colleague later |
| 14 | `01_datos/` carries two parallel trees: Stata (`presim/`, `dbmod/`, `temp/`, `.dta`) and R (`r_presim/`, `r_mod/`) | `01_datos/` | Easy to read stale Stata-era files | The app reads only the R tree |
| 15 | Shapefile incomplete: only `.shp` present | `input/shp/` | Map fails silently (GDAL error, sheet skipped) | **Superseded:** the app uses the geojson `data/gis/departamentos_guatemala.json`; the shapefile is not used |

Reference results for validation: the colleague's own `01_datos/r_mod/*.rds` (reproduced
exactly, see section 1b) and the Stata workbook
`02_codigo/do_files/ados/GTM26SUB_resultados_26_Sep_2026.xlsx` (R cannot reproduce Stata's
random draws exactly, so compare aggregates with a tolerance).

---

## 4. Target architecture: three layers

```
Layer A  BASE DATA        (rarely changes)     -> RDS in data/, built by data-raw/
Layer B  PRE-PROCESSING   (once per data vintage) -> RDS in data/, built by data-raw/
Layer C  SCENARIO ENGINE  (every user run)     -> pure functions, in-memory
Layer D  VIEWS / OUTPUTS  (Shiny + exports)
```

### Layer A / B: data converted to RDS (built once, committed or stored in a pin/board)

| RDS file | Source | Produced by (reference script) | Used by |
|---|---|---|---|
| `data/encovi_hogares.rds`, `encovi_fuentes.rds`, `encovi_consumo.rds` | 3 ENCOVI `.dta` files | `leer_base()` in `x_funciones.R` (read only) | calibration only (offline) |
| `data/kwh_admin.rds` | `kwh_agooct23.xlsx` | direct read | calibration only |
| `data/energia_ajustado.rds` | **frozen copy of `r_presim/energia_ajustado.rds`** (the shipped `06` does not reproduce it, see 1b) | colleague's calibration | scheme engine (02) |
| `data/disp_income.rds` | **output of 01 / 06 part 2** | `01` (or `06`) offline | 03, 04 |
| `data/dispincome_raw.rds` | `DispIncome` input | direct | 01 |
| `data/gis/departamentos_guatemala.json` | geojson of departments (replaces the shapefile) | already in repo | maps |
| `data/params_base.rds` | `global_params` sheet + `OPC` fixed constants | split from master and xlsx | 04, indicators |
| `data/tariffs_base.rds` | `sim_sub` rows for scheme 0 (current tariffs), lighting charges, `TAR` | xlsx + 06 | scheme engine |
| `data/base_scenario.rds` | **scenario 0 results** (`sub_ele0_*`, `ys0`, `yt0`, base indicators, `umbr`, deciles, strata) | steps 02-04 run once with only scenario 0 | every comparison in 04 |
| `data/MANIFEST.rds` (+ `.md`) | metadata: source file, md5, date, seed, `M`, reference commit | `data-raw/` | integrity checks |

Rules:
- Raw inputs (`.dta`, `.xlsx`, `.shp`) stay out of the app repo (or in `data-raw/`, git-ignored). Only RDS ships.
- Anything in layer A/B is rebuilt **only** when the colleague changes raw data, the calibration, or the base income construction. `sync_reference()` flags this explicitly (section 7); otherwise the app never recomputes it.
- Permanent switches (`OPC` fields: PPP factor, poverty lines, `tol_recibe`, `M`, seed, `run_diag`) live in **one** file, `R/config.R`, as a plain list mirroring his `OPC` names so his code finds them unchanged. Do not expose them in the UI unless asked. `abrir_excel` and file-path fields are dropped (app-irrelevant).

### Layer C: scenario engine (what the user changes)

Inputs only: the **scenario table** (`escenarios`), the **tariff scheme table** (`sim_sub`),
and the **compensation table** (`sim_extra`). These three tables, with exactly his column
names, are the app's scenario model; the UI edits them and the adapter hands them to his
code. Everything else is data from layer A/B.

Cost control: schemes and compensations are keyed by id (`sim_sub`, `sim_com`). Cache each
result by a hash of its parameter row so changing one scenario does not recompute others;
scenario 0 is always read from `base_scenario.rds`.

### Layer D: views and outputs

Shiny modules read result objects (`GTM_resumen`, `GTM_insumos`-shaped) only. No module may
call a reference script. The Excel export through script 05 (`adapter_export.R`, `run_report()`) was DISCARDED; the only exports are the library/scenario workbook and plotly's PNG download. Charts in the app are rebuilt from data; they are not extracted from the workbook.

---

## 5. Files to bring into the app repo

### 5.1 Drop zone: `reference/` (verbatim copies, never edited, versioned by his release)

```
reference/
  scripts_R/
    00_GTM26SUB_Master.R            # read-only: source of OPC, packages, run order
    x_GTM26SUB_funciones.R          # sourced by adapter
    01_GTM26SUB_BaseIngreso.R       # offline only
    06_GTM26SUB_BaseIngreso.R       # offline only (calibration); shipped version is broken, kept as is
    06_GTM26SUB_BaseIngreso_fix.R   # offline only; replacement re-translated from the .do (use this one)
    02_GTM26SUB_Subsidio.R          # engine
    03_GTM26SUB_Transferencia.R     # engine
    04_GTM26SUB_Consolida.R         # engine
    05_GTM26SUB_Reportes.R          # export only
  params/
    GTM23SUB_parametros.xlsx        # template for scenario tables + constants
  stata/                            # optional: .do files, for audit only (never executed)
  VERSION                           # his release label/date; free text
  CHANGELOG.md                      # his notes on what changed (ask him to write one line)
```

Do **not** copy `07`-`10` (superseded copies), Stata logs, result workbooks (except one
validation file, below), or `~$*.xlsx` lock files.

### 5.2 Data to carry over (all available locally in `01_datos/`, untracked)

Minimum to run the engine (`01`-`05`) and the app:

| From (this repo) | To (app repo) |
|---|---|
| `01_datos/r_presim/DispIncome.RDS` | `data/dispincome_raw.rds` (only needed if `01` is re-run) |
| `01_datos/r_presim/disp_income.rds` | `data/disp_income.rds` |
| `01_datos/r_presim/energia_ajustado.rds` | `data/energia_ajustado.rds` (frozen; see 1b) |
| `01_datos/input/GTM23SUB_parametros.xlsx` | `reference/params/` (template) and split into `data/params_base.rds` |
| (shapefile not carried over) | `data/gis/departamentos_guatemala.json` (geojson, already in the repo) |

Offline only, not needed by the app: the three ENCOVI RDS (`01_datos/r_presim/*.RDS`
or `raw/encovi_2023/`), `kwh_agooct23.xlsx`. Everything else in `01_datos/` (`temp/`,
`presim/`, `dbmod/`, `.dta` files, `input/adm/`) belongs to the Stata pipeline and is not carried over.

Still to obtain from the colleague: confirmation of the `06` fix (or his own `06`, plus
the seed behind the stored draws), and (the shapefile is no longer needed: geojson in use).

### 5.3 Validation fixtures: `tests/fixtures/`

Copy from `01_datos/r_mod/`: `GTM_resumen.rds`, `GTM_subeleh.rds`, `GTM_simcomp.rds`,
`GTM_simulaciones_subsidio.rds`, `GTM_tarifas.rds`, `GTM_insumos.rds`. These were verified
to be exactly reproduced by `00`-`05` with the current inputs and parameters (1b). Also
`GTM26SUB_resultados_26_Sep_2026.xlsx` (Stata) for a loose check. Refresh these fixtures
whenever the colleague delivers a new version.

### 5.4 App-side files (written by us, never by him)

```
R/
  config.R               # permanent switches (OPC mirror), package list
  adapter_env.R          # builds the shimmed environment (globals, folders, escenarios, parm_glob)
  adapter_params.R       # scenario tables <-> his xlsx sheet shapes; params_base -> parm_glob
  adapter_engine.R       # run_schemes(), run_compensations(), run_consolidation(): wrap 02/03/04
  # adapter_export.R    # DISCARDED: no Excel export of results; script 05 is not used by the app
  base_scenario.R        # base_vigente(): checks data/base_scenario.rds against reference/ and data md5
  adapter_names.R        # file-name / column-name normalization (indice vs indice_ipc_encovi, RDS case)
  data_load.R            # reads data/*.rds once at app start
  sync_reference.R       # section 7
mod_*.R                  # Shiny modules; consume results objects only
data-raw/
  build_base.R           # runs 06 (+01) offline -> energia_ajustado, disp_income
  build_base_scenario.R  # runs engine once for scenario 0 -> base_scenario.rds
  build_params_base.R    # splits global_params/OPC -> params_base.rds
data/                    # RDS only (layer A/B)
reference/               # drop zone
tests/                   # parity tests (section 8)
(no renv: decided by the user; packages are standard and stable)
```

---

## 6. The adapter: how his code runs unchanged

All coupling to his code sits in `adapter_*.R`.

1. **Shim environment.** `make_ref_env(session_dir)` creates a child env containing: `OPC`
   (from `R/config.R`), folder globals (`finput`, `fpresim`, `fdbmod`, `fresults`,
   `fencovi`, `fscripts`, `fparams`) pointing at a per-session temp dir pre-seeded with the
   needed RDS from `data/` under the **file names his scripts expect** (including `.RDS`
   vs `.rds` case variants), `escenarios`, `parm_glob`, and the functions from
   `x_GTM26SUB_funciones.R`. Libraries are attached from the master's package list.
2. **Parameter workbook.** `write_params_xlsx(scenario_tables, params_base, path)` writes a
   workbook with the **same sheet names and column names** as his template
   (`global_params`, `escenarios`, `sim_sub`, `sim_extra`). His scripts then read it
   as they do today. This is the single most important compatibility contract: if he
   adds a column or sheet, the template in `reference/params/` shows it and the sync
   report flags it.
3. **Execution.** `sys.source("reference/scripts_R/02_...R", envir = env)` then 03, 04. Results
   are read from the env or the temp `fdbmod`. Scenario 0 comes from `base_scenario.rds`
   (do not rerun).
4. **Isolation.** One env and one temp dir per Shiny session/run; remove on session end.
5. **Escape hatch (phase 2).** When a calculation is stable and slow, reimplement it as a
   native function in `R/engine_*.R` and keep the wrapped version as the oracle in
   tests. Switch by config flag (`engine = "reference" | "native"`). Do this only
   after parity tests exist.

Known compatibility issues the adapter must absorb: `01` reads `DispIncome.RDS` from the
wrong folder variable (header says `finput`, code uses `fpresim`); `05` reads `GTM_resumen.RDS`
(upper case); `08`/`03` column `indice_ipc_encovi` vs `indice`; `01` references calibration
parameters and `rm()` calls for objects it never creates (harmless, wrapped in
`suppressWarnings`).

---

## 7. `sync_reference()`: absorbing his updates

Purpose: after dropping new files into `reference/`, tell the agent exactly what to touch.

Steps it performs (store the previous snapshot in `reference/.manifest.rds`):

1. md5 of every file in `reference/` vs the previous snapshot -> list added/changed/removed.
2. For each changed script, static scan (via `codetools`/`all.names` or `grep`) for:
   new `read*`/`readRDS` targets, new `write*/saveRDS` targets, new global objects used
   (`OPC$...`, `escenarios$...`, new `fpresim`-style folder variables), new columns read from
   the parameter sheets.
3. For the template workbook: diff of sheet names and column names against the previous one.
4. Classify impact, using the table below, and print it.

| What changed | Impact | Action |
|---|---|---|
| `x_funciones.R` only | none for adapter | rerun tests |
| `02`/`03`/`04` logic, same inputs/outputs | engine | rerun parity tests |
| New column/sheet in params xlsx | **scenario model** | extend `adapter_params.R` + UI input for the new parameter |
| New `OPC$field` | **permanent switch** | add to `R/config.R` (default from his master) |
| New `read` of a file not in `data/` | **layer A/B** | request the raw input, add builder in `data-raw/`, add RDS + manifest entry |
| `01`/`06` changed | **base data stale** | rebuild `data/energia_ajustado.rds`, `disp_income.rds`, and `base_scenario.rds` (it depends on them) |
| New output object in `GTM_resumen` attributes | **views** | add to the relevant `mod_*.R` |
| `05` changed | export only | rerun export test; app charts unaffected |
| New script file (e.g. `06b_...`) | new step | add a wrapper in `adapter_engine.R`; register in `R/pipeline_map.R` |

`R/pipeline_map.R` is a small table (one row per reference script: file, layer, phase
`offline|engine|export`, inputs, outputs, depends on). It is the mapping he never has to
know about. Update it only when `sync_reference()` reports a new or restructured script.

---

## 8. Parity tests (the safety net)

Located in `tests/`; run after every sync.

1. **Golden run.** Execute his pipeline end-to-end inside a scratch dir with the fixture
   data; compare against the adapter's outputs for the same scenario tables: all
   `sub_ele*_hh`, `comp_*`, `ys*/yt*` columns, and every table in `GTM_resumen`
   (`all.equal`, tolerance `1e-8`; `1e-6` after calibration).
2. **Base-scenario cache.** `base_scenario.rds` equals a fresh run of scenario 0.
3. **Parameter round-trip.** scenario tables -> xlsx -> read by his code gives the same values; `param_glob()` lookups all resolve.
4. **Stata check** (loose): aggregates in the fixture workbook within tolerance (random draws differ).
5. **Isolation.** Two sessions with different scenarios give independent results.

---

## 9. Order of work for the agent

1. **The agent copies** the data listed in 5.2 from the colleague's repo (`../GTM_ELEC`, read-only at the source; the user copies nothing by hand), with the destination names given there, verifying md5 and recording origin, md5, date and source commit in `data/MANIFEST`. Use `06_GTM26SUB_BaseIngreso_fix.R` in place of `06` until the colleague confirms it; he only needs to provide the full shapefile. Confirm the canonical script set (`07`-`10` appear to be stale copies of `02`-`05`).
2. Create the new repo skeleton (5.4) and **the agent copies** `reference/` (5.1) from `../GTM_ELEC`, verbatim, with md5 verification recorded in `reference/.manifest.rds`. Do not alter these copies afterwards; later updates are re-copied only when the user says the colleague delivered changes, then `sync_reference()` runs.
3. Write `data-raw/` builders; produce RDS and `MANIFEST`; commit RDS (or pin them).
4. Write the adapter and `pipeline_map.R`; get the golden run passing (section 8).
5. Build Shiny modules against the result objects (bslib). Scenario editor edits the three tables.
6. Add `sync_reference()`; document the drop-in procedure in the app's README.
7. Only then consider native reimplementation of hot spots (6.5).

## 10. Do and don't

- Do treat the `reference/` folder as immutable; changes are by replacement.
- Do keep every column name, sheet name and file name his code expects, inside the adapter only.
- Do keep a single source of permanent switches (`R/config.R`).
- Don't copy his logic into app modules.
- Don't recompute layer A/B or scenario 0 on a user run.
- Don't source `00_Master.R`, don't `setwd`, don't `install.packages` at runtime.
- Don't read from `.GlobalEnv`; always from the shim env.
