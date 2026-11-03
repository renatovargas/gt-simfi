# Las pruebas corren en tests/testthat/: se fija la raíz de la app sin setwd().
raiz <- normalizePath(test_path("..", ".."))
options(gtsimfi.raiz = raiz)

for (f in c("config.R", "adapter_names.R", "adapter_params.R", "adapter_env.R",
            "adapter_engine.R", "pipeline_map.R", "base_scenario.R", "adapter_results.R")) {
  source(file.path(raiz, "R", f), encoding = "UTF-8")
}

fixture <- function(nombre) readRDS(file.path(raiz, "tests", "fixtures", nombre))
