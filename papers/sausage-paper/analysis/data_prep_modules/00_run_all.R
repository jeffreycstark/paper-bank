# ==============================================================================
# 00_run_all.R - Master Runner for Data Preparation Modules
# Paper: "I Don't Care How the Sausage Gets Made"
# ==============================================================================
#
# Purpose: Execute all data preparation modules in sequence
# Output: analysis/sausage_analysis.rds (analysis-ready dataset)
#
# Usage:
#   source("papers/sausage-paper/analysis/data_prep_modules/00_run_all.R")
#
# ==============================================================================

cat("\n")
cat("=" , rep("=", 69), "\n", sep = "")
cat("  SAUSAGE PAPER - DATA PREPARATION PIPELINE\n")
cat("=" , rep("=", 69), "\n", sep = "")
cat("  Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=" , rep("=", 69), "\n\n", sep = "")

# Track timing
pipeline_start <- Sys.time()

# Set up paths
library(here)
here::i_am("papers/sausage-paper/analysis/data_prep_modules/00_run_all.R")
module_dir <- here("papers/sausage-paper/analysis/data_prep_modules")

# ==============================================================================
# Execute modules in sequence
# ==============================================================================

modules <- c(
  "01_load_data.R",
  "02_select_variables.R",
  "03_select_countries.R",
  "04_create_composites.R",
  "05_recode_variables.R",
  "06_handle_missing.R",
  "07_export_analysis_data.R"
)

for (module in modules) {
  module_path <- file.path(module_dir, module)

  if (file.exists(module_path)) {
    cat("\n--- Running:", module, "---\n")
    module_start <- Sys.time()

    tryCatch({
      source(module_path)
      module_time <- round(difftime(Sys.time(), module_start, units = "secs"), 1)
      cat("  Completed in", module_time, "seconds\n")
    }, error = function(e) {
      cat("  ERROR:", conditionMessage(e), "\n")
      stop("Pipeline halted due to error in ", module)
    })
  } else {
    cat("\n--- SKIPPING (not found):", module, "---\n")
  }
}

# ==============================================================================
# Pipeline summary
# ==============================================================================

pipeline_time <- round(difftime(Sys.time(), pipeline_start, units = "secs"), 1)

cat("\n")
cat("=" , rep("=", 69), "\n", sep = "")
cat("  PIPELINE COMPLETE\n")
cat("=" , rep("=", 69), "\n", sep = "")
cat("  Total time:", pipeline_time, "seconds\n")
cat("  Output: papers/sausage-paper/analysis/sausage_analysis.rds\n")
cat("  Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=" , rep("=", 69), "\n\n", sep = "")
