#!/usr/bin/env Rscript
# ==============================================================================
# MASTER DATA PREPARATION PIPELINE
# ==============================================================================
# Purpose: Run all 13 modules in sequence
# Usage: source("papers/01_vietnam_covid_paradox/analysis/data_prep_modules/00_run_all.R")
#    or: Rscript papers/01_vietnam_covid_paradox/analysis/data_prep_modules/00_run_all.R
#
# Output: ab_analysis_v2.rds, ab_analysis_v2_validated.rds
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("ASIAN BAROMETER DATA PREPARATION v2.0\n")
cat("Modular Pipeline - 13 Modules\n")
cat(rep("=", 80), "\n\n")

# ============================================
# CONFIGURATION
# ============================================

skip_validation <- FALSE  # Set TRUE for quick runs (skips Module 09)

# ============================================
# MODULE EXECUTION
# ============================================

modules <- c(
  "01_setup_config.R",
  "02_load_filter.R",
  "03_variable_selection.R",
  "04_trust_variables.R",
  "04b_corruption_variables.R",
  "04c_economic_variables.R",  # ADDED: Economic variables for sensitivity analysis
  "04d_political_attitudes.R", # ADDED: Political attitudes for robustness analyses
  "05_democracy_variables.R",
  "06_authoritarianism_variables.R",
  "07_emergency_powers.R",
  "08_covid_variables.R",
  "09_validation_quality.R",
  "10_finalize_export.R"
)

# Optional: Skip validation module
if (skip_validation) {
  modules <- modules[modules != "09_validation_quality.R"]
  cat("⚠ Skipping validation module (quick run mode)\n\n")
}

# Execute pipeline
pipeline_start_time <- Sys.time()

for (i in seq_along(modules)) {
  cat(sprintf("[%d/%d] Running %s...\n", i, length(modules), modules[i]))

  .mod_timer_start <- Sys.time()
  source(here::here("papers", "01_vietnam_covid_paradox", "analysis",
                    "data_prep_modules", modules[i]))
  .mod_timer_elapsed <- difftime(Sys.time(), .mod_timer_start, units = "secs")

  cat(sprintf("      ✓ Completed in %.1f seconds\n\n", .mod_timer_elapsed))
}

total_time <- difftime(Sys.time(), pipeline_start_time, units = "mins")

# ============================================
# COMPLETION SUMMARY
# ============================================

cat(rep("=", 80), "\n", sep = "")
cat(sprintf("✓ PIPELINE COMPLETE in %.1f minutes\n", total_time))
cat(rep("=", 80), "\n\n")

cat("Next Steps:\n")
cat("  1. Check data/processed/ for output files\n")
cat("  2. Run downstream analyses (05_hypothesis_testing.qmd, etc.)\n")
cat("  3. Verify outputs match original version\n\n")

cat("Quick Data Check:\n")
cat("  ab_analysis: N =", nrow(ab_analysis), "observations\n")
cat("  ab_analysis_validated: N =", nrow(ab_analysis_validated), "observations\n\n")
