# ==============================================================================
# 01_load_data.R - Load Combined Dataset
# Paper: "I Don't Care How the Sausage Gets Made"
# ==============================================================================

cat("Module 01: Loading combined dataset\n")

# Required packages
library(tidyverse)
library(here)
library(haven)

# ==============================================================================
# Load the combined ABS dataset (all 6 waves, 330+ variables)
# ==============================================================================

data_path <- here("outputs", "abs_econdev_authpref.rds")

if (!file.exists(data_path)) {
  stop("Combined dataset not found at: ", data_path,
       "\nRun the main harmonization pipeline first.")
}

sausage_raw <- readRDS(data_path)

cat("  Loaded", nrow(sausage_raw), "observations x", ncol(sausage_raw), "variables\n")
cat("  Waves:", paste(sort(unique(sausage_raw$wave)), collapse = ", "), "\n")
cat("  Countries:", length(unique(sausage_raw$country)), "\n")

# ==============================================================================
# Load V-Dem democracy scores for regime classification
# ==============================================================================

vdem_path <- here("data", "external", "vdem_scores.rds")

if (file.exists(vdem_path)) {
  vdem_scores <- readRDS(vdem_path)
  cat("  V-Dem scores loaded:", nrow(vdem_scores), "country-years\n")
} else {
  cat("  WARNING: V-Dem scores not found - regime classification unavailable\n")
  vdem_scores <- NULL
}

# Store in global environment for next module
assign("sausage_raw", sausage_raw, envir = .GlobalEnv)
assign("vdem_scores", vdem_scores, envir = .GlobalEnv)

cat("  -> sausage_raw ready for processing\n")
