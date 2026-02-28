## ──────────────────────────────────────────────────────────────────────────────
## 00 — Data Preparation: South Korea Accountability Gap
##
## Purpose: Load harmonized ABS and KAMOS data, subset/clean, construct
##          analysis variables, and save the analysis dataset.
##
## Data sources:
##   ABS  — Asian Barometer Survey (country_num == 3, Korea, waves 1–6)
##   KAMOS — Korean Attitudes and Mass Opinion Survey (domestic panel)
##
## Note: KAMOS harmonization pipeline lives in survey-data-prep.
##       kamos_harmonized_path must be defined in _data_config.R before
##       this script will run. Add it alongside abs_harmonized_path.
##
## Input:   abs_harmonized_path  (via _data_config.R)
##          kamos_harmonized_path (via _data_config.R — TBD)
## Output:  analysis/kor_abs.rds
##          analysis/kor_kamos.rds
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/07_south_korea_accountability_gap")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")

# ─── 1. ABS — Asian Barometer Survey ─────────────────────────────────────────

abs <- readRDS(abs_harmonized_path)
cat("ABS harmonized:", nrow(abs), "obs,", n_distinct(abs$country_num), "countries\n")

# ABS country code: 3 = Korea
kor_abs <- abs |> filter(country == 3)
cat("Korea (ABS): n =", nrow(kor_abs), "\n")
cat("Waves:", sort(unique(kor_abs$wave)), "\n\n")

# ─── 2. KAMOS — Korean Attitudes and Mass Opinion Survey ─────────────────────

if (!exists("kamos_harmonized_path")) {
  warning(
    "kamos_harmonized_path is not defined in _data_config.R.\n",
    "Add it once the KAMOS harmonization pipeline is complete in survey-data-prep.\n",
    "Skipping KAMOS load."
  )
  kor_kamos <- NULL
} else {
  kor_kamos <- readRDS(kamos_harmonized_path)
  cat("KAMOS: n =", nrow(kor_kamos), "\n")
  cat("Waves/years:", sort(unique(kor_kamos$year)), "\n\n")
}

# ─── 3. Construct analysis variables ─────────────────────────────────────────

# [VARIABLE CONSTRUCTION TBD — will draw on both kor_abs and kor_kamos]

# ─── 4. Save ─────────────────────────────────────────────────────────────────

saveRDS(kor_abs, file.path(analysis_dir, "kor_abs.rds"))
cat("✓ Saved kor_abs.rds\n")

if (!is.null(kor_kamos)) {
  saveRDS(kor_kamos, file.path(analysis_dir, "kor_kamos.rds"))
  cat("✓ Saved kor_kamos.rds\n")
}
