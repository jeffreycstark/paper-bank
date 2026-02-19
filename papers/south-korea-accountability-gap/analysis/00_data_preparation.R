## ──────────────────────────────────────────────────────────────────────────────
## 00 — Data Preparation: South Korea Accountability Gap
##
## Purpose: Load harmonized ABS data, subset to South Korea, construct
##          analysis variables, and save the analysis dataset.
##
## Input:   abs_harmonized_path (via _data_config.R)
## Output:  analysis/kor_panel.rds
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
source(file.path(project_root, "_data_config.R"))

paper_dir    <- file.path(project_root, "papers/south-korea-accountability-gap")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")

# ─── 1. Load harmonized data ─────────────────────────────────────────────────

d <- readRDS(abs_harmonized_path)
cat("ABS harmonized:", nrow(d), "obs,", n_distinct(d$country_num), "countries\n")

# ─── 2. Subset to South Korea ────────────────────────────────────────────────

# ABS country code: 3 = Korea
kor <- d |> filter(country_num == 3)
cat("South Korea: n =", nrow(kor), "\n")
cat("Waves:", sort(unique(kor$wave)), "\n\n")

# ─── 3. Construct analysis variables ─────────────────────────────────────────

# [VARIABLE CONSTRUCTION TBD]

# ─── 4. Save ─────────────────────────────────────────────────────────────────

saveRDS(kor, file.path(analysis_dir, "kor_panel.rds"))
cat("✓ Saved kor_panel.rds\n")
