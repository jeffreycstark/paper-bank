## ──────────────────────────────────────────────────────────────────────────────
## 02 — Main Models: South Korea Accountability Gap
##
## Purpose: Estimate main statistical models.
##
## Input:   analysis/kor_panel.rds
## Output:  analysis/results/*.rds
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

project_root <- "/Users/jeffreystark/Development/Research/paper-bank"
paper_dir    <- file.path(project_root, "papers/south-korea-accountability-gap")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")

kor <- readRDS(file.path(analysis_dir, "kor_panel.rds"))

# ─── [MODELS TBD] ─────────────────────────────────────────────────────────────
