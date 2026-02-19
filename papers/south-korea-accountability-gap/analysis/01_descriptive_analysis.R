## ──────────────────────────────────────────────────────────────────────────────
## 01 — Descriptive Analysis: South Korea Accountability Gap
##
## Purpose: Descriptive statistics and exploratory figures.
##
## Input:   analysis/kor_panel.rds
## Output:  analysis/figures/fig*.png
##          analysis/tables/tab*.csv
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
paper_dir    <- file.path(project_root, "papers/south-korea-accountability-gap")
analysis_dir <- file.path(paper_dir, "analysis")
fig_dir      <- file.path(analysis_dir, "figures")
table_dir    <- file.path(analysis_dir, "tables")
results_dir  <- file.path(analysis_dir, "results")

kor <- readRDS(file.path(analysis_dir, "kor_panel.rds"))

# ─── [DESCRIPTIVES TBD] ───────────────────────────────────────────────────────
