## ──────────────────────────────────────────────────────────────────────────────
## 01 — Descriptive Analysis: The Satisfaction Paradox (South Korea, 2001–2019)
##
## Purpose: Generate descriptive figures documenting the satisfaction paradox —
##          trends in economic performance perceptions, institutional trust, and
##          democratic satisfaction across ABS waves 1–5.
##
## Input:   analysis/kor_abs.rds
##          analysis/comp_abs.rds
## Output:  analysis/figures/fig1_econ_trust_trends.png
##          analysis/figures/fig2_dem_sat_trends.png
##          analysis/figures/fig3_comparators.png
##          analysis/results/descriptive_stats.rds
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
paper_dir    <- file.path(project_root, "papers/south-korea-hollow-satisfaction")
analysis_dir <- file.path(paper_dir, "analysis")
fig_dir      <- file.path(analysis_dir, "figures")
results_dir  <- file.path(analysis_dir, "results")

kor_abs  <- readRDS(file.path(analysis_dir, "kor_abs.rds"))
comp_abs <- readRDS(file.path(analysis_dir, "comp_abs.rds"))

# ─── Shared theme ─────────────────────────────────────────────────────────────

theme_paper <- theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = "grey92")
  )

# ─── Figure 1: Economic performance perceptions × institutional trust ─────────
# [TBD: plot wave-mean econ retro and trust index on dual-axis or faceted panel]

# ─── Figure 2: Democratic satisfaction over time ──────────────────────────────
# [TBD: dem_satisfaction wave means with confidence bands]

# ─── Figure 3: Cross-national comparators ────────────────────────────────────
# [TBD: Korea vs Taiwan vs Japan on key outcomes]

# ─── Descriptive stats table ─────────────────────────────────────────────────
# [TBD: wave N, mean ± SD for key variables]

cat("01_descriptive_analysis.R — stub complete. Implement figures once variables confirmed.\n")
