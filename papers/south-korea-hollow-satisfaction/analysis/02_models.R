## ──────────────────────────────────────────────────────────────────────────────
## 02 — Models: The Satisfaction Paradox (South Korea, 2001–2019)
##
## Purpose: Estimate main models relating economic performance perceptions to
##          institutional trust and democratic satisfaction.  Tests the core
##          hypothesis that economic performance effects on democratic
##          satisfaction are mediated (or moderated) by institutional trust.
##
## Candidate model sequence:
##   M1: Pooled OLS — dem_satisfaction ~ econ_retro + controls + wave FE
##   M2: + institutional trust mediators
##   M3: Interaction — econ × trust (performance–legitimacy decoupling test)
##   M4: Lagged specification (if cross-wave linkage available)
##   M5: Comparator models (Taiwan, Japan) for external validity
##
## Input:   analysis/kor_abs.rds
## Output:  analysis/results/m1_pooled.rds
##          analysis/results/m2_trust_mediated.rds
##          analysis/results/m3_interaction.rds
##          analysis/results/model_comparison.rds
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(lme4)

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
paper_dir    <- file.path(project_root, "papers/south-korea-hollow-satisfaction")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")

kor_abs <- readRDS(file.path(analysis_dir, "kor_abs.rds"))

# ─── Model specifications [TBD] ───────────────────────────────────────────────
#
# M1 — Baseline: econ performance → dem satisfaction
# m1 <- lm(dem_sat ~ econ_retro + age + female + education + urban +
#             income_quintile + factor(wave),
#           data = kor_abs)
#
# M2 — With trust mediators
# m2 <- lm(dem_sat ~ econ_retro + trust_govt + trust_legislature +
#             trust_courts + trust_parties + trust_elections +
#             age + female + education + urban + income_quintile + factor(wave),
#           data = kor_abs)
#
# M3 — Interaction: performance × trust
# m3 <- lm(dem_sat ~ econ_retro * trust_govt +
#             age + female + education + urban + income_quintile + factor(wave),
#           data = kor_abs)

cat("02_models.R — stub complete. Implement once 00_data_preparation.R variables confirmed.\n")
