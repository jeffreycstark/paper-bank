## ──────────────────────────────────────────────────────────────────────────────
## 03 — Hypothesis Tests: The Satisfaction Paradox (South Korea, 2001–2019)
##
## Purpose: Formal tests of the paper's core hypotheses.
##
## H1 (Decoupling): Economic performance perceptions have a declining marginal
##    effect on democratic satisfaction across ABS waves 1–5 in Korea.
##
## H2 (Trust Mediation): The relationship between economic performance and
##    democratic satisfaction is mediated by institutional trust, such that
##    declining trust attenuates the positive performance–satisfaction link.
##
## H3 (Institutional Specificity): The decoupling is more pronounced for
##    political/accountability institutions (legislature, parties, elections)
##    than for civil service or courts.
##
## H4 (Comparator Falsification): The decoupling pattern is specific to Korea
##    and is not observed (or is weaker) in Taiwan and Japan over the same
##    period.
##
## Input:   analysis/results/m1_pooled.rds
##          analysis/results/m2_trust_mediated.rds
##          analysis/results/m3_interaction.rds
## Output:  analysis/results/hypothesis_tests.rds
##          analysis/results/inline_stats.RData
## ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)

project_root <- "/Users/jeffreystark/Development/Research/econdev-authpref"
paper_dir    <- file.path(project_root, "papers/south-korea-hollow-satisfaction")
analysis_dir <- file.path(paper_dir, "analysis")
results_dir  <- file.path(analysis_dir, "results")

# [TBD: load model objects, run marginaleffects / mediation tests]

cat("03_hypothesis_tests.R — stub complete.\n")
