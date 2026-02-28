## R/helpers.R
## Thailand Trust Collapse — Shared constants and helpers
## Sourced by: 01_descriptive_analysis.R, 03_hypothesis_tests.R,
##             04_attitudinal_mechanisms.R, 05_new_analyses.R, 06_manuscript_figures.R

# ── Country colour palette ────────────────────────────────────────────────────
country_colors <- c(
  "Thailand"    = "#e67e22",
  "Philippines" = "#e74c3c",
  "Taiwan"      = "#2ecc71"
)

# ── Control variable string ───────────────────────────────────────────────────
# Age, gender, education, urban/rural — used in H1–H5 models
controls <- "age_centered + female + education_z + is_urban"

# ── Helper: clustered standard errors for stacked OLS models ─────────────────
# Stacked models have 2 rows per respondent (military + government trust).
# Use sandwich::vcovCL to cluster SEs by respondent_id.
# Requires: sandwich, lmtest
tidy_clustered <- function(model, cluster_var) {
  vcov_cl <- sandwich::vcovCL(model, cluster = cluster_var)
  ct <- lmtest::coeftest(model, vcov. = vcov_cl)
  tibble::tibble(
    term      = rownames(ct),
    estimate  = ct[, "Estimate"],
    std.error = ct[, "Std. Error"],
    statistic = ct[, "t value"],
    p.value   = ct[, "Pr(>|t|)"]
  )
}
