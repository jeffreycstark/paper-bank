# ============================================================================
# VERIFY INLINE STATISTICS
# Run this script to check that manuscript inline stats match computed values
# ============================================================================

library(tidyverse)
library(here)

# Set paths
results_dir <- here("papers/01_vietnam_covid_paradox/analysis/results")
output_dir <- here("papers/01_vietnam_covid_paradox/analysis/output/inline_stats")

# Create output directory if needed
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("VERIFYING INLINE STATISTICS\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n\n")

# ============================================================================
# 1. COEFFICIENT REDUCTION (Bivariate → Multivariate)
# ============================================================================

cat("1. COEFFICIENT REDUCTION ANALYSIS\n")
cat("-" |> rep(40) |> paste(collapse = ""), "\n")

# Load results
h2a_results <- readRDS(file.path(results_dir, "h2a_trust_direct_effects.rds"))
models_full <- readRDS(file.path(results_dir, "m2_full_models.rds"))

# Bivariate coefficients (trust predicting approval, no controls)
bivariate_coefs <- sapply(h2a_results, function(x) x$coef)
cat("\nBivariate trust coefficients (M1: trust only):\n")
print(round(bivariate_coefs, 3))

# Multivariate coefficients (trust with all controls)
multivariate_coefs <- sapply(models_full, function(m) coef(m)["covid_trust_info"])
cat("\nMultivariate trust coefficients (M2: full model):\n")
print(round(multivariate_coefs, 3))

# Calculate reductions
reductions <- (bivariate_coefs - multivariate_coefs) / bivariate_coefs * 100
cat("\nCoefficient reductions (%):\n")
print(round(reductions, 1))

# Summary stats
coef_reduction_stats <- list(
  # By country
  cambodia_bivariate = bivariate_coefs["Cambodia"],
  cambodia_multivariate = multivariate_coefs["Cambodia"],
  cambodia_reduction_pct = reductions["Cambodia"],
  
  thailand_bivariate = bivariate_coefs["Thailand"],
  thailand_multivariate = multivariate_coefs["Thailand"],
  thailand_reduction_pct = reductions["Thailand"],
  
  vietnam_bivariate = bivariate_coefs["Vietnam"],
  vietnam_multivariate = multivariate_coefs["Vietnam"],
  vietnam_reduction_pct = reductions["Vietnam"],
  
  # Ranges for manuscript
  reduction_min_pct = min(reductions),
  reduction_max_pct = max(reductions),
  retention_min_pct = 100 - max(reductions),
  retention_max_pct = 100 - min(reductions),
  
  # Minimum beta across countries (multivariate)
  beta_min = min(multivariate_coefs)
)

cat("\n--- KEY VALUES FOR MANUSCRIPT ---\n")
cat("Reduction range:", round(coef_reduction_stats$reduction_min_pct, 1), 
    "to", round(coef_reduction_stats$reduction_max_pct, 1), "%\n")
cat("Retention range:", round(coef_reduction_stats$retention_min_pct, 1), 
    "to", round(coef_reduction_stats$retention_max_pct, 1), "%\n")
cat("Minimum beta (multivariate):", round(coef_reduction_stats$beta_min, 2), "\n")

# ============================================================================
# 2. SPEECH FREEDOM × APPROVAL CROSSTAB (Social Desirability Defense)
# ============================================================================

cat("\n\n2. SPEECH FREEDOM × APPROVAL ANALYSIS\n")
cat("-" |> rep(40) |> paste(collapse = ""), "\n")

# Load Vietnam data
vietnam <- haven::read_sav(here("data/raw/wave6/W6_11_Vietnam_Release_20250117.sav"))

# q106: People are free to speak without fear (1-2 = agree/free, 3-4 = disagree/constrained)
# q142: Government pandemic handling (1-2 = well, 3-4 = badly)

speech_approval <- vietnam %>%
  filter(q106 %in% 1:4, q142 %in% 1:4) %>%
  mutate(
    constrained = ifelse(q106 %in% c(3, 4), "Feel constrained", "Feel free"),
    approve = ifelse(q142 %in% c(1, 2), "Approve", "Disapprove")
  )

# Overall % constrained
pct_constrained <- speech_approval %>%
  summarise(pct = mean(constrained == "Feel constrained") * 100) %>%
  pull(pct)

# Approval by speech freedom perception
approval_by_constraint <- speech_approval %>%
  group_by(constrained) %>%
  summarise(
    n = n(),
    n_approve = sum(approve == "Approve"),
    pct_approve = mean(approve == "Approve") * 100,
    .groups = "drop"
  )

cat("\nSpeech freedom perception (q106):\n")
cat("  % feeling constrained:", round(pct_constrained, 1), "%\n")
cat("  % feeling free:", round(100 - pct_constrained, 1), "%\n")

cat("\nApproval rates by speech freedom perception:\n")
print(approval_by_constraint)

# Extract for manuscript
approval_constrained <- approval_by_constraint %>% 
  filter(constrained == "Feel constrained") %>% 
  pull(pct_approve)
approval_free <- approval_by_constraint %>% 
  filter(constrained == "Feel free") %>% 
  pull(pct_approve)

speech_freedom_stats <- list(
  pct_constrained = pct_constrained,
  pct_free = 100 - pct_constrained,
  approval_among_constrained = approval_constrained,
  approval_among_free = approval_free,
  n_constrained = approval_by_constraint %>% filter(constrained == "Feel constrained") %>% pull(n),
  n_free = approval_by_constraint %>% filter(constrained == "Feel free") %>% pull(n)
)

cat("\n--- KEY VALUES FOR MANUSCRIPT ---\n")
cat("% constrained:", round(speech_freedom_stats$pct_constrained, 1), "%\n")
cat("Approval among constrained:", round(speech_freedom_stats$approval_among_constrained, 1), "%\n")
cat("Approval among free:", round(speech_freedom_stats$approval_among_free, 1), "%\n")

# ============================================================================
# 3. OTHER INLINE STATS
# ============================================================================

cat("\n\n3. OTHER KEY STATISTICS\n")
cat("-" |> rep(40) |> paste(collapse = ""), "\n")

# Load additional results if available
h1_results <- readRDS(file.path(results_dir, "h1_infection_effects.rds"))
h4a_results <- readRDS(file.path(results_dir, "h4a_economic_bivariate.rds"))

# R-squared for trust (bivariate)
r_squared_trust <- sapply(h2a_results, function(x) {
  if (!is.null(x$r_squared)) x$r_squared else x$coef^2  
})

# Trust effect in points (range of scale * max coefficient)
# Assuming 4-point scale (1-4), moving from 1 to 4 = 3 units * coefficient
trust_effect_points <- max(bivariate_coefs) * 3

# Infection effect (max absolute value)
infection_coefs <- sapply(h1_results, function(x) x$coef)
infection_effect_points <- max(abs(infection_coefs))

other_stats <- list(
  r_squared_trust_min_pct = min(r_squared_trust) * 100,
  r_squared_trust_max_pct = max(r_squared_trust) * 100,
  trust_effect_points = trust_effect_points,
  infection_effect_points = infection_effect_points
)

cat("\nR² range for trust (bivariate):", 
    round(other_stats$r_squared_trust_min_pct, 0), "to", 
    round(other_stats$r_squared_trust_max_pct, 0), "%\n")
cat("Trust effect (scale points, 1→4):", round(other_stats$trust_effect_points, 2), "\n")
cat("Infection effect (max |β|):", round(other_stats$infection_effect_points, 3), "\n")

# ============================================================================
# 4. COMPILE AND SAVE ALL INLINE STATS
# ============================================================================

cat("\n\n4. SAVING VERIFIED STATISTICS\n")
cat("-" |> rep(40) |> paste(collapse = ""), "\n")

# Compile all inline stats
all_inline_stats <- list(
  # Coefficient reduction
  coef_reduction_min_pct = coef_reduction_stats$reduction_min_pct,
  coef_reduction_max_pct = coef_reduction_stats$reduction_max_pct,
  coef_retention_min_pct = coef_reduction_stats$retention_min_pct,
  coef_retention_max_pct = coef_reduction_stats$retention_max_pct,
  beta_min = coef_reduction_stats$beta_min,
  
  # By country
  cambodia_reduction_pct = coef_reduction_stats$cambodia_reduction_pct,
  thailand_reduction_pct = coef_reduction_stats$thailand_reduction_pct,
  vietnam_reduction_pct = coef_reduction_stats$vietnam_reduction_pct,
  
  # Speech freedom (SDB defense)
  speech_pct_constrained = speech_freedom_stats$pct_constrained,
  speech_approval_constrained = speech_freedom_stats$approval_among_constrained,
  speech_approval_free = speech_freedom_stats$approval_among_free,
  
  # Other
  r_squared_trust_min_pct = other_stats$r_squared_trust_min_pct,
  r_squared_trust_max_pct = other_stats$r_squared_trust_max_pct,
  trust_effect_points = other_stats$trust_effect_points,
  infection_effect_points = other_stats$infection_effect_points
)

# Save compiled stats
saveRDS(all_inline_stats, file.path(output_dir, "verified_inline_stats.rds"))
saveRDS(coef_reduction_stats, file.path(output_dir, "coef_reduction_stats.rds"))
saveRDS(speech_freedom_stats, file.path(output_dir, "speech_freedom_stats.rds"))

cat("\nSaved to:", output_dir, "\n")
cat("Files created:\n")
cat("  - verified_inline_stats.rds (all stats)\n")
cat("  - coef_reduction_stats.rds\n")
cat("  - speech_freedom_stats.rds\n")

# ============================================================================
# 5. COMPARISON WITH MANUSCRIPT HARDCODED VALUES
# ============================================================================

cat("\n\n5. COMPARISON: COMPUTED vs MANUSCRIPT\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")

# Manuscript claims (from your edits)
manuscript_claims <- list(
  reduction_range = "16--26%",
  retention = "approximately three-quarters",
  beta_min = 0.31,
  speech_constrained = 40.6,
  speech_approval_constrained = 97.7,
  speech_approval_free = 97.7
)

cat("\nCOEFFICIENT REDUCTION:\n")
cat("  Manuscript says: 16--26%\n")
cat("  Computed:       ", round(coef_reduction_stats$reduction_min_pct, 1), "--", 
    round(coef_reduction_stats$reduction_max_pct, 1), "%\n")
cat("  MATCH:", abs(coef_reduction_stats$reduction_min_pct - 16) < 1 & 
    abs(coef_reduction_stats$reduction_max_pct - 26) < 1, "\n")

cat("\nMINIMUM BETA:\n")
cat("  Manuscript says: > 0.31\n")
cat("  Computed:       ", round(coef_reduction_stats$beta_min, 3), "\n")
cat("  MATCH:", coef_reduction_stats$beta_min > 0.30, "\n")

cat("\nSPEECH FREEDOM (% constrained):\n")
cat("  Manuscript says: ~40%\n")
cat("  Computed:       ", round(speech_freedom_stats$pct_constrained, 1), "%\n")
cat("  MATCH:", abs(speech_freedom_stats$pct_constrained - 40) < 2, "\n")

cat("\nAPPROVAL AMONG CONSTRAINED:\n")
cat("  Manuscript says: 97.7%\n
")
cat("  Computed:       ", round(speech_freedom_stats$approval_among_constrained, 1), "%\n")
cat("  MATCH:", abs(speech_freedom_stats$approval_among_constrained - 97.7) < 1, "\n")

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("VERIFICATION COMPLETE\n")
cat("If any MATCH = FALSE, update manuscript accordingly.\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
