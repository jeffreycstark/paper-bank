# ==============================================================================
# MODULE 05: DEMOCRACY VARIABLES
# ==============================================================================
# Purpose: Recode democracy variables, normalize scales, create composites
# Dependencies: Modules 01-04 (CONFIG, ab_selected, helper functions)
# Inputs: ab_selected (with democracy vars)
# Outputs: ab_selected (with democracy composites and standardized vars)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 05: DEMOCRACY VARIABLES\n")
cat(rep("=", 70), "\n\n")

# ============================================
# REVERSE-CODE 4-POINT DEMOCRACY VARIABLES
# ============================================

cat("[democracy] Recoding 4-point democracy variables...\n")
cat("  q90: Satisfaction with democracy\n")
cat("  q91: How much of a democracy\n")
cat("  q106: People are free to speak what they think without fear\n")
cat("  q128: Democracy may have problems but still best\n\n")

dem_4pt_vars <- c("q90", "q91", "q106", "q128")

ab_selected <- ab_selected %>%
  mutate(across(all_of(dem_4pt_vars), safe_reverse_4pt, .names = "dem_{.col}"))

recoded_dem_vars <- paste0("dem_", dem_4pt_vars)

validate_range(ab_selected, recoded_dem_vars, 1, 4, "4-point democracy variables")

# Create named variable for freedom of speech
ab_selected <- ab_selected %>%
  mutate(freedom_of_speech = dem_q106)

cat("✓ Created named variable: freedom_of_speech (from q106)\n")

# ============================================
# CLEAN 10-POINT DEMOCRACY SCALES
# ============================================

cat("\n[democracy] Cleaning 10-point democracy scales...\n")
cat("  q92: How democratic is current government [1-10]\n")
cat("  q95: How suitable is democracy for our country [1-10]\n\n")

dem_10pt_vars <- c("q92", "q95")

ab_selected <- ab_selected %>%
  mutate(
    across(all_of(dem_10pt_vars),
           ~{
             x <- as.numeric(.)
             x <- round(x, 0)
             if_else(x >= 1 & x <= 10, x, NA_real_)
           },
           .names = "{.col}_clean")
  )

# Validate 10-point scales
stopifnot("q92_clean missing values 7-9" = all(c(7, 8, 9) %in% ab_selected$q92_clean))
stopifnot("q95_clean missing values 7-9" = all(c(7, 8, 9) %in% ab_selected$q95_clean))
stopifnot("q92_clean range not 1-10" = all(range(ab_selected$q92_clean, na.rm = TRUE) == c(1, 10)))
stopifnot("q95_clean range not 1-10" = all(range(ab_selected$q95_clean, na.rm = TRUE) == c(1, 10)))

cat("✓ 10-point scales cleaned and validated [1-10]\n")

# ============================================
# STANDARDIZE VARIABLES
# ============================================

cat("\n[democracy] Standardizing all democracy variables...\n")

ab_selected <- ab_selected %>%
  mutate(
    # Standardize 4-point recoded variables
    z_dem_q90 = standardize_z(dem_q90),
    z_dem_q91 = standardize_z(dem_q91),
    z_dem_q128 = standardize_z(dem_q128),
    # Standardize 10-point cleaned variables
    z_dem_q92 = standardize_z(q92_clean),
    z_dem_q95 = standardize_z(q95_clean),
    # Normalize to 0-1 for indices
    n_dem_q90 = normalize_0_1(dem_q90),
    n_dem_q91 = normalize_0_1(dem_q91),
    n_dem_q128 = normalize_0_1(dem_q128),
    n_dem_q92 = normalize_0_1(q92_clean),
    n_dem_q95 = normalize_0_1(q95_clean)
  )

cat("✓ Variables standardized (z-scores) and normalized (0-1)\n")

# ============================================
# CREATE DEMOCRACY COMPOSITES
# ============================================

cat("\n[democracy] Creating democracy composites...\n")

ab_selected <- ab_selected %>%
  mutate(
    # Standardized composites for regression (vectorized with rowMeans)
    dem_satisfaction_z = rowMeans(select(., z_dem_q90, z_dem_q92), na.rm = TRUE),
    dem_legitimacy_z = rowMeans(select(., z_dem_q91, z_dem_q95), na.rm = TRUE),
    # Normalized indices for descriptive stats
    dem_satisfaction_index = rowMeans(select(., n_dem_q90, n_dem_q92), na.rm = TRUE),
    dem_legitimacy_index = rowMeans(select(., n_dem_q91, n_dem_q95), na.rm = TRUE),
    # Backwards compatibility aliases
    dem_satisfaction = dem_satisfaction_z,
    dem_legitimacy = dem_legitimacy_z
  )

# Calculate Spearman-Brown reliability for 2-item scales
r_satisfaction <- cor(ab_selected$z_dem_q90, ab_selected$z_dem_q92, use = "complete.obs")
r_legitimacy <- cor(ab_selected$z_dem_q91, ab_selected$z_dem_q95, use = "complete.obs")

sb_satisfaction <- (2 * r_satisfaction) / (1 + r_satisfaction)
sb_legitimacy <- (2 * r_legitimacy) / (1 + r_legitimacy)

cat("✓ Democracy satisfaction composite: Spearman-Brown =", round(sb_satisfaction, 3), "\n")
cat("✓ Democracy legitimacy composite: Spearman-Brown =", round(sb_legitimacy, 3), "\n")

# ============================================
# MISSING DATA REPORT
# ============================================

cat("\n[missing] Missing data report:\n\n")

all_dem_vars <- c(recoded_dem_vars, "q92_clean", "q95_clean")
report_missing(ab_selected, all_dem_vars, label = "Democracy Variables")

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 05 COMPLETE\n")
cat("  - 4-point democracy variables reverse-coded and validated\n")
cat("  - 10-point democracy scales cleaned [1-10]\n")
cat("  - Democracy composites created with reliability checks\n")
cat(rep("=", 70), "\n\n")
