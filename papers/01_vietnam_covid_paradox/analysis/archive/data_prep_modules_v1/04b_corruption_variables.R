# ==============================================================================
# CORRUPTION VARIABLES MODULE
# ==============================================================================
# Purpose: Process corruption perception and experience variables
# Dependencies: Modules 01-03 (CONFIG, ab_selected, helper functions)
# Inputs: ab_selected (with q115-q119, q79)
# Outputs: ab_selected (with corruption perception and experience variables)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("CORRUPTION VARIABLES MODULE\n")
cat(rep("=", 70), "\n\n")

# ============================================
# CORRUPTION PERCEPTION VARIABLES (Q115-Q116)
# ============================================

cat("[corruption] Corruption perception variables (q115-q116):\n")
cat("  Original coding (non-ordinal):\n")
cat("    1 = Hardly anyone, 2 = Not a lot, 5 = No one (all LOW)\n")
cat("    3 = Most officials, 4 = Almost everyone (HIGH)\n")
cat("  Recoding to ordinal 1-4 scale: Higher = More corruption perception\n\n")

# Recode corruption perception variables directly in mutate
# Maps non-ordinal scale to ordinal 1-5 where higher = more corruption
ab_selected <- ab_selected %>%
  mutate(
    corrupt_local = case_when(
      q115 == 5 ~ 1,  # No one is involved → lowest corruption
      q115 == 1 ~ 2,  # Hardly anyone → low corruption
      q115 == 2 ~ 3,  # Not a lot → moderate corruption
      q115 == 3 ~ 4,  # Most officials → high corruption
      q115 == 4 ~ 5,  # Almost everyone → highest corruption
      TRUE ~ NA_real_
    ),
    corrupt_national = case_when(
      q116 == 5 ~ 1,  # No one is involved → lowest corruption
      q116 == 1 ~ 2,  # Hardly anyone → low corruption
      q116 == 2 ~ 3,  # Not a lot → moderate corruption
      q116 == 3 ~ 4,  # Most officials → high corruption
      q116 == 4 ~ 5,  # Almost everyone → highest corruption
      TRUE ~ NA_real_
    )
  )

cat("✓ Recoded q115 → corrupt_local (1-5 scale, higher = more corruption)\n")
cat("✓ Recoded q116 → corrupt_national (1-5 scale, higher = more corruption)\n")

# ============================================
# ANTI-CORRUPTION EFFECTIVENESS (Q117)
# ============================================

cat("\n[corruption] Anti-corruption effectiveness (q117):\n")
cat("  Original: 1=Very effective, 4=Not effective at all\n")
cat("  Recoding: Higher = More effective (reversed)\n\n")

# q117 is standard 4-point scale, can use safe_reverse_4pt
ab_selected <- ab_selected %>%
  mutate(corrupt_effectiveness = safe_reverse_4pt(q117))

cat("✓ Recoded q117 → corrupt_effectiveness (1-4 scale, higher = more effective)\n")

# ============================================
# HIGH CORRUPTION BINARY INDICATORS
# ============================================

cat("\n[corruption] Creating binary high corruption indicators:\n")
cat("  High = 'Most officials' or 'Almost everyone' (original values 3-4)\n\n")

ab_selected <- ab_selected %>%
  mutate(
    corrupt_local_high = case_when(
      q115 %in% c(3, 4) ~ 1,
      q115 %in% c(1, 2, 5) ~ 0,
      TRUE ~ NA_real_
    ),
    corrupt_national_high = case_when(
      q116 %in% c(3, 4) ~ 1,
      q116 %in% c(1, 2, 5) ~ 0,
      TRUE ~ NA_real_
    )
  )

cat("✓ Created corrupt_local_high (binary: 1 = high corruption perception)\n")
cat("✓ Created corrupt_national_high (binary: 1 = high corruption perception)\n")

# ============================================
# CORRUPTION EXPERIENCE VARIABLES (Q118, Q79)
# ============================================

cat("\n[corruption] Corruption experience variables (q118, q79):\n")
cat("  q118: Personal/witnessed corruption (1=Yes, 2=No)\n")
cat("  q79: Bribe offered by candidate/party (1=Yes, 2=No)\n")
cat("  Recoding to binary: 1=Yes, 0=No\n\n")

ab_selected <- ab_selected %>%
  mutate(
    corrupt_experience = case_when(
      q118 == 1 ~ 1,  # Yes, experienced/witnessed corruption
      q118 == 2 ~ 0,  # No
      TRUE ~ NA_real_
    ),
    corrupt_bribe_offered = case_when(
      q79 == 1 ~ 1,   # Yes, bribe offered
      q79 == 2 ~ 0,   # No
      TRUE ~ NA_real_
    )
  )

cat("✓ Created corrupt_experience (binary)\n")
cat("✓ Created corrupt_bribe_offered (binary)\n")

# ============================================
# CORRUPTION KNOWLEDGE SOURCE VARIABLES (Q119A-C)
# ============================================

cat("\n[corruption] Corruption knowledge sources (q119a-c):\n")
cat("  Recoding to binary: 1=Mentioned, 0=Not mentioned\n\n")

ab_selected <- ab_selected %>%
  mutate(
    corrupt_know_personal = if_else(q119a == 1, 1, 0, missing = NA_real_),
    corrupt_know_family = if_else(q119b == 1, 1, 0, missing = NA_real_),
    corrupt_know_media = if_else(q119c == 1, 1, 0, missing = NA_real_)
  )

cat("✓ Created 3 binary knowledge source variables\n")

# ============================================
# VALIDATE RECODING
# ============================================

cat("\n[validation] Validating variable ranges...\n")

# Validate continuous corruption variables
validate_range(ab_selected, c("corrupt_local", "corrupt_national"), 1, 5, 
               "Corruption perception (local/national)")
validate_range(ab_selected, "corrupt_effectiveness", 1, 4, 
               "Anti-corruption effectiveness")

# Validate binary variables
binary_vars <- c("corrupt_local_high", "corrupt_national_high", 
                 "corrupt_experience", "corrupt_bribe_offered",
                 "corrupt_know_personal", "corrupt_know_family", "corrupt_know_media")

for (var in binary_vars) {
  if (var %in% names(ab_selected)) {
    min_val <- min(ab_selected[[var]], na.rm = TRUE)
    max_val <- max(ab_selected[[var]], na.rm = TRUE)
    if (min_val < 0 || max_val > 1) {
      stop(sprintf("Range check failed for %s: [%.0f, %.0f], expected [0, 1]",
                   var, min_val, max_val))
    }
  }
}

cat("✓ All variables within expected ranges\n")

# ============================================
# MISSING DATA REPORT
# ============================================

cat("\n[missing] Missing data report by country:\n\n")

all_corrupt_vars <- c("corrupt_local", "corrupt_national", "corrupt_effectiveness",
                      "corrupt_local_high", "corrupt_national_high",
                      "corrupt_experience", "corrupt_bribe_offered")

report_missing(ab_selected, all_corrupt_vars, label = "Corruption Variables")

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ CORRUPTION VARIABLES MODULE COMPLETE\n")
cat("  Variables created:\n")
cat("    - corrupt_local (1-5, higher = more corruption)\n")
cat("    - corrupt_national (1-5, higher = more corruption)\n")
cat("    - corrupt_effectiveness (1-4, higher = more effective)\n")
cat("    - corrupt_local_high (binary: perceives high local corruption)\n")
cat("    - corrupt_national_high (binary: perceives high national corruption)\n")
cat("    - corrupt_experience (binary: witnessed corruption)\n")
cat("    - corrupt_bribe_offered (binary: offered bribe)\n")
cat(rep("=", 70), "\n\n")