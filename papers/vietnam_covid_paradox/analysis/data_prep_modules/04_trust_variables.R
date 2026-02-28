# ==============================================================================
# MODULE 04: INSTITUTIONAL TRUST VARIABLES
# ==============================================================================
# Purpose: Reverse-code trust variables, validate, create composite index
# Dependencies: Modules 01-03 (CONFIG, ab_selected, helper functions)
# Inputs: ab_selected (with q7-q15)
# Outputs: ab_selected (with trust_q7-q15, institutional_trust_index)
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 04: INSTITUTIONAL TRUST VARIABLES\n")
cat(rep("=", 70), "\n\n")

# ============================================
# REVERSE-CODE TRUST VARIABLES
# ============================================

cat("[trust] Trust variables (q7-q15): 1=Great deal, 4=None at all\n")
cat("[trust] Recoding so higher = more trust (4=Great deal, 1=None at all)\n\n")

ab_selected <- ab_selected %>%
  mutate(across(q7:q15, safe_reverse_4pt, .names = "trust_{.col}"))

# Define variable set for reuse
trust_vars_recoded <- paste0("trust_q", 7:15)

cat("✓ Reversed", length(trust_vars_recoded), "trust variables\n")

# ============================================
# VALIDATE RECODING
# ============================================

cat("\n[validation] Validating range (should be 1-4)...\n")

validate_range(ab_selected, trust_vars_recoded, 1, 4, "Trust variables")

# ============================================
# CREATE VALIDATED COMPOSITE
# ============================================

cat("\n[composite] Creating institutional trust index...\n")

ab_selected <- create_validated_composite(
  data = ab_selected,
  vars = trust_vars_recoded,
  composite_name = "institutional_trust_index",
  min_alpha = CONFIG$min_alpha,
  method = "cronbach",
  min_valid = 7  # Require at least 7 of 9 items (75%)
)

# ============================================
# MISSING DATA REPORT
# ============================================

cat("\n[missing] Missing data report by country:\n\n")

report_missing(ab_selected, trust_vars_recoded, label = "Trust Variables")

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 04 COMPLETE\n")
cat("  - Trust variables reverse-coded (q7-q15)\n")
cat("  - Composite created: institutional_trust_index\n")
cat("  - Reliability checked (α ≥", CONFIG$min_alpha, ")\n")
cat(rep("=", 70), "\n\n")
