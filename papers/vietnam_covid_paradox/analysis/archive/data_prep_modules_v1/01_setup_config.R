# ==============================================================================
# MODULE 01: CONFIGURATION AND SETUP
# ==============================================================================
# Purpose: Load packages, configure environment, source helper functions
# Dependencies: R/utils/_load_functions.R
# Outputs: CONFIG (list object), all helper functions in scope
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 01: CONFIGURATION AND SETUP\n")
cat(rep("=", 70), "\n\n")

# ============================================
# PACKAGE LOADING
# ============================================

# Set CRAN mirror and global options
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(scipen = 999)  # Disable scientific notation
set.seed(2025)         # Reproducibility

# Load required packages
suppressPackageStartupMessages({
  library(conflicted)   # Handle namespace collisions
  library(tidyverse)
  library(haven)
  library(here)
  library(janitor)
  library(assertr)      # Hard data validation
  library(psych)        # Reliability analysis
  library(gt)           # Tables
  library(naniar)       # Missing data viz
  library(skimr)        # Data summaries
  library(labelled)     # Labeled data handling
})

# Resolve namespace conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("alpha", "psych")

cat("✓ Packages loaded and conflicts resolved\n")

# ============================================
# SOURCE CUSTOM HELPER FUNCTIONS
# ============================================

t0 <- Sys.time()
source_success <- tryCatch({
  source(here("R", "utils", "_load_functions.R"))
  TRUE
}, error = function(e) {
  cat("❌ Error sourcing R/utils/_load_functions.R:", conditionMessage(e), "\n")
  FALSE
})
t1 <- Sys.time()

if (!source_success) {
  stop("Failed to source helper functions")
}

cat(sprintf("✓ Helper functions sourced in %.2f sec\n",
            as.numeric(difftime(t1, t0, units = "secs"))))

# ============================================
# VERIFY CRITICAL FUNCTIONS EXIST
# ============================================

required_functions <- c(
  # Recoding functions
  "safe_reverse_3pt", "safe_reverse_4pt", "safe_reverse_5pt",
  # New data prep helpers
  "create_validated_composite", "validate_range", "report_missing",
  "batch_clean_missing"
)

missing_functions <- required_functions[!sapply(required_functions, exists)]

if (length(missing_functions) > 0) {
  stop("❌ Missing required functions: ", paste(missing_functions, collapse = ", "))
}

cat("✓ All", length(required_functions), "required functions available\n")

# ============================================
# CONFIGURATION OBJECT
# ============================================

CONFIG <- list(
  # Country selection (3 analysis countries for Vietnam COVID paradox paper)
  countries_of_interest = c(8, 11, 12),
  country_labels = c(
    "8" = "Thailand",
    "11" = "Vietnam",
    "12" = "Cambodia"
  ),

  # Missing value codes
  missing_codes_universal = c(-1, 0, 97, 98, 99),
  missing_codes_short_scale = c(7, 8, 9),

  # Scale variable definitions
  short_scale_vars = c(
    # Economic Evaluations (5-point scales: q1-q6)
    paste0("q", 1:6),
    # Trust (4-point scales: q7-q15)
    paste0("q", 7:15),
    # Democracy (4-point only - EXCLUDE q92 and q95 which are 10-point)
    "q90", "q91", "q128",
    # Trade-offs (5-point scales)
    "q126", "q127",
    # Authoritarianism Support (4-point: q129-q132)
    paste0("q", 129:132),
    # Acceptance of Authoritarian Actions (4-point: q168-q171)
    paste0("q", 168:171),
    # Emergency Powers (4-point: q172a-e)
    paste0("q172", letters[1:5]),
    # COVID Variables (4-point or 3-point)
    "q138", "q140", "q141", "q142", "q161", "q162", "q163",
    paste0("q139", letters[1:4]),
    paste0("q143", letters[1:5]),
    # Political engagement
    "q47", "q48", "q49",
    # Political attitudes (blind loyalty, govt trust, responsiveness, satisfaction, system support, transparency)
    "q137", "q136", "q112", "q108", "q96", "q82", "q124",
    # Employment & SES variables
    "se9", "se9c_4", "se10", "se14", "se14a"
  ),

  # Reliability thresholds
  min_alpha = 0.70,
  min_items_fraction = 0.60
)

cat("✓ CONFIG object created with", length(CONFIG), "parameters\n")

# ============================================
# VERIFY PACKAGE VERSIONS
# ============================================

cat("\n=== Key Package Versions ===\n")
cat("dplyr:", as.character(packageVersion("dplyr")), "\n")
cat("tidyverse:", as.character(packageVersion("tidyverse")), "\n")
cat("here:", as.character(packageVersion("here")), "\n")

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 01 COMPLETE\n")
cat("  - Packages loaded and configured\n")
cat("  - Helper functions sourced and verified\n")
cat("  - CONFIG object ready\n")
cat(rep("=", 70), "\n\n")
