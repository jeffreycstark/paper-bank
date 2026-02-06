# 99_create_final_dataset.R
# Final step: Combine harmonized waves and create abs_econdev_authpref.rds
#
# This script:
# 1. Loads the master harmonized datasets from outputs/
# 2. Row-binds all waves into a single dataset
# 3. Applies known data corrections
# 4. Zaps haven labels for clean R usage
# 5. Saves as abs_econdev_authpref.rds

library(here)
library(dplyr)
library(haven)

cat("\n=== CREATING FINAL DATASET: abs_econdev_authpref.rds ===\n\n")

# Load all master wave files
output_dir <- here("outputs")
wave_files <- list.files(output_dir, pattern = "^master_w[1-6]\\.rds$", full.names = TRUE)

cat("Loading master wave files...\n")
wave_list <- lapply(wave_files, function(f) {
  wave_name <- gsub("master_|\\.rds", "", basename(f))
  wave_num <- as.integer(gsub("w", "", wave_name))
  cat("  Loading", basename(f), "...")
  df <- readRDS(f)
  # Add wave column
  df$wave <- wave_num
  cat(format(nrow(df), big.mark = ","), "rows\n")
  df
})

# Combine all waves
cat("\nCombining waves...\n")
abs_combined <- bind_rows(wave_list)
cat("  Combined dataset:", format(nrow(abs_combined), big.mark = ","), "rows,",
    ncol(abs_combined), "columns\n")

# =============================================================================
# KNOWN DATA CORRECTION: Vietnam (country=11) Wave 2 & 3 trust scale reversal
# =============================================================================
# ABS Vietnam trust items (q24-q26) are coded in the opposite direction from
# other countries in both W2 and W3.
#
# W2: Other countries coded 1=None -> 4=Great deal (identity applied).
#     Vietnam W2 coded 1=Great deal -> 4=None. Fix: reverse with 5-x.
#     Evidence: 767/1195 (64%) at value 1 for trust_relatives vs. other
#     countries clustering at values 3-4.
#
# W3: Other countries coded 1=Great deal -> 4=None (safe_reverse_4pt applied).
#     Vietnam W3 coded 1=None -> 4=Great deal, so the reversal was wrong.
#     Fix: undo the reversal with 5-x (same operation).
#     Evidence: After standard reversal, Vietnam W3 mean=1.49 while other
#     countries mean=3.24 and Vietnam W2/W4/W5/W6 all show means 3.3-3.6.
# =============================================================================
cat("\nApplying known data corrections...\n")

for (w in c(2, 3)) {
  vietnam_w <- abs_combined$country == 11 & abs_combined$wave == w
  for (v in c("trust_relatives", "trust_neighbors", "trust_acquaintances")) {
    n_valid <- sum(!is.na(abs_combined[[v]][vietnam_w]))
    abs_combined[[v]][vietnam_w] <- ifelse(
      is.na(abs_combined[[v]][vietnam_w]),
      NA_real_,
      5 - abs_combined[[v]][vietnam_w]
    )
    cat("  Vietnam W", w, " ", v, ": ", n_valid, " obs reversed (5-x)\n", sep = "")
  }
}

# Zap haven labels (convert labelled vectors to regular R vectors)
cat("\nZapping haven labels...\n")
abs_econdev_authpref <- abs_combined %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels))

# Also zap any remaining label attributes
abs_econdev_authpref <- abs_econdev_authpref %>%
  mutate(across(everything(), ~{
    attr(.x, "label") <- NULL
    attr(.x, "format.spss") <- NULL
    attr(.x, "display_width") <- NULL
    .x
  }))

cat("  Labels zapped successfully\n")

# Summary by wave
cat("\n=== WAVE SUMMARY ===\n")
wave_summary <- abs_econdev_authpref %>%
  group_by(wave) %>%
  summarise(n = n(), .groups = "drop")
print(wave_summary)

# Save final dataset to data/processed (primary) and outputs (backup)
output_file <- here("data", "processed", "abs_econdev_authpref.rds")
saveRDS(abs_econdev_authpref, output_file)

# Also save to outputs for convenience
saveRDS(abs_econdev_authpref, here("outputs", "abs_econdev_authpref.rds"))

cat("\n=== FINAL DATASET SAVED ===\n")
cat("File:", output_file, "\n")
cat("Rows:", format(nrow(abs_econdev_authpref), big.mark = ","), "\n")
cat("Columns:", ncol(abs_econdev_authpref), "\n")
cat("Waves:", paste(unique(abs_econdev_authpref$wave), collapse = ", "), "\n")

# List variables
cat("\nVariables:\n")
var_names <- setdiff(names(abs_econdev_authpref), c("wave", "row_id"))
cat(paste(" ", var_names, collapse = "\n"), "\n")

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
cat("DONE: abs_econdev_authpref.rds ready for analysis\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n", sep = "")
