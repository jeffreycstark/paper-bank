# ==============================================================================
# 07_export_analysis_data.R - Export Analysis-Ready Datasets
# Paper: "I Don't Care How the Sausage Gets Made"
# ==============================================================================

cat("Module 07: Exporting analysis-ready datasets\n")

library(tidyverse)
library(here)

# ==============================================================================
# Define output paths
# ==============================================================================

output_dir <- here("papers", "trust-dynamics", "analysis")

# ==============================================================================
# Export full dataset (with missing data flags)
# ==============================================================================

full_path <- file.path(output_dir, "sausage_analysis_full.rds")
saveRDS(sausage_missing, full_path)
cat("  Saved full dataset:", full_path, "\n")
cat("    Dimensions:", nrow(sausage_missing), "x", ncol(sausage_missing), "\n")

# ==============================================================================
# Export complete cases dataset (for main analysis)
# ==============================================================================

complete_path <- file.path(output_dir, "sausage_analysis.rds")
saveRDS(sausage_complete, complete_path)
cat("  Saved complete cases:", complete_path, "\n")
cat("    Dimensions:", nrow(sausage_complete), "x", ncol(sausage_complete), "\n")

# ==============================================================================
# Export variable codebook
# ==============================================================================

codebook <- tibble(
  variable = names(sausage_complete),
  class = sapply(sausage_complete, class) %>% sapply(function(x) paste(x, collapse = ", ")),
  n_missing = sapply(sausage_complete, function(x) sum(is.na(x))),
  n_valid = nrow(sausage_complete) - sapply(sausage_complete, function(x) sum(is.na(x))),
  pct_valid = round(100 * (nrow(sausage_complete) - sapply(sausage_complete, function(x) sum(is.na(x)))) / nrow(sausage_complete), 1)
)

codebook_path <- file.path(output_dir, "variable_codebook.csv")
write_csv(codebook, codebook_path)
cat("  Saved codebook:", codebook_path, "\n")

# ==============================================================================
# Export sample summary
# ==============================================================================

sample_summary <- list(
  total_n = nrow(sausage_missing),
  analysis_n = nrow(sausage_complete),
  n_countries = length(unique(sausage_complete$country)),
  n_waves = length(unique(sausage_complete$wave)),
  countries = sort(unique(sausage_complete$country_name)),
  waves = sort(unique(sausage_complete$wave)),
  regime_distribution = table(sausage_complete$regime_type),
  date_created = Sys.time()
)

summary_path <- file.path(output_dir, "sample_summary.rds")
saveRDS(sample_summary, summary_path)
cat("  Saved sample summary:", summary_path, "\n")

# ==============================================================================
# Print final summary
# ==============================================================================

cat("\n")
cat("=" , rep("=", 59), "\n", sep = "")
cat("  DATA PREPARATION COMPLETE\n")
cat("=" , rep("=", 59), "\n", sep = "")
cat("  Analysis sample: n =", sample_summary$analysis_n, "\n")
cat("  Countries:", sample_summary$n_countries, "\n")
cat("  Waves:", sample_summary$n_waves, "\n")
cat("  Regime distribution:\n")
print(sample_summary$regime_distribution)
cat("=" , rep("=", 59), "\n", sep = "")
