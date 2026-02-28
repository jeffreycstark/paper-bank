library(arrow)
library(tidyverse)

# Load harmonized WVS data
source("../../../_data_config.R")
wvs_path <- wvs_harmonized_path

stopifnot("Harmonized WVS parquet not found" = file.exists(wvs_path))

wvs <- read_parquet(wvs_path)
hk <- wvs |> filter(country == "HKG", wave == 7)

cat("Hong Kong W7 N:", nrow(hk), "\n")

# Check democracy items
cat("\n=== dem_importance_democracy ===\n")
cat("Distribution:\n")
print(table(hk$dem_importance_democracy, useNA = "always"))
cat("Mean:", round(mean(hk$dem_importance_democracy, na.rm = TRUE), 2), "\n")

cat("\n=== dem_how_democratic ===\n")
cat("Distribution:\n")
print(table(hk$dem_how_democratic, useNA = "always"))
cat("Mean:", round(mean(hk$dem_how_democratic, na.rm = TRUE), 2), "\n")

cat("\n=== dem_strong_leader ===\n")
cat("Distribution:\n")
print(table(hk$dem_strong_leader, useNA = "always"))
cat("Mean:", round(mean(hk$dem_strong_leader, na.rm = TRUE), 2), "\n")

cat("\n=== dem_democratic_system ===\n")
cat("Distribution:\n")
print(table(hk$dem_democratic_system, useNA = "always"))
cat("Mean:", round(mean(hk$dem_democratic_system, na.rm = TRUE), 2), "\n")

# Search for all democracy-related variables
cat("\n=== All dem_ variables ===\n")
dem_vars <- grep("^dem_", names(hk), value = TRUE)
for (v in dem_vars) {
  vals <- hk[[v]]
  vals_valid <- vals[!is.na(vals)]
  if (length(vals_valid) > 0) {
    cat(v, ": Valid N:", length(vals_valid), " Mean:", round(mean(vals_valid), 2),
        " Range:", min(vals_valid), "-", max(vals_valid), "\n")
  } else {
    cat(v, ": No valid observations\n")
  }
}

# Check trust variables (already reversed: higher = more trust)
cat("\n=== Trust variable means (1-4, higher = more trust) ===\n")
trust_vars <- c("trust_police", "trust_courts", "trust_government",
                "trust_parliament", "trust_armed_forces", "trust_political_parties")
for (v in trust_vars) {
  if (v %in% names(hk)) {
    vals <- hk[[v]]
    vals_valid <- vals[!is.na(vals)]
    cat(v, ": Valid N:", length(vals_valid), " Mean:", round(mean(vals_valid), 2), "\n")
  }
}
