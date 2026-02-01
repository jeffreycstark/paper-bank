# W5 detailed investigation
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/data/processed"
w5 <- readRDS(file.path(data_dir, "w5.rds"))

cat("=== W5 STRUCTURE ===\n\n")
cat("Dimensions:", nrow(w5), "x", ncol(w5), "\n")

# Find country variable
cat("\nLooking for country identifier...\n")
country_candidates <- names(w5)[str_detect(tolower(names(w5)), "country|nation|ctry|cntry")]
cat("Candidates:", paste(country_candidates, collapse = ", "), "\n")

# Check first few columns
cat("\nFirst 10 variable names:\n")
print(names(w5)[1:10])

# Try common alternatives
for (v in c("Country", "COUNTRY", "cntry", "ctry", "nation", "w5_country")) {
  if (v %in% names(w5)) {
    cat("\nFound:", v, "\n")
    print(table(w5[[v]]))
  }
}

# Check if there's a level or region variable that might indicate country
cat("\nChecking 'level' variable:\n")
if ("level" %in% names(w5)) {
  print(table(w5$level))
}

# Just show unique values of first few columns
cat("\nFirst column values:\n")
print(head(w5[[1]], 20))
cat("\nColumn 1 name:", names(w5)[1], "\n")

# Check the q85 battery in W5 - it seems different from W3/W6
cat("\n\n=== W5 DEMOCRACY MEANING BATTERY ===\n")

for (v in paste0("q", 85:91)) {
  if (v %in% names(w5)) {
    cat("\n", v, ":\n")
    lab <- attr(w5[[v]], "label")
    cat("  Label:", substr(lab, 1, 100), "\n")
  }
}

# So W5 has a DIFFERENT democracy meaning battery!
# Let's check what the actual "4 sets" question looks like

cat("\n\nSearching for 'essential' or 'characteristic' in all labels...\n")
for (v in names(w5)) {
  lab <- attr(w5[[v]], "label")
  if (!is.null(lab) && str_detect(lab, "essential|characteristic")) {
    cat(v, ":", substr(lab, 1, 100), "\n")
  }
}

# Check around q88-q95 area
cat("\n\nChecking q88-q99 labels:\n")
for (v in paste0("q", 88:99)) {
  if (v %in% names(w5)) {
    lab <- attr(w5[[v]], "label")
    cat(v, ":", substr(lab, 1, 80), "\n")
  }
}
