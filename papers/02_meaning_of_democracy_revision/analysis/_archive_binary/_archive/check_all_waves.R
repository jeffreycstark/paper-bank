# Check all waves for winner/loser and meaning of democracy variables
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/paper-bank/data/processed"

waves <- c("w1", "w2", "w3", "w4", "w5", "w6")

cat("=============================================================\n")
cat("VARIABLE AVAILABILITY ACROSS ALL WAVES\n")
cat("=============================================================\n\n")

for (w in waves) {
  file_path <- file.path(data_dir, paste0(w, ".rds"))
  if (file.exists(file_path)) {
    cat("---", toupper(w), "---\n")
    df <- readRDS(file_path)
    cat("  Rows:", nrow(df), " Cols:", ncol(df), "\n")
    
    # Check for winner/loser variable
    has_q34a <- "q34a" %in% names(df)
    cat("  q34a (winner/loser):", has_q34a, "\n")
    
    if (has_q34a) {
      cat("    Label:", attr(df$q34a, "label"), "\n")
    }
    
    # Check for q34 (vote choice)
    has_q34 <- "q34" %in% names(df)
    cat("  q34 (vote/party):", has_q34, "\n")
    if (has_q34) {
      q34_label <- attr(df$q34, "label")
      cat("    Label:", substr(q34_label, 1, 80), "\n")
    }
    
    # Check for meaning of democracy variables
    # Pattern varies: q85-q88 in some waves, q88-q91 in W4
    dem_vars_85 <- sum(c("q85", "q86", "q87", "q88") %in% names(df))
    dem_vars_88 <- sum(c("q88", "q89", "q90", "q91") %in% names(df))
    
    cat("  Meaning of democracy (q85-q88):", dem_vars_85, "of 4\n")
    cat("  Meaning of democracy (q88-q91):", dem_vars_88, "of 4\n")
    
    # Check q85 label to confirm it's the right battery
    if ("q85" %in% names(df)) {
      q85_label <- attr(df$q85, "label")
      if (!is.null(q85_label)) {
        is_dem_meaning <- str_detect(q85_label, "essential|democracy|characteristic")
        cat("    q85 is democracy meaning battery:", is_dem_meaning, "\n")
      }
    }
    
    # Countries
    if ("country" %in% names(df)) {
      countries <- unique(as.character(df$country))
      cat("  Countries:", length(countries), "\n")
    }
    
    cat("\n")
    
    rm(df)
    gc()
  } else {
    cat("---", toupper(w), "--- FILE NOT FOUND\n\n")
  }
}

# Now detailed check of W5
cat("\n=============================================================\n")
cat("W5 DETAILED CHECK\n")
cat("=============================================================\n\n")

w5 <- readRDS(file.path(data_dir, "w5.rds"))

cat("W5 variables containing 'q34':\n")
q34_vars <- names(w5)[str_detect(names(w5), "q34")]
print(q34_vars)

for (v in q34_vars) {
  cat("\n", v, ":\n")
  cat("  Label:", attr(w5[[v]], "label"), "\n")
  if (is.labelled(w5[[v]])) {
    labels <- attr(w5[[v]], "labels")
    cat("  Value labels (first 10):\n")
    for (i in seq_len(min(10, length(labels)))) {
      cat("    ", labels[i], "=", names(labels)[i], "\n")
    }
  }
}

cat("\nW5 q85 check:\n")
if ("q85" %in% names(w5)) {
  cat("  Label:", attr(w5$q85, "label"), "\n")
  if (is.labelled(w5$q85)) {
    labels <- attr(w5$q85, "labels")
    cat("  Value labels:\n")
    for (i in seq_along(labels)) {
      cat("    ", labels[i], "=", names(labels)[i], "\n")
    }
  }
}

cat("\nW5 countries:\n")
print(table(w5$country))

# Check W2 and W1 briefly
cat("\n=============================================================\n")
cat("W1 AND W2 QUICK CHECK\n")
cat("=============================================================\n\n")

for (w in c("w1", "w2")) {
  df <- readRDS(file.path(data_dir, paste0(w, ".rds")))
  cat("---", toupper(w), "---\n")
  cat("Variables with 'q34':", paste(names(df)[str_detect(names(df), "q34")], collapse = ", "), "\n")
  cat("Variables with 'q85':", "q85" %in% names(df), "\n")
  cat("Variables with 'q88':", "q88" %in% names(df), "\n")
  
  # Check if any democracy meaning questions exist
  all_vars <- names(df)
  potential_dem <- all_vars[str_detect(tolower(all_vars), "essential|meaning|characteristic")]
  cat("Potential democracy meaning vars:", paste(potential_dem, collapse = ", "), "\n\n")
  rm(df)
}
