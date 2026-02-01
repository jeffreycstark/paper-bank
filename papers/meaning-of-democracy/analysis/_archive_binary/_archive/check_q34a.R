# Check q34a and break down by education/urban
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/data/processed"

w3 <- readRDS(file.path(data_dir, "w3.rds"))
w4 <- readRDS(file.path(data_dir, "w4.rds"))
w6 <- readRDS(file.path(data_dir, "w6.rds"))

cat("=== Q34a Variable Check ===\n\n")

# Check W6 first
cat("W6 q34a exists:", "q34a" %in% names(w6), "\n")
if ("q34a" %in% names(w6)) {
  cat("\nW6 q34a label:\n")
  print(attr(w6$q34a, "label"))
  cat("\nW6 q34a value labels:\n")
  if (is.labelled(w6$q34a)) {
    labels <- attr(w6$q34a, "labels")
    for (i in seq_along(labels)) {
      cat("  ", labels[i], "=", names(labels)[i], "\n")
    }
  }
  cat("\nW6 q34a distribution:\n")
  print(table(as.integer(w6$q34a), useNA = "ifany"))
}

# Check W3
cat("\n\nW3 q34a exists:", "q34a" %in% names(w3), "\n")
if ("q34a" %in% names(w3)) {
  cat("\nW3 q34a label:\n")
  print(attr(w3$q34a, "label"))
  cat("\nW3 q34a value labels:\n")
  if (is.labelled(w3$q34a)) {
    labels <- attr(w3$q34a, "labels")
    for (i in seq_along(labels)) {
      cat("  ", labels[i], "=", names(labels)[i], "\n")
    }
  }
}

# Check W4
cat("\n\nW4 q34a exists:", "q34a" %in% names(w4), "\n")
if ("q34a" %in% names(w4)) {
  cat("\nW4 q34a label:\n")
  print(attr(w4$q34a, "label"))
}

# Look for similar variables about winning/losing
cat("\n\n=== Searching for 'win' or 'lose' in variable labels ===\n")
for (v in names(w6)) {
  lab <- attr(w6[[v]], "label")
  if (!is.null(lab) && (grepl("win", lab, ignore.case = TRUE) || grepl("lose", lab, ignore.case = TRUE))) {
    cat(v, ":", substr(lab, 1, 100), "\n")
  }
}
