# Check W2 and W3 for winner/loser variables with correct names
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/data/processed"

cat("=== CHECKING W2 FOR q39a ===\n\n")
w2 <- readRDS(file.path(data_dir, "w2.rds"))

cat("W2 has q39a:", "q39a" %in% names(w2), "\n")
if ("q39a" %in% names(w2)) {
  cat("Label:", attr(w2$q39a, "label"), "\n")
  cat("\nDistribution:\n")
  print(table(as.integer(w2$q39a), useNA = "ifany"))
  
  if (is.labelled(w2$q39a)) {
    cat("\nValue labels:\n")
    labels <- attr(w2$q39a, "labels")
    for (i in seq_along(labels)) {
      cat("  ", labels[i], "=", names(labels)[i], "\n")
    }
  }
}

# Also check what democracy meaning questions exist in W2
cat("\n\nW2 democracy meaning battery check:\n")
for (v in paste0("q", 80:95)) {
  if (v %in% names(w2)) {
    lab <- attr(w2[[v]], "label")
    if (!is.null(lab) && str_detect(lab, "essential|characteristic|democracy")) {
      cat(v, ":", substr(lab, 1, 80), "\n")
    }
  }
}

# Check q85-q88 labels specifically
cat("\nW2 q85-q88:\n")
for (v in paste0("q", 85:88)) {
  if (v %in% names(w2)) {
    cat(v, ":", substr(attr(w2[[v]], "label"), 1, 80), "\n")
  }
}

cat("\n\n=== CHECKING W3 FOR q33a ===\n\n")
w3 <- readRDS(file.path(data_dir, "w3.rds"))

cat("W3 has q33a:", "q33a" %in% names(w3), "\n")
if ("q33a" %in% names(w3)) {
  cat("Label:", attr(w3$q33a, "label"), "\n")
  cat("\nDistribution:\n")
  print(table(as.integer(w3$q33a), useNA = "ifany"))
  
  if (is.labelled(w3$q33a)) {
    cat("\nValue labels:\n")
    labels <- attr(w3$q33a, "labels")
    for (i in seq_along(labels)) {
      cat("  ", labels[i], "=", names(labels)[i], "\n")
    }
  }
}

# W3 democracy meaning - we know this works
cat("\nW3 q85 (confirming democracy battery):\n")
cat(substr(attr(w3$q85, "label"), 1, 100), "\n")

cat("\n\n=== SUMMARY ===\n")
cat("\nIf both exist, we can potentially use:\n")
cat("  W2 (~2005-08): q39a + q85-q88?\n")
cat("  W3 (2010-12): q33a + q85-q88\n")
cat("  W4 (2014-16): q34a + q88-q91\n")
cat("  W6 (2019-22): q34a + q85-q88\n")
