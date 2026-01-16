# Check the actual question text and labels for the meaning of democracy battery
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/econdev-authpref/data/processed"

w6 <- readRDS(file.path(data_dir, "w6.rds"))
w3 <- readRDS(file.path(data_dir, "w3.rds"))

cat("=== W6 Question Labels ===\n\n")

# Q85 (Set 1)
cat("Q85 (Set 1):\n")
cat("Variable label:", attr(w6$q85, "label"), "\n")
cat("Value labels:\n")
if (is.labelled(w6$q85)) {
  labels <- attr(w6$q85, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

cat("\n---\n\n")

# Q86 (Set 2)
cat("Q86 (Set 2):\n")
cat("Variable label:", attr(w6$q86, "label"), "\n")
cat("Value labels:\n")
if (is.labelled(w6$q86)) {
  labels <- attr(w6$q86, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

cat("\n---\n\n")

# Q87 (Set 3)
cat("Q87 (Set 3):\n")
cat("Variable label:", attr(w6$q87, "label"), "\n")
cat("Value labels:\n")
if (is.labelled(w6$q87)) {
  labels <- attr(w6$q87, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

cat("\n---\n\n")

# Q88 (Set 4)
cat("Q88 (Set 4):\n")
cat("Variable label:", attr(w6$q88, "label"), "\n")
cat("Value labels:\n")
if (is.labelled(w6$q88)) {
  labels <- attr(w6$q88, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

cat("\n\n=== W3 Question Labels (for comparison) ===\n\n")

# Q85 (Set 1)
cat("Q85 (Set 1):\n")
cat("Variable label:", attr(w3$q85, "label"), "\n")
cat("Value labels:\n")
if (is.labelled(w3$q85)) {
  labels <- attr(w3$q85, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}

cat("\n---\n\n")

# Q86 (Set 2)
cat("Q86 (Set 2):\n")
cat("Variable label:", attr(w3$q86, "label"), "\n")
cat("Value labels:\n")
if (is.labelled(w3$q86)) {
  labels <- attr(w3$q86, "labels")
  for (i in seq_along(labels)) {
    cat("  ", labels[i], "=", names(labels)[i], "\n")
  }
}
