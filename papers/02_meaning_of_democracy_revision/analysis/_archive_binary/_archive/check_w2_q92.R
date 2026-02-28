# Check W2 q92 - the single-item democracy meaning question
library(haven)
library(tidyverse)

data_dir <- "/Users/jeffreystark/Development/Research/paper-bank/data/processed"
w2 <- readRDS(file.path(data_dir, "w2.rds"))

cat("=== W2 Q92 CHECK ===\n\n")

cat("q92 exists in data:", "q92" %in% names(w2), "\n")

if ("q92" %in% names(w2)) {
  cat("\nLabel:", attr(w2$q92, "label"), "\n")
  
  cat("\nValue labels:\n")
  if (is.labelled(w2$q92)) {
    labels <- attr(w2$q92, "labels")
    for (i in seq_along(labels)) {
      cat("  ", labels[i], "=", names(labels)[i], "\n")
    }
  }
  
  cat("\nDistribution:\n")
  print(table(as.integer(w2$q92), useNA = "ifany"))
}

# Also check q91_1, q91_2, q91_3
cat("\n\n=== Q91 OPEN-ENDED CHECK ===\n")
for (v in c("q91_1", "q91_2", "q91_3", "q91")) {
  if (v %in% names(w2)) {
    cat("\n", v, "exists\n")
    cat("  Label:", attr(w2[[v]], "label"), "\n")
    cat("  First 10 values:\n")
    print(head(w2[[v]], 10))
  }
}

# Check all variables with "92" in name
cat("\n\n=== ALL VARIABLES CONTAINING '92' ===\n")
vars_92 <- names(w2)[str_detect(names(w2), "92")]
print(vars_92)

for (v in vars_92) {
  cat("\n", v, ":\n")
  cat("  Label:", attr(w2[[v]], "label"), "\n")
  if (is.labelled(w2[[v]])) {
    cat("  Value labels present: YES\n")
  }
  cat("  Distribution:\n")
  print(table(as.integer(w2[[v]]), useNA = "ifany"))
}

# Check around q90-q95
cat("\n\n=== CHECKING Q90-Q95 ===\n")
for (v in paste0("q", 90:95)) {
  if (v %in% names(w2)) {
    lab <- attr(w2[[v]], "label")
    cat(v, ":", substr(lab, 1, 80), "\n")
  }
}
