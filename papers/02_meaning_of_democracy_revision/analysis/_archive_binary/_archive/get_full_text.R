# Get complete question text from W3 (which has labels)
library(haven)

w3 <- readRDS("/Users/jeffreystark/Development/Research/paper-bank/data/processed/w3.rds")

cat("=== MEANING OF DEMOCRACY BATTERY ===\n")
cat("Question: 'Many things may be desirable, but not all of them are essential\n")
cat("characteristics of democracy. If you have to choose only one from each\n")
cat("four sets of statements, which one would you choose as the most essential\n")
cat("characteristics of a democracy?'\n\n")

show_full_labels <- function(var, set_num) {
  if (is.labelled(var)) {
    labels <- attr(var, "labels")
    cat("SET", set_num, ":\n")
    for (i in seq_along(labels)) {
      val <- labels[i]
      label <- names(labels)[i]
      if (val %in% 1:4) {
        cat("  ", val, "=", label, "\n")
      }
    }
    cat("\n")
  }
}

show_full_labels(w3$q85, 1)
show_full_labels(w3$q86, 2)
show_full_labels(w3$q87, 3)
show_full_labels(w3$q88, 4)

# Also check if there's a codebook or longer labels somewhere
cat("\n=== Full label attribute check ===\n")
cat("\nQ85 full label:\n")
print(attr(w3$q85, "label"))
