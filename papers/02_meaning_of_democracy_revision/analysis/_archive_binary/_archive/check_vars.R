# Quick check of variable names across waves
library(tidyverse)
library(haven)

data_dir <- "/Users/jeffreystark/Development/Research/paper-bank/data/processed"

w3 <- readRDS(file.path(data_dir, "w3.rds"))
w4 <- readRDS(file.path(data_dir, "w4.rds"))
w6 <- readRDS(file.path(data_dir, "w6.rds"))

cat("=== Demographic variable check ===\n\n")

# Gender (se2)
cat("Gender variable:\n")
cat("  W3 se2:", "se2" %in% names(w3), "\n")
cat("  W4 se2:", "se2" %in% names(w4), "\n")
cat("  W6 se2:", "se2" %in% names(w6), "\n")

# Birth year (se3)
cat("\nBirth year variable:\n")
cat("  W3 se3:", "se3" %in% names(w3), "\n")
cat("  W4 se3:", "se3" %in% names(w4), "\n")
cat("  W6 se3:", "se6" %in% names(w6), "\n")

# W4 - look for similar vars
cat("\nW4 variables starting with 'se':\n")
w4_se <- names(w4)[str_detect(names(w4), "^[Ss][Ee][0-9]")]
print(head(w4_se, 30))

# Check case sensitivity
cat("\nW4 all lowercase 'se' vars:\n")
w4_se_lower <- names(w4)[str_detect(tolower(names(w4)), "^se[0-9]")]
print(head(w4_se_lower, 30))

# Just list first 50 W4 variable names
cat("\nW4 first 50 variable names:\n")
print(names(w4)[1:50])

cat("\nW4 variable names containing '2' or '3':\n")
w4_nums <- names(w4)[str_detect(names(w4), "[23]")]
print(head(w4_nums, 30))
