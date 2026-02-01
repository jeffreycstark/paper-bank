# Quick debug of W6 country variable
library(haven)

w6 <- readRDS("/Users/jeffreystark/Development/Research/econdev-authpref/data/processed/w6.rds")

cat("W6 country variable structure:\n")
cat("Class:", class(w6$country), "\n")
cat("First 10 values:\n")
print(head(w6$country, 10))

# Check if it's character
if (is.character(w6$country)) {
  cat("\nUnique country values:\n")
  print(unique(w6$country))
}

# Check for alternative country variable
cat("\nSearching for country-like variables:\n")
country_vars <- names(w6)[str_detect(tolower(names(w6)), "country|nation|ctry")]
print(country_vars)

# Check each
for (v in country_vars) {
  cat("\n", v, ":\n")
  print(head(w6[[v]], 5))
  if (is.labelled(w6[[v]])) {
    cat("Labels:\n")
    print(attr(w6[[v]], "labels"))
  }
}
