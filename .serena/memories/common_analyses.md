# Common Analysis Patterns

## Loading the Dataset
```r
d <- readRDS("data/processed/abs_econdev_authpref.rds")
```

## Cross-tabulations by Country
```r
# Country x Variable
table(d$country, d$variable_name, useNA = "ifany")

# With row percentages
tab <- table(d$country, d$variable_name)
round(prop.table(tab, 1) * 100, 1)
```

## Filtering by Country and Wave
```r
# Korea Wave 6
korea_w6 <- d[d$country == 3 & d$wave == "w6", ]

# Young people (under 30)
young <- d[d$age < 30 & !is.na(d$age), ]

# Combine filters
korea_young_w6 <- d[d$country == 3 & d$wave == "w6" & d$age < 30 & !is.na(d$age), ]
```

## Age Cohorts
```r
d$cohort <- cut(d$age, 
                breaks = c(17, 29, 39, 49, 59, 69, 79, 100),
                labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"))
table(d$cohort)
```

## Actual Participation (Political Action)
```r
# Codes 3-5 = actually did the action
did_demo <- d$action_demonstration %in% c(3, 4, 5)
did_petition <- d$action_petition %in% c(3, 4, 5)
did_either <- did_demo | did_petition

sum(did_demo, na.rm = TRUE)  # N who demonstrated
mean(did_demo, na.rm = TRUE) # Proportion
```

## By Gender Analysis
```r
# 1 = Male, 2 = Female
for (g in c(1, 2)) {
  subset <- d[d$gender == g & !is.na(d$gender), ]
  cat(ifelse(g == 1, "Male", "Female"), "N =", nrow(subset), "\n")
}
```

## Variable Coverage Check
```r
# Check valid N per wave
for (w in sort(unique(d$wave))) {
  wd <- d[d$wave == w, ]
  valid <- sum(!is.na(wd$variable_name))
  cat(w, ":", valid, "/", nrow(wd), "\n")
}
```

## Check Raw Data
```r
# Load raw SPSS file for verification
library(haven)
raw <- read_sav("data/raw/wave6/W6_Korea_Release_20241220.sav")

# Check variable labels
attr(raw$q76, "label")
attr(raw$q76, "labels")
table(raw$q76, useNA = "ifany")
```
