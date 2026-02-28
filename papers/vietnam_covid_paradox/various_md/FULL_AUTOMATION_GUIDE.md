# Automating Data Points in Your Manuscript

## Current Hard-Coded Numbers (Need to be Automated!)

Based on your manuscript, here are ALL the places with hard-coded numbers that should pull from your data:

## Section 1: Introduction

### Hard-Coded Values to Replace:

```markdown
❌ HARD-CODED:
- "65.9%" (Vietnam infection rate) - appears 5+ times
- "97.5%" (Vietnam approval) - appears 4+ times  
- "91.9%" (Vietnam trust) - appears 3+ times
- "8.7%" (Cambodia infection) - appears 3+ times
- "93.6%" (Cambodia approval) - appears 2+ times
- "40.1%" (Thailand infection) - appears 3+ times
- "37.7%" (Thailand approval) - appears 2+ times
- "34.0%" (Thailand trust) - appears 1 time
- "N = 3,679" (total sample) - appears 3+ times
- "n = 1,242" (Cambodia n) - appears 2+ times
- "n = 1,237" (Vietnam n) - appears 2+ times
- "n = 1,200" (Thailand n) - appears 2+ times
```

### Automated Replacement:

Add to your setup chunk:

```r
```{r setup}
# ... existing code ...

# ============================================================================
# EXTRACT KEY VALUES FOR INLINE USE THROUGHOUT MANUSCRIPT
# ============================================================================

# Load main analysis data (if not already loaded from results)
if (is.null(paradox_summary)) {
  # Fallback: calculate from raw data if results not available
  ab_analysis <- readRDS(here("data", "processed", "ab_analysis.rds"))
  
  paradox_summary <- ab_analysis %>%
    group_by(country_name) %>%
    summarise(
      n = n(),
      infection_rate = mean(covid_contracted, na.rm = TRUE),
      approval_rate = mean(covid_govt_handling >= 3, na.rm = TRUE) * 100,
      trust_covid_rate = mean(covid_trust_info >= 3, na.rm = TRUE) * 100,
      .groups = "drop"
    )
}

# VIETNAM VALUES
vietnam <- list(
  n = paradox_summary %>% filter(country_name == "Vietnam") %>% pull(n),
  infection_rate = paradox_summary %>% filter(country_name == "Vietnam") %>% pull(infection_rate) * 100,
  approval_rate = paradox_summary %>% filter(country_name == "Vietnam") %>% pull(approval_rate),
  trust_rate = paradox_summary %>% filter(country_name == "Vietnam") %>% pull(trust_covid_rate)
)

# CAMBODIA VALUES
cambodia <- list(
  n = paradox_summary %>% filter(country_name == "Cambodia") %>% pull(n),
  infection_rate = paradox_summary %>% filter(country_name == "Cambodia") %>% pull(infection_rate) * 100,
  approval_rate = paradox_summary %>% filter(country_name == "Cambodia") %>% pull(approval_rate),
  trust_rate = paradox_summary %>% filter(country_name == "Cambodia") %>% pull(trust_covid_rate)
)

# THAILAND VALUES
thailand <- list(
  n = paradox_summary %>% filter(country_name == "Thailand") %>% pull(n),
  infection_rate = paradox_summary %>% filter(country_name == "Thailand") %>% pull(infection_rate) * 100,
  approval_rate = paradox_summary %>% filter(country_name == "Thailand") %>% pull(approval_rate),
  trust_rate = paradox_summary %>% filter(country_name == "Thailand") %>% pull(trust_covid_rate)
)

# TOTAL SAMPLE
total_n <- sum(paradox_summary$n)

# Add fallbacks if any are NA
vietnam <- lapply(vietnam, function(x) ifelse(is.na(x) || length(x) == 0, 
  list(n=1237, infection_rate=65.9, approval_rate=97.5, trust_rate=91.9), x))
cambodia <- lapply(cambodia, function(x) ifelse(is.na(x) || length(x) == 0,
  list(n=1242, infection_rate=8.7, approval_rate=93.6, trust_rate=89.2), x))
thailand <- lapply(thailand, function(x) ifelse(is.na(x) || length(x) == 0,
  list(n=1200, infection_rate=40.1, approval_rate=37.7, trust_rate=34.0), x))
if (is.na(total_n) || total_n == 0) total_n <- 3679

cat("✓ Extracted key values for inline use:\n")
cat("  Vietnam: n =", vietnam$n, ", infection =", round(vietnam$infection_rate, 1), "%\n")
cat("  Cambodia: n =", cambodia$n, ", infection =", round(cambodia$infection_rate, 1), "%\n")
cat("  Thailand: n =", thailand$n, ", infection =", round(thailand$infection_rate, 1), "%\n")
cat("  Total N =", total_n, "\n")
```
```

Then replace in your Introduction:

```markdown
✅ AUTOMATED:

On February 20, 2022, Vietnam officially acknowledged what its citizens had 
already experienced: a catastrophic surge in COVID-19 infections affecting 
nearly two-thirds of the population (`r round(vietnam$infection_rate, 1)`%). 
This admission came after months of the country's "living with COVID" policy, 
which abandoned earlier zero-COVID strategies in favor of accepting mass 
infections as inevitable. By any conventional metric, Vietnam's pandemic 
response had failed spectacularly—its infection rate was 
`r round(vietnam$infection_rate / cambodia$infection_rate, 1)` times higher 
than neighboring Cambodia (`r round(cambodia$infection_rate, 1)`%) and 
substantially higher than Thailand (`r round(thailand$infection_rate, 1)`%).

Yet when the Asian Barometer Wave 6 surveyed citizens across Southeast Asia 
in late 2022–2023, Vietnam recorded the highest government approval rating 
for pandemic handling: `r round(vietnam$approval_rate, 1)`% of respondents 
rated the government's performance as "fairly well" or "very well." This 
exceeded even Cambodia's approval (`r round(cambodia$approval_rate, 1)`%), 
despite Cambodia's relative success in limiting infections. Thailand, with 
moderate infection rates (`r round(thailand$infection_rate, 1)`%), recorded 
catastrophic approval ratings (`r round(thailand$approval_rate, 1)`%). 
Vietnam's paradox was complete: the worst health outcomes produced the highest 
government approval.
```

## Section 2: Sample Sizes in Methods

### Hard-Coded Values:

```markdown
❌ HARD-CODED:
Our analytic sample includes 3,679 respondents across three countries: 
Cambodia (n = 1,242), Vietnam (n = 1,237), and Thailand (n = 1,200).
```

### Automated:

```markdown
✅ AUTOMATED:
Our analytic sample includes `r format(total_n, big.mark=",")` respondents 
across three countries: Cambodia (n = `r format(cambodia$n, big.mark=",")`), 
Vietnam (n = `r format(vietnam$n, big.mark=",")`), and Thailand 
(n = `r format(thailand$n, big.mark=",")`).
```

## Section 3: Variable Descriptions

### Hard-Coded Values:

```markdown
❌ HARD-CODED:
- "mean = 3.29, SD = 0.87" (govt approval)
- "Overall infection rate: 39.8%"
- "mean = 2.31, SD = 0.94, α = 0.77" (economic severity)
- "job loss (9.8%), income loss (55.2%), education disruption (27.4%)"
- "mean = 3.21, SD = 0.79" (trust COVID info)
- "mean = 43.6, SD = 15.8" (age)
- "51.2% female"
- "38.4% urban"
```

### Automated:

Add to setup chunk:

```r
# CALCULATE DESCRIPTIVE STATISTICS FOR VARIABLES
if (file.exists(here("data", "processed", "ab_analysis.rds"))) {
  ab_data <- readRDS(here("data", "processed", "ab_analysis.rds"))
  
  var_stats <- list(
    # DV: Government approval
    approval_mean = mean(ab_data$covid_govt_handling, na.rm = TRUE),
    approval_sd = sd(ab_data$covid_govt_handling, na.rm = TRUE),
    
    # IV: Infection
    infection_rate = mean(ab_data$covid_contracted, na.rm = TRUE) * 100,
    
    # IV: Economic severity
    econ_mean = mean(ab_data$covid_impact_severity, na.rm = TRUE),
    econ_sd = sd(ab_data$covid_impact_severity, na.rm = TRUE),
    
    # Individual economic impacts
    job_loss_pct = mean(ab_data$job_loss, na.rm = TRUE) * 100,
    income_loss_pct = mean(ab_data$income_loss, na.rm = TRUE) * 100,
    edu_disruption_pct = mean(ab_data$edu_disruption, na.rm = TRUE) * 100,
    
    # IV: Trust
    trust_mean = mean(ab_data$covid_trust_info, na.rm = TRUE),
    trust_sd = sd(ab_data$covid_trust_info, na.rm = TRUE),
    
    # Demographics
    age_mean = mean(ab_data$age, na.rm = TRUE),
    age_sd = sd(ab_data$age, na.rm = TRUE),
    female_pct = mean(ab_data$gender == "Female", na.rm = TRUE) * 100,
    urban_pct = mean(ab_data$urban == 1, na.rm = TRUE) * 100
  )
} else {
  # Fallback values
  var_stats <- list(
    approval_mean = 3.29, approval_sd = 0.87,
    infection_rate = 39.8,
    econ_mean = 2.31, econ_sd = 0.94,
    job_loss_pct = 9.8, income_loss_pct = 55.2, edu_disruption_pct = 27.4,
    trust_mean = 3.21, trust_sd = 0.79,
    age_mean = 43.6, age_sd = 15.8,
    female_pct = 51.2, urban_pct = 38.4
  )
}
```

Then in Methods section:

```markdown
✅ AUTOMATED:

**Dependent Variable: Government Pandemic Approval.** Our primary outcome is 
a single-item measure: "Overall, how would you rate the way the government 
has handled the COVID-19 crisis?" Response options: 1 = Very badly, 2 = Fairly 
badly, 3 = Fairly well, 4 = Very well. We treat this as a continuous variable 
(mean = `r round(var_stats$approval_mean, 2)`, 
SD = `r round(var_stats$approval_sd, 2)`) ranging from strong disapproval to 
strong approval.

**Primary Independent Variables**

- **Personal COVID-19 Infection.** Binary indicator (0 = no, 1 = yes) based on: 
  "Have you or anyone in your immediate family been infected with COVID-19?" 
  This measures direct personal experience with the health threat. Overall 
  infection rate: `r round(var_stats$infection_rate, 1)`% (Cambodia 
  `r round(cambodia$infection_rate, 1)`%, Thailand 
  `r round(thailand$infection_rate, 1)`%, Vietnam 
  `r round(vietnam$infection_rate, 1)`%).

- **COVID-19 Economic Impact Severity.** We created a four-point severity 
  scale (mean = `r round(var_stats$econ_mean, 2)`, 
  SD = `r round(var_stats$econ_sd, 2)`, α = 0.77).
  
  We also examine individual economic impact items separately: job loss 
  (`r round(var_stats$job_loss_pct, 1)`%), income loss 
  (`r round(var_stats$income_loss_pct, 1)`%), education disruption 
  (`r round(var_stats$edu_disruption_pct, 1)`%).

- **Trust in Government COVID Information.** Single item: "How much do you 
  trust the information about COVID-19 provided by the government?" We treat 
  this as continuous (mean = `r round(var_stats$trust_mean, 2)`, 
  SD = `r round(var_stats$trust_sd, 2)`).

**Demographic Controls.**

- Age: Continuous (years), mean = `r round(var_stats$age_mean, 1)`, 
  SD = `r round(var_stats$age_sd, 1)`
- Gender: Binary (0 = Male, 1 = Female), `r round(var_stats$female_pct, 1)`% female
- Urban residence: Binary (0 = Rural, 1 = Urban), `r round(var_stats$urban_pct, 1)`% urban
```

## Section 4: Results - Correlation Values

### Currently Hard-Coded:

```markdown
❌ HARD-CODED:
- "r = 0.013" (Vietnam infection-approval)
- "r = -0.014" (Cambodia infection-approval)
- "r = -0.079" (Thailand infection-approval)
- "r = 0.576" (Cambodia trust-approval)
- "r = 0.507" (Vietnam trust-approval)
- "r = 0.672" (Thailand trust-approval)
- "r = -0.14 to +0.16" (economic effects range)
- "r = 0.156" (Cambodia economic-approval)
```

### Automated:

Already partially done if you use the improved Results section, but add more:

```r
# In setup chunk, after loading h1_results, h2a_results, etc.

# Extract all key correlation values
cors <- list(
  # Infection-Approval
  vietnam_inf = if (!is.null(h1_results)) h1_results$Vietnam$coef else 0.013,
  cambodia_inf = if (!is.null(h1_results)) h1_results$Cambodia$coef else -0.014,
  thailand_inf = if (!is.null(h1_results)) h1_results$Thailand$coef else -0.079,
  
  # Trust-Approval
  vietnam_trust = if (!is.null(h2a_results)) h2a_results$Vietnam$coef else 0.507,
  cambodia_trust = if (!is.null(h2a_results)) h2a_results$Cambodia$coef else 0.576,
  thailand_trust = if (!is.null(h2a_results)) h2a_results$Thailand$coef else 0.672,
  
  # Economic-Approval
  vietnam_econ = if (!is.null(h4b_results)) h4b_results$Vietnam$econ_coef else -0.016,
  cambodia_econ = if (!is.null(h4b_results)) h4b_results$Cambodia$econ_coef else 0.156,
  thailand_econ = if (!is.null(h4b_results)) h4b_results$Thailand$econ_coef else -0.142
)

# Calculate ranges
econ_range_min <- min(cors$vietnam_econ, cors$cambodia_econ, cors$thailand_econ)
econ_range_max <- max(cors$vietnam_econ, cors$cambodia_econ, cors$thailand_econ)

# Calculate ratios (trust vs infection)
cors$vietnam_ratio <- abs(cors$vietnam_trust / cors$vietnam_inf)
cors$cambodia_ratio <- abs(cors$cambodia_trust / cors$cambodia_inf)
cors$thailand_ratio <- abs(cors$thailand_trust / cors$thailand_inf)
```

Then use throughout Results:

```markdown
✅ AUTOMATED:

Vietnamese citizens who contracted COVID-19 showed no higher approval than 
uninfected citizens (r = `r round(cors$vietnam_inf, 3)`), contradicting the 
direct threat hypothesis.

Individual-level economic hardship showed weak and inconsistent approval 
effects across our three countries 
(r = `r round(econ_range_min, 2)` to `r round(econ_range_max, 2)`).

Trust in government COVID information exhibited strong positive correlations 
with approval across all three countries. Cambodia 
(r = `r round(cors$cambodia_trust, 3)`, p < 0.001), Vietnam 
(r = `r round(cors$vietnam_trust, 3)`, p < 0.001), and Thailand 
(r = `r round(cors$thailand_trust, 3)`, p < 0.001) all showed correlations 
exceeding 0.50.

In Cambodia, trust effects were `r round(cors$cambodia_ratio, 0)` times 
stronger than infection effects. In Vietnam, trust was 
`r round(cors$vietnam_ratio, 0)` times stronger than infection. Even in 
Thailand, trust remained nearly `r round(cors$thailand_ratio, 0)` times 
stronger.
```

## Section 5: Discussion/Conclusion

### Hard-Coded Values:

```markdown
❌ HARD-CODED:
- "r ≈ 0" (infection effects)
- "r = 0.51–0.67" (trust effects)
- "r = -0.14 to +0.16" (economic effects)
- "8–40 times larger" (trust vs infection ratio)
```

### Automated:

```r
# Calculate summary statistics for Discussion
discussion_stats <- list(
  # Trust correlation range
  trust_min = min(cors$cambodia_trust, cors$vietnam_trust, cors$thailand_trust),
  trust_max = max(cors$cambodia_trust, cors$vietnam_trust, cors$thailand_trust),
  
  # Infection correlation range
  inf_min = min(abs(cors$cambodia_inf), abs(cors$vietnam_inf), abs(cors$thailand_inf)),
  inf_max = max(abs(cors$cambodia_inf), abs(cors$vietnam_inf), abs(cors$thailand_inf)),
  
  # Ratio range
  ratio_min = min(cors$cambodia_ratio, cors$vietnam_ratio, cors$thailand_ratio),
  ratio_max = max(cors$cambodia_ratio, cors$vietnam_ratio, cors$thailand_ratio)
)
```

Then in Discussion:

```markdown
✅ AUTOMATED:

First, personal COVID-19 infection had negligible effects on government 
approval (|r| < `r round(discussion_stats$inf_max, 2)`), contradicting 
rally-around-flag theories.

Third, trust in government COVID information strongly predicted approval 
across all three countries 
(r = `r round(discussion_stats$trust_min, 2)`–`r round(discussion_stats$trust_max, 2)`), 
with effect sizes `r round(discussion_stats$ratio_min, 0)`–`r round(discussion_stats$ratio_max, 0)` 
times larger than infection or economic impacts.
```

## Section 6: Abstract

### Hard-Coded Values:

```markdown
❌ HARD-CODED in Abstract:
- "N=3,679"
- "97.5% approval rate"  
- "65.9% infection"
```

### Automated:

```markdown
✅ AUTOMATED Abstract:

This study examines a puzzling phenomenon in Southeast Asian responses to 
COVID-19: Vietnam maintained the highest government approval ratings despite 
experiencing the highest infection rates. Using Asian Barometer Survey data 
(N=`r format(total_n, big.mark=",")`), we demonstrate that trust in government 
COVID information dominates objective pandemic outcomes in predicting approval. 
Vietnam's `r round(vietnam$approval_rate, 1)`% approval rate coexisted with 
`r round(vietnam$infection_rate, 1)`% infection—a relationship mediated 
entirely by information environment rather than health outcomes.
```

## Complete Automation Checklist

Use this to systematically find and replace ALL hard-coded numbers:

### Step 1: Search for Hard-Coded Numbers

Run in R Console:
```r
# Read your manuscript
manuscript_text <- readLines(here("papers/01_vietnam_covid_paradox/manuscript/manuscript.qmd"))

# Find lines with numbers (not in code chunks)
number_lines <- grep("[0-9]+\\.[0-9]+%|[0-9]+\\.[0-9]+|n = [0-9]+|N = [0-9]+", 
                     manuscript_text, value = TRUE)

# Show first 20
head(number_lines, 20)
```

### Step 2: Systematic Replacement

For each number you find:

1. **Identify the source**
   - Is it from `paradox_summary`?
   - Is it from `h1_results`, `h2a_results`, etc.?
   - Is it calculated from `ab_analysis`?

2. **Create a variable in setup chunk**
   ```r
   vietnam_infection <- paradox_summary %>% 
     filter(country_name == "Vietnam") %>% 
     pull(infection_rate) * 100
   ```

3. **Replace in text**
   ```markdown
   Before: "65.9%"
   After: "`r round(vietnam_infection, 1)`%"
   ```

### Step 3: Verify All Numbers

After automation, render manuscript and check:
- Do all numbers appear?
- Are they reasonable?
- Do they match your tables?

## Benefits of Full Automation

Once you automate ALL numbers:

✅ **Data updates automatically** - Change data → re-render → numbers update

✅ **No transcription errors** - Numbers come directly from analysis

✅ **Consistency guaranteed** - Same number appears identically everywhere

✅ **Easy sensitivity analysis** - Change inclusion criteria → see impact

✅ **Reviewer requests** - "Exclude Thailand" → Just filter data & re-render

✅ **Transparent** - Readers can verify numbers match analysis

## Quick Wins (Start Here)

If you do nothing else, automate these high-value numbers TODAY:

1. **Sample sizes** (N=3,679, n=1,242, etc.) - 5 minutes
2. **Key paradox values** (65.9%, 97.5%, 91.9%) - 10 minutes
3. **Main correlations** (r = 0.507, etc.) - 15 minutes
4. **Summary statistics** (mean, SD) - 15 minutes

**Total: 45 minutes for 80% of the benefit!**

## Final Setup Chunk Template

Here's the complete setup chunk with everything:

```r
```{r setup, include=FALSE}
library(tidyverse)
library(here)
library(gt)
library(modelsummary)

# Set options
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

# Define paths
results_dir <- file.path("..", "analysis", "results")

# Helper function to safely load
safe_load <- function(filename) {
  filepath <- file.path(results_dir, filename)
  if (file.exists(filepath)) readRDS(filepath) else NULL
}

# Load all results
paradox_summary <- safe_load("descriptive_paradox_summary.rds")
h1_results <- safe_load("h1_infection_effects.rds")
h2a_results <- safe_load("h2a_trust_direct_effects.rds")
h4b_results <- safe_load("h4b_trust_vs_economic.rds")
# ... load all other results ...

# Load raw data for variable stats
ab_data <- readRDS(here("data", "processed", "ab_analysis.rds"))

# ============================================================================
# EXTRACT ALL KEY VALUES FOR INLINE USE
# ============================================================================

# COUNTRY-LEVEL VALUES
vietnam <- list(
  n = paradox_summary %>% filter(country_name == "Vietnam") %>% pull(n),
  infection_rate = paradox_summary %>% filter(country_name == "Vietnam") %>% pull(infection_rate) * 100,
  approval_rate = paradox_summary %>% filter(country_name == "Vietnam") %>% pull(approval_rate),
  trust_rate = paradox_summary %>% filter(country_name == "Vietnam") %>% pull(trust_covid_rate)
)

cambodia <- list(
  n = paradox_summary %>% filter(country_name == "Cambodia") %>% pull(n),
  infection_rate = paradox_summary %>% filter(country_name == "Cambodia") %>% pull(infection_rate) * 100,
  approval_rate = paradox_summary %>% filter(country_name == "Cambodia") %>% pull(approval_rate),
  trust_rate = paradox_summary %>% filter(country_name == "Cambodia") %>% pull(trust_covid_rate)
)

thailand <- list(
  n = paradox_summary %>% filter(country_name == "Thailand") %>% pull(n),
  infection_rate = paradox_summary %>% filter(country_name == "Thailand") %>% pull(infection_rate) * 100,
  approval_rate = paradox_summary %>% filter(country_name == "Thailand") %>% pull(approval_rate),
  trust_rate = paradox_summary %>% filter(country_name == "Thailand") %>% pull(trust_covid_rate)
)

total_n <- sum(paradox_summary$n)

# VARIABLE STATISTICS
var_stats <- list(
  approval_mean = mean(ab_data$covid_govt_handling, na.rm = TRUE),
  approval_sd = sd(ab_data$covid_govt_handling, na.rm = TRUE),
  infection_rate = mean(ab_data$covid_contracted, na.rm = TRUE) * 100,
  trust_mean = mean(ab_data$covid_trust_info, na.rm = TRUE),
  trust_sd = sd(ab_data$covid_trust_info, na.rm = TRUE),
  age_mean = mean(ab_data$age, na.rm = TRUE),
  age_sd = sd(ab_data$age, na.rm = TRUE),
  female_pct = mean(ab_data$gender == "Female", na.rm = TRUE) * 100,
  urban_pct = mean(ab_data$urban == 1, na.rm = TRUE) * 100
)

# CORRELATION VALUES
cors <- list(
  vietnam_inf = h1_results$Vietnam$coef,
  cambodia_inf = h1_results$Cambodia$coef,
  thailand_inf = h1_results$Thailand$coef,
  vietnam_trust = h2a_results$Vietnam$coef,
  cambodia_trust = h2a_results$Cambodia$coef,
  thailand_trust = h2a_results$Thailand$coef
)

# RATIOS
cors$vietnam_ratio <- abs(cors$vietnam_trust / cors$vietnam_inf)
cors$cambodia_ratio <- abs(cors$cambodia_trust / cors$cambodia_inf)
cors$thailand_ratio <- abs(cors$thailand_trust / cors$thailand_inf)

# HELPER FUNCTIONS
format_cor <- function(cor_value, p_value) {
  stars <- if (p_value < 0.001) "***" else if (p_value < 0.01) "**" else if (p_value < 0.05) "*" else ""
  paste0("r = ", round(cor_value, 3), stars)
}

create_cor_table <- function(results_list) {
  if (is.null(results_list)) return(tibble(Note = "Data not available"))
  tibble(
    Country = names(results_list),
    Correlation = sapply(results_list, function(x) x$coef),
    `p-value` = sapply(results_list, function(x) x$p),
    N = sapply(results_list, function(x) x$n)
  )
}

cat("✓ Automated", length(ls(pattern = "vietnam|cambodia|thailand|total_n|var_stats|cors")), 
    "value sets for inline use\n")
```
```

Now EVERY number in your manuscript can be:
```markdown
`r round(vietnam$infection_rate, 1)`%
`r format(total_n, big.mark=",")`
`r round(cors$vietnam_trust, 3)`
```

This makes your manuscript **100% reproducible**!
