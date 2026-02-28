# Manuscript Fixes: Formatting and Integration Issues

## Issue 1: Bullet Points/Hypotheses Running Together

### The Problem

In your manuscript, hypotheses and bullet points are running together like this:

```markdown
This framework generates several testable hypotheses:
	•	H1 (Information Dominance Hypothesis): Trust in government...
	•	H2 (Infection Irrelevance Hypothesis): Personal COVID-19...
```

The tabs + bullets (•) don't render properly in Quarto. They show as a single paragraph.

### The Fix

Use proper Markdown list syntax:

```markdown
This framework generates several testable hypotheses:

- **H1 (Information Dominance Hypothesis)**: Trust in government COVID information will show stronger positive correlations with approval than personal infection or economic hardship in all three countries, regardless of regime type.

- **H2 (Infection Irrelevance Hypothesis)**: Personal COVID-19 infection will show negligible correlation with government approval, contradicting rally-around-the-flag predictions.

- **H3 (Economic Weakness Hypothesis)**: Economic hardship will show weak and inconsistent correlations with approval, contradicting economic voting predictions.

- **H4 (Authoritarian Information Advantage Hypothesis)**: Authoritarian regimes (Cambodia, Vietnam) will maintain higher trust in government COVID information than democracies (Thailand), enabling approval disconnection from performance.

- **H5 (Collective Information Hypothesis)**: The relationship between aggregate infection rates and aggregate approval operates through information environment rather than individual infection experience.
```

**Key Changes:**
1. Use `-` (hyphen-space) for bullets, NOT `•` (bullet character)
2. Add blank line before the list
3. Use `**text**` for bold hypothesis names
4. Put each hypothesis on its own line

### Apply This Fix Throughout

Search for all instances of:
- Tabs + bullets (	•)
- Numbered lists that run together
- Any list that doesn't have proper spacing

## Issue 2: Poor Integration of R Code with Text

### The Problem

Your manuscript has code chunks like this:

```r
if (!is.null(h1_results)) {
  # Convert list structure to data frame
  tibble(
    Country = names(h1_results),
    r = sapply(h1_results, function(x) x$coef)
    # ... complex code ...
  ) %>% gt()
} else {
  cat("Table data not yet available...")
}
```

**Problems:**
1. Too much code in the manuscript (violates separation of concerns)
2. Assumes structure of .rds files without documenting it
3. Doesn't integrate data values into prose
4. Generic error messages instead of helpful content

### The Fix: Better Integration Pattern

**Step 1: Verify .rds File Structure**

First, let's check what's actually in your results files:

```r
# In R Console or RStudio
library(tidyverse)
library(here)

# Load one file to see its structure
h1_results <- readRDS(here("papers/01_vietnam_covid_paradox/analysis/results/h1_infection_effects.rds"))

# Examine structure
str(h1_results)
names(h1_results)
h1_results[[1]]  # Look at first element
```

**Step 2: Create Helper Functions (in setup chunk)**

```r
```{r setup}
# ... existing setup code ...

# HELPER FUNCTIONS for cleaner manuscript code
format_correlation <- function(cor_value, p_value) {
  if (p_value < 0.001) {
    paste0("r = ", round(cor_value, 3), "***")
  } else if (p_value < 0.01) {
    paste0("r = ", round(cor_value, 3), "**")
  } else if (p_value < 0.05) {
    paste0("r = ", round(cor_value, 3), "*")
  } else {
    paste0("r = ", round(cor_value, 3))
  }
}

create_correlation_table <- function(results_list) {
  if (is.null(results_list)) {
    return(data.frame(Note = "Data not available. Render analysis scripts first."))
  }
  
  # Convert to tidy format
  tibble(
    Country = names(results_list),
    Correlation = sapply(results_list, function(x) x$coef),
    `p-value` = sapply(results_list, function(x) x$p),
    `95% CI` = sapply(results_list, function(x) 
      paste0("[", round(x$ci_lower, 3), ", ", round(x$ci_upper, 3), "]")
    ),
    N = sapply(results_list, function(x) x$n)
  )
}
```
```

**Step 3: Integrate Values into Narrative**

Instead of:
```markdown
Personal infection showed negligible relationships with approval.

```{r}
# 15 lines of complex code
```
```

Do this:
```markdown
Personal COVID-19 infection showed negligible relationships with government 
approval across all three countries. In Cambodia, the correlation was 
effectively zero (r = `r round(h1_results$Cambodia$coef, 3)`, 
p = `r round(h1_results$Cambodia$p, 2)`). Vietnam exhibited a slight 
positive correlation (r = `r round(h1_results$Vietnam$coef, 3)`), suggesting 
that infected individuals were marginally MORE likely to approve—precisely 
the opposite of what crisis theories predict.

```{r}
#| label: tbl-infection-correlation
#| tbl-cap: "Correlation: Personal COVID-19 Infection and Government Approval"

create_correlation_table(h1_results) %>%
  gt() %>%
  fmt_number(columns = Correlation, decimals = 3) %>%
  fmt_number(columns = `p-value`, decimals = 4)
```
```

**Key Improvements:**
1. ✅ Inline R code in narrative: `` `r code` ``
2. ✅ Simple, clean code chunks using helper functions
3. ✅ Actual numbers in the text, not just vague descriptions
4. ✅ Tables complement (not replace) narrative

**Step 4: Use Conditional Text for Missing Data**

Instead of empty tables, provide helpful guidance:

```markdown
```{r}
#| label: tbl-paradox
#| tbl-cap: "The Vietnam Paradox"

if (!is.null(paradox_summary)) {
  paradox_summary %>%
    gt() %>%
    fmt_number(columns = where(is.numeric), decimals = 1)
} else {
  # Create helpful placeholder table
  tibble(
    `Status` = "Analysis results not yet loaded",
    `Action Needed` = "Render the analysis scripts to generate .rds files",
    `Scripts to Run` = "03_descriptive_analysis_IMPROVED.qmd"
  ) %>%
    gt() %>%
    tab_header(title = "Data Not Yet Available")
}
```
```

## Complete Example: Before & After

### BEFORE (Poor Integration)

```markdown
Table 1 shows the paradox.

```{r}
if (!is.null(paradox_summary)) {
  paradox_summary %>%
    select(country_name, infection_rate, approval_rate) %>%
    mutate(infection_rate = infection_rate * 100) %>%
    arrange(desc(infection_rate)) %>%
    gt() %>%
    fmt_number(columns = where(is.numeric), decimals = 1) %>%
    cols_label(
      country_name = "Country",
      infection_rate = "Infection Rate (%)",
      approval_rate = "Approval Rate (%)"
    )
} else {
  cat("Table data not available.\n")
}
```
```

### AFTER (Good Integration)

```markdown
Table @tbl-paradox presents the core puzzle: Vietnam experienced the highest 
infection rate (`r if(!is.null(paradox_summary)) round(paradox_summary$infection_rate[paradox_summary$country_name=="Vietnam"]*100, 1) else "65.9"`%), 
yet maintained the highest government approval 
(`r if(!is.null(paradox_summary)) round(paradox_summary$approval_rate[paradox_summary$country_name=="Vietnam"], 1) else "97.5"`%). 
This contrasts sharply with Cambodia's successful disease control 
(`r if(!is.null(paradox_summary)) round(paradox_summary$infection_rate[paradox_summary$country_name=="Cambodia"]*100, 1) else "8.7"`% infection) 
which achieved similar approval 
(`r if(!is.null(paradox_summary)) round(paradox_summary$approval_rate[paradox_summary$country_name=="Cambodia"], 1) else "93.6"`%).

```{r}
#| label: tbl-paradox
#| tbl-cap: "The Vietnam Paradox: COVID Outcomes vs. Government Approval"

if (!is.null(paradox_summary)) {
  paradox_summary %>%
    select(country_name, infection_rate, approval_rate, trust_covid_rate) %>%
    arrange(desc(infection_rate)) %>%
    gt() %>%
    fmt_percent(columns = infection_rate, decimals = 1) %>%
    fmt_number(columns = c(approval_rate, trust_covid_rate), decimals = 1) %>%
    cols_label(
      country_name = "Country",
      infection_rate = "Infection Rate",
      approval_rate = "Approval (%)",
      trust_covid_rate = "Trust COVID Info (%)"
    ) %>%
    tab_style(
      style = cell_fill(color = "#fff9e6"),
      locations = cells_body(rows = country_name == "Vietnam")
    )
} else {
  create_placeholder_table(
    "Paradox Summary",
    "Run: 03_descriptive_analysis_IMPROVED.qmd"
  )
}
```

Vietnam's pattern defies conventional wisdom that poor pandemic outcomes 
reduce government support.
```

**Improvements:**
1. ✅ Numbers appear in prose using inline R
2. ✅ Table supplements narrative (not replaces it)
3. ✅ Fallback values if data not loaded
4. ✅ Highlights (yellow) emphasize key row
5. ✅ Natural flow: text → data → interpretation

## Action Items

### Fix Formatting (30 minutes)

1. Open `manuscript.qmd` in RStudio
2. Find all bullet lists (search for `•`)
3. Replace with proper Markdown:
   ```markdown
   - **Item 1**: Description
   - **Item 2**: Description
   ```
4. Add blank lines before/after lists
5. Re-render to verify formatting

### Improve Integration (2 hours)

1. **Add helper functions to setup chunk**
   - `format_correlation()`
   - `create_correlation_table()`
   - `create_placeholder_table()`

2. **Review each code chunk**
   - Is there complex data manipulation? → Move to helper function
   - Can values go inline in text? → Use `` `r code` ``
   - Is the table essential? → Keep it simple

3. **Add inline R values**
   - Every number mentioned should use `` `r round(value, 2)` ``
   - Provides fallback values for when .rds files don't exist yet

4. **Simplify table code**
   - Use helper functions
   - One or two pipe operations max
   - Focus on formatting, not data wrangling

## Testing Your Fixes

### Test Formatting

```bash
cd ~/Development/Research/AsianBarometer-ResearchHub/papers/01_vietnam_covid_paradox/manuscript
quarto render manuscript.qmd --to html
# Open index.html and check:
# - Are hypotheses on separate lines?
# - Are lists properly formatted?
# - Is there proper spacing?
```

### Test Integration

```r
# In RStudio
# 1. Render manuscript WITHOUT running analysis first
#    - Should show placeholder tables
#    - Should have fallback numbers in text

# 2. Run analysis scripts
source(here("papers/01_vietnam_covid_paradox/analysis/05_hypothesis_testing.qmd"))

# 3. Re-render manuscript
#    - Should show actual tables
#    - Should have actual numbers in text
#    - Should match what's in .rds files
```

## Next Steps

1. **Today**: Fix bullet point formatting (30 min)
2. **Tomorrow**: Add helper functions to setup chunk (1 hour)  
3. **This Week**: Rewrite 3-5 code chunks with better integration (3 hours)
4. **Next Week**: Complete integration for all sections (5 hours)

Start with the Theory section (hypotheses) and Results section (tables).
These are the most important for proper formatting and integration.
