# Analysis → Manuscript Integration Workflow

## Overview

This project now uses the **Save/Load Pattern** to avoid code duplication between analysis scripts and the manuscript. Analysis code exists **only once** in `analysis/*.qmd` files, and results are saved as `.rds` files for the manuscript to load.

## The Problem We Solved

❌ **Before:** Copying code from analysis to manuscript created two versions to maintain
✅ **After:** Analysis scripts save results → Manuscript loads results (single source of truth)

## File Structure

```
papers/01_vietnam_covid_paradox/
├── analysis/
│   ├── 03_descriptive_analysis_IMPROVED.qmd  → Saves 7 result files
│   ├── 05_hypothesis_testing.qmd             → Saves 10 result files
│   ├── 06_mediation_analysis.qmd             → Saves 7 result files
│   ├── 07_country_comparisons.qmd            → Saves 7 result files
│   ├── 08_robustness_checks.qmd              → Saves 8 result files
│   └── results/
│       ├── RESULTS_MANIFEST.md               → Documents all .rds files
│       ├── descriptive_paradox_summary.rds
│       ├── h1_infection_effects_by_country.rds
│       ├── mediation_bootstrap_results.rds
│       └── ... (39 total .rds files)
└── manuscript/
    ├── manuscript.qmd                         → Loads all .rds files
    └── MANUSCRIPT_SETUP_TEMPLATE.md           → Template for setup chunk
```

## Complete Workflow

### Step 1: Update Analysis Scripts (DONE ✅)

Each analysis script now has a "Save Results for Manuscript" section at the end that saves key objects:

```r
# Save Results for Manuscript

```{r 99-save-results-for-manuscript}
results_dir <- here("papers/01_vietnam_covid_paradox/analysis/results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(paradox_data, file.path(results_dir, "descriptive_paradox_summary.rds"))
saveRDS(h1_results, file.path(results_dir, "h1_infection_effects_by_country.rds"))
# ... [saves all key results]
```
```

### Step 2: Update Manuscript Setup (TO DO)

Add the comprehensive setup chunk to `manuscript.qmd`:

1. Open `manuscript/MANUSCRIPT_SETUP_TEMPLATE.md`
2. Copy the complete setup chunk
3. Paste it into `manuscript/manuscript.qmd` right after the YAML header (after `---`)

This loads all 39 pre-computed result objects.

### Step 3: Use Results in Manuscript

Replace code chunks in the manuscript with formatted display of loaded results:

**Before (duplicated code):**
```r
```{r}
# ❌ This duplicates analysis code!
paradox_data <- ab_analysis %>%
  group_by(country_name) %>%
  summarise(
    infection_rate = mean(covid_contracted) * 100,
    approval = mean(covid_govt_handling >= 3) * 100
  )

paradox_data %>% gt()
```
```

**After (load and display):**
```r
```{r}
#| label: tbl-paradox
#| tbl-cap: "The Vietnam Paradox"

# ✅ Just format pre-loaded data!
paradox_summary %>%
  gt() %>%
  fmt_number(columns = c(infection_rate, approval), decimals = 1)
```
```

### Step 4: Daily Workflow

**When you change analysis:**

1. Edit `analysis/05_hypothesis_testing.qmd` (or any analysis script)
2. Click **Render** → creates/updates `.rds` files in `results/`
3. Switch to `manuscript/manuscript.qmd`
4. Click **Render** → loads updated results
5. ✅ **Manuscript automatically has your latest analysis!**

**No manual copying. No synchronization. One source of truth.**

## What Gets Saved? (39 Result Objects)

### Descriptive Analysis (7 objects)
- Paradox summary table
- Sample characteristics
- Scale reliability
- Correlation matrix
- Bootstrap correlations
- Effect sizes
- Economic impacts

### Hypothesis Testing (10 objects)
- H1-H4 test results
- Regression models (core + full)
- Interaction models
- Hypothesis summary table

### Mediation Analysis (7 objects)
- Path coefficients
- Bootstrap results
- Sensitivity analysis
- Moderated mediation
- Cross-country comparison

### Country Comparisons (7 objects)
- Country-specific correlations
- Fisher's Z tests
- Publication summary table

### Robustness Checks (8 objects)
- Missing data analysis
- Alternative specifications
- Outlier diagnostics
- Sensitivity tests

See `analysis/results/RESULTS_MANIFEST.md` for complete details.

## Testing the Workflow

### Test 1: Render Descriptive Analysis

```bash
# Navigate to the analysis directory
cd papers/01_vietnam_covid_paradox/analysis

# Render the descriptive analysis (creates .rds files)
quarto render 03_descriptive_analysis_IMPROVED.qmd
```

**Expected output:**
```
=== SAVED RESULTS FOR MANUSCRIPT ===
✓ descriptive_paradox_summary.rds
✓ descriptive_sample_characteristics.rds
✓ descriptive_scale_reliability.rds
✓ descriptive_correlation_matrix.rds
✓ descriptive_bootstrap_correlations.rds
✓ descriptive_effect_sizes.rds
✓ descriptive_economic_impacts.rds
Results saved to: /path/to/results
```

### Test 2: Verify Files Created

```bash
ls -lh results/descriptive*.rds
```

You should see 7 `.rds` files with recent timestamps.

### Test 3: Render Manuscript (After Setup)

```bash
cd ../manuscript
quarto render manuscript.qmd
```

**Expected output:**
```
✓ Loaded all pre-computed analysis results
  - Descriptive: 7 objects
  - Hypothesis tests: 10 objects
  - Mediation: 7 objects
  - Country comparisons: 7 objects
  - Robustness checks: 8 objects
  Total: 39 analysis objects loaded
```

## Git Strategy (Recommendation)

**Option A: Commit `.rds` files** (RECOMMENDED)

```bash
git add analysis/results/*.rds
git commit -m "Updated analysis results"
```

**Pros:**
- ✅ Manuscript renders without re-running analysis
- ✅ Collaborators get latest results immediately
- ✅ Can track when results change

**Cons:**
- ⚠️ Binary files in git (but `.rds` files are small)

**Option B: Ignore `.rds` files**

Add to `.gitignore`:
```
papers/01_vietnam_covid_paradox/analysis/results/*.rds
```

**Pros:**
- ✅ Cleaner git history (no binaries)

**Cons:**
- ⚠️ Must re-render all analysis before rendering manuscript

We recommend **Option A** for this project.

## Troubleshooting

### Problem: "cannot open file 'xxx.rds'"

**Cause:** Analysis script hasn't been rendered yet
**Solution:** Render the corresponding analysis script first

```bash
quarto render analysis/03_descriptive_analysis_IMPROVED.qmd
```

### Problem: Results seem outdated

**Cause:** `.rds` files weren't updated after analysis changes
**Solution:** Re-render the analysis script

```bash
quarto render analysis/05_hypothesis_testing.qmd  # Updates .rds files
quarto render manuscript/manuscript.qmd           # Loads updated results
```

### Problem: "object 'xxx' not found" in manuscript

**Cause:** Variable name mismatch between save and load
**Solution:** Check `RESULTS_MANIFEST.md` for correct object names

## Benefits Summary

✅ **No duplication** - Analysis code exists only once
✅ **Fast renders** - Manuscript just loads results
✅ **Auto-sync** - Update analysis → Manuscript updates automatically
✅ **Clear separation** - Analysis vs. presentation
✅ **Version control** - Track when results change
✅ **Collaboration** - Team always has latest results

## Next Steps

1. ✅ Analysis scripts updated with save commands
2. ✅ Results directory created
3. ✅ Manifest and templates created
4. ⏳ Add setup chunk to `manuscript.qmd` (see `MANUSCRIPT_SETUP_TEMPLATE.md`)
5. ⏳ Test workflow by rendering one analysis script
6. ⏳ Replace duplicated code in manuscript with loaded results
7. ⏳ Render full manuscript to verify integration

## References

- **Workflow guide:** `WORKFLOW_BEST_PRACTICES.md`
- **Results catalog:** `analysis/results/RESULTS_MANIFEST.md`
- **Setup template:** `manuscript/MANUSCRIPT_SETUP_TEMPLATE.md`
- **This workflow:** `INTEGRATION_WORKFLOW.md`

---

**Last Updated:** 2025-12-12
**Status:** Implementation complete, testing pending
