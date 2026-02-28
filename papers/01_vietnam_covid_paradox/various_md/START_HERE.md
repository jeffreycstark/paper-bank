# 🎯 QUICK FIX GUIDE - Do These 3 Steps (30 minutes total)

I've done all the work for you! Just copy-paste these three sections into your manuscript.

## ✅ Step 1: Replace Setup Chunk (5 minutes)

**File:** `SETUP_CHUNK_AUTOMATED.R`

1. Open `manuscript.qmd` in RStudio
2. Find the setup chunk (starts with ` ```{r setup, include=FALSE}`)
3. DELETE everything from ` ```{r setup` through the closing ` ``` `
4. Open `SETUP_CHUNK_AUTOMATED.R`
5. COPY the entire contents
6. PASTE into your manuscript where the old setup chunk was
7. Save

**What this does:**
- ✅ Loads all your .rds results (same as before)
- ✅ NEW: Extracts key values into variables like `vietnam$infection_pct`
- ✅ NEW: Adds helper functions for cleaner tables
- ✅ NEW: Provides fallback values if .rds files don't exist yet

## ✅ Step 2: Fix Theory Hypotheses (5 minutes)

**File:** `THEORY_HYPOTHESES_FIXED.md`

1. In `manuscript.qmd`, search for: `This framework generates several testable hypotheses:`
2. DELETE from that line through the end of the "Scope Conditions" paragraph
3. Open `THEORY_HYPOTHESES_FIXED.md`
4. COPY the entire contents
5. PASTE into your manuscript where you deleted
6. Save

**What this fixes:**
- ✅ Hypotheses now on separate lines (proper Markdown bullets)
- ✅ Mechanism patterns clearly formatted
- ✅ Scope conditions numbered properly
- ✅ No more running-together text!

## ✅ Step 3: Automate Introduction Numbers (10 minutes)

**File:** `INTRODUCTION_AUTOMATED.md`

1. In `manuscript.qmd`, find the Introduction section (starts after Theoretical Framework heading)
2. DELETE the entire Introduction (from "On February 20, 2022..." through "...modern age.")
3. Open `INTRODUCTION_AUTOMATED.md`
4. COPY the entire contents  
5. PASTE into your manuscript
6. Save

**What this does:**
- ✅ ALL numbers now pull from data using `` `r code` ``
- ✅ Changes data → re-render → numbers update automatically
- ✅ No more hard-coded 65.9%, 97.5%, etc.
- ✅ Fallback values if .rds files don't exist yet

## ✅ Step 4: Render and Verify (10 minutes)

1. Click **Render** button in RStudio (or Cmd/Ctrl + Shift + K)
2. Check the output:
   - [ ] Hypotheses show on separate lines?
   - [ ] Numbers appear in Introduction?
   - [ ] No weird `` `r ...` `` showing in text?
3. If everything looks good, you're done! 🎉

## 🎊 What You've Accomplished

After these 3 steps, you have:

✅ **Fixed formatting** - Hypotheses display properly  
✅ **Automated 15+ key numbers** - Vietnam infection, approval, sample sizes, etc.
✅ **Added helper functions** - Cleaner code in Results section
✅ **Made manuscript reproducible** - Change data → numbers update everywhere

## 📊 Before & After Examples

### Before (Hard-Coded):
```markdown
Vietnam's infection rate (65.9%) exceeded Cambodia's (8.7%)...
```

### After (Automated):
```markdown
Vietnam's infection rate (`r round(vietnam$infection_pct, 1)`%) 
exceeded Cambodia's (`r round(cambodia$infection_pct, 1)`%)...
```

### Rendered Output (Same!):
```
Vietnam's infection rate (65.9%) exceeded Cambodia's (8.7%)...
```

But now if your data changes, the numbers update automatically!

## 🚀 Next Steps (After Today)

Once you verify everything works, you can continue automating:

1. **Methods section** - Variable descriptive statistics
2. **Results section** - Correlation values  
3. **Discussion section** - Summary statistics
4. **Abstract** - Key findings

See `FULL_AUTOMATION_GUIDE.md` for complete instructions.

## ❓ Troubleshooting

### Problem: Numbers show as `` `r ...` `` in output
**Solution:** You're in a code fence. Make sure inline R is OUTSIDE of ` ``` ` blocks.

### Problem: "Object 'vietnam' not found"
**Solution:** Setup chunk didn't run. Make sure you rendered the entire document (not just a section).

### Problem: Numbers show as fallback values (65.9, 97.5)
**Solution:** Your .rds files don't exist yet. Run your analysis scripts to create them:
```r
# In R Console
source(here("papers/01_vietnam_covid_paradox/analysis/03_descriptive_analysis_IMPROVED.qmd"))
```

### Problem: Hypotheses still running together
**Solution:** Make sure you have a BLANK LINE before the list starts, and use `-` not `•`

## ✨ You're Ready!

Follow the 3 steps above in order. Each takes just a few minutes.

Total time: **~30 minutes**  
Result: **Properly formatted, automated manuscript!**

---

**Files in this folder:**
- `SETUP_CHUNK_AUTOMATED.R` ← Copy into manuscript setup chunk
- `THEORY_HYPOTHESES_FIXED.md` ← Copy into Theory section  
- `INTRODUCTION_AUTOMATED.md` ← Copy into Introduction section
- `THIS_FILE.md` ← You are here!
- `FULL_AUTOMATION_GUIDE.md` ← For automating everything else later

**Ready to go?** Open `manuscript.qmd` and start with Step 1! 🚀
