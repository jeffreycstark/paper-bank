# Instructions for Claude Code

If you prefer to have Claude Code do the work instead of copy-pasting manually, give it these instructions:

---

## Task: Fix manuscript formatting and automate key values

**Files to modify:**
`~/Development/Research/AsianBarometer-ResearchHub/papers/01_vietnam_covid_paradox/manuscript/manuscript.qmd`

**Reference files (DO NOT MODIFY, just use as templates):**
- `SETUP_CHUNK_AUTOMATED.R`
- `THEORY_HYPOTHESES_FIXED.md`
- `INTRODUCTION_AUTOMATED.md`

### Part 1: Replace Setup Chunk

1. Open `manuscript.qmd`
2. Find the setup chunk (starts with ` ```{r setup, include=FALSE}`)
3. Replace the ENTIRE setup chunk with the contents of `SETUP_CHUNK_AUTOMATED.R`
4. Keep the surrounding YAML header and text unchanged

### Part 2: Fix Theory Section Hypotheses

In `manuscript.qmd`:
1. Find the text: `This framework generates several testable hypotheses:`
2. Replace from that line through the end of "Scope Conditions" paragraph with the contents of `THEORY_HYPOTHESES_FIXED.md`
3. This fixes bullet point formatting (using `-` instead of `•` characters)

### Part 3: Automate Introduction

In `manuscript.qmd`:
1. Find the Introduction section (starts with "On February 20, 2022...")
2. Replace the entire Introduction with the contents of `INTRODUCTION_AUTOMATED.md`
3. This converts hard-coded numbers like "65.9%" to inline R code like `` `r round(vietnam$infection_pct, 1)`% ``

### Part 4: Verify

After making changes:
1. The manuscript should still render without errors
2. Hypotheses should appear on separate lines
3. Introduction numbers should display (even if using fallback values)

### Key Points:

- Keep existing YAML header unchanged
- Keep all other sections (Theory intro, Methods, Results, Discussion, Conclusion) unchanged
- Only replace the three specific sections mentioned above
- Preserve all markdown formatting, citations, and structure

### Expected Result:

- ✅ Setup chunk loads results AND extracts automated values
- ✅ Hypotheses H1-H5 display on separate lines with proper bullets
- ✅ Introduction uses inline R for all key numbers (infection rates, approval rates, sample sizes)

---

That's it! Claude Code should be able to make these three targeted replacements.
