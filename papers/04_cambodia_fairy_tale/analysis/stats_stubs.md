# Stats Stubs: Marching Orders for Claude Code
# Cambodia Fairy Tale Paper — cd_manuscript.qmd
# Project: /Users/jeffreystark/Development/Research/paper-bank/papers/cambodia-fairy-tale

## Overview

The manuscript has three table stubs and two figure-dependent sections that
need to be built from the ABS Cambodia data. The analysis pipeline is
scaffolded in `analysis/00_data_preparation.qmd` and `analysis/01_descriptive_analysis.qmd`
but the descriptive tables haven't been generated yet.

Data source: `analysis/results/analysis_data.rds` (produced by 00_data_preparation.qmd)
Helpers: `R/helpers.R` (has theme_pub, pal_main, survey_years_lookup, etc.)
Output tables to: `analysis/tables/`
Output figures to: `analysis/figures/`

---

## Task 1: Table 1 — Wave 2 Baseline (line ~124 in cd_manuscript.qmd)

**Location in manuscript**: "The Quiet Kingdom: Cambodia in 2008" section

**What it should show**: Wave 2 (2008) mean values across all five thematic
domains, establishing the baseline before the mobilization surge.

**Structure**: Single-column table (W2 values only), grouped by domain:

| Domain | Variable | Variable Label | W2 Mean | N |
|--------|----------|---------------|---------|---|
| **Political Participation** | action_contact_elected | Contacted elected official | 3.44 | — |
| | action_contact_civil_servant | Contacted civil servant | 3.43 | — |
| | community_leader_contact | Contacted community leader | 1.53 | — |
| | voted_last_election | Voted in last election | — | — |
| | action_demonstration | Attended demonstration | — | — |
| | action_petition | Signed petition | — | — |
| **Authoritarian Preferences** | expert_rule | Expert rule | — | — |
| | single_party_rule | Single-party rule | 1.79 | — |
| | strongman_rule | Strongman rule | 1.60 | — |
| | military_rule | Military rule | 2.19 | — |
| **Democratic Expectations** | dem_country_future | Democratic future (10pt) | — | — |
| | dem_country_past | Democratic past (10pt) | — | — |
| | dem_country_present_govt | Democratic present (10pt) | — | — |
| **Corruption** | corrupt_witnessed | Witnessed corruption (binary) | 0.277 | — |
| | corrupt_national_govt | National govt corruption (1-4) | 2.86 | — |
| | corrupt_local_govt | Local govt corruption (1-4) | 2.56 | — |
| **Media & Political Interest** | pol_news_follow | Follows political news | — | — |
| | news_internet | Internet news | — | — |
| | political_interest | Political interest | — | — |
| | pol_discuss | Discusses politics | — | — |

**Notes**:
- Some variables (demonstration, petition, dem_country_future/past/present)
  may not exist in W2. Mark as "—" and note in table footnote.
- Report scale ranges in a table note (e.g., "Participation: 1=never to 5=often;
  Authoritarian preferences: 1=very bad to 4=very good")
- Values shown above are from the prospecting memo — verify against actual data
- Use kableExtra for formatting, save as both .rds and render-ready in the qmd

---

## Task 2: Table 2 — W3→W4 Comparison (line ~168)

**Location in manuscript**: "The Reckoning: Cambodia in 2015" section

**What it should show**: Side-by-side W3 (2012) and W4 (2015) means with
change values, showing the selective retreat pattern.

**Structure**:

| Domain | Variable Label | W3 Mean | W4 Mean | Δ (W4−W3) |
|--------|---------------|---------|---------|-----------|
| **Political Participation** | Contacted elected official | 4.31 | 3.08 | −1.23 |
| ... | ... | ... | ... | ... |

Same variable list as Table 1. Include all five domains.

**Key patterns the table should make visible**:
- Participation: all declining
- Authoritarian preferences: flat or slightly declining (NOT yet rising)
- Democratic expectations: future falling sharply (9.58→7.72)
- Corruption witnessed: rising (0.494→0.628)
- Media/interest: modest declines

---

## Task 3: Table 3 — Full Four-Wave Trajectory (line ~192)

**Location in manuscript**: "The Silence: Cambodia in 2021" section

**This is the key empirical table of the paper.**

**What it should show**: All four waves side by side with the complete
trajectory visible.

**Structure**:

| Domain | Variable Label | W2 (2008) | W3 (2012) | W4 (2015) | W6 (2021) |
|--------|---------------|-----------|-----------|-----------|-----------|
| **Political Participation** | | | | | |
| Contacted elected official | | 3.44 | 4.31 | 3.08 | 1.74 |
| ... | | | | | |

Same variable list. Consider adding a "Δ W3→W6" column to highlight the
magnitude of collapse from peak to trough.

**Formatting notes**:
- Bold or shade the W6 column to draw the eye to the endpoint
- Include N per wave in the header row or a footnote
- Table note should list all scale ranges
- Consider whether voted_last_election (the paradoxical rise) should
  be visually distinguished since it moves opposite to everything else

---

## Task 4: Figure 1 — Multi-panel trend plot (optional but recommended)

**Location**: Could go near Table 3, or as a standalone figure

**What it should show**: Five small-multiple panels (one per domain),
each showing the wave trajectory as a line plot. This makes the
"everything rises in W3, everything falls by W6" pattern visually
immediate.

**Design**:
- facet_wrap(~domain, scales = "free_y", ncol = 2 or 3)
- X-axis: wave years (2008, 2012, 2015, 2021)
- Y-axis: mean value (free scales since domains use different metrics)
- Multiple lines per panel (one per variable in that domain)
- Use theme_pub and pal_main from helpers.R
- Vertical dashed line at 2017 (CNRP dissolution) as reference

**Variables to include**: Select the most narratively important 2-3
variables per domain rather than plotting everything:
- Participation: action_contact_elected, action_demonstration, voted_last_election
- Auth preferences: single_party_rule, strongman_rule
- Democratic expectations: dem_country_future, dem_country_present_govt
- Corruption: corrupt_witnessed, corrupt_national_govt
- Media: pol_news_follow, political_interest

---

## Task 5: Verify hardcoded stats in manuscript text

The manuscript body contains many specific numbers drawn from the
prospecting memo. CC should verify each against the actual analysis_data.rds.

**Numbers to verify** (search the manuscript for these):

### Participation (1-5 scale)
- action_contact_elected: W2=3.44, W3=4.31, W4=3.08, W6=1.74
- action_contact_civil_servant: W2=3.43, W3=4.33, W4=3.19, W6=2.47
- action_demonstration: W3=4.48, W4=3.07, W6=1.29
- action_petition: W3=4.34, W4=3.23, W6=2.14
- community_leader_contact: W2=1.53, W3=1.90, W4=2.00, W6=1.39
- voted_last_election: W3=0.787, W4=0.832, W6=0.881

### Authoritarian preferences (1-4 scale)
- expert_rule: W3=1.58, W4=1.66, W6=2.11
- single_party_rule: W2=1.79, W3=1.97, W4=1.78, W6=2.21
- strongman_rule: W2=1.60, W3=1.73, W4=1.76, W6=2.17
- military_rule: W2=2.19, W3=2.19, W4=1.98, W6=2.21

### Democratic expectations (10pt scale)
- dem_country_future: W3=9.58, W4=7.72, W6=6.67
- dem_country_past: W3=3.97, W4=3.87, W6=4.78
- dem_country_present_govt: W3=5.85, W4=5.06, W6=5.77

### Corruption
- corrupt_witnessed (binary): W2=0.277, W3=0.494, W4=0.628, W6=0.149
- corrupt_national_govt (1-4): W2=2.86, W3=2.67, W4=2.90, W6=2.33
- corrupt_local_govt (1-4): W2=2.56, W3=2.47, W4=2.56, W6=2.36

### Media & interest
- pol_news_follow: W3=3.07, W4=2.86, W6=2.04
- news_internet: W3=5.78, W4=2.17, W6=2.17
- political_interest: W3=2.57, W4=2.29, W6=2.06
- pol_discuss: W3=1.45, W4=1.47, W6=1.26

### China perceptions
- intl_china_asia_goodharm: W4=2.69, W6=2.88
- intl_future_influence_asia: trajectory rising to W6=3.30

### Action items if any numbers don't match:
1. Note the discrepancy
2. Report the correct value from the data
3. Flag which manuscript line needs updating
4. Do NOT silently change manuscript text — report back

---

## Task 6: Check variable names

The variable names used above are from the prospecting memo and may
not match the actual column names in analysis_data.rds. Before building
tables:

1. Load analysis_data.rds
2. Run names(dat) or glimpse(dat)
3. Map the prospecting memo variable names to actual column names
4. Report any variables that don't exist in the data
   (they may need to be constructed in 00_data_preparation.qmd)

---

## Output Checklist

When done, the following should exist:

- [ ] `analysis/tables/table1_w2_baseline.rds` — Table 1 data
- [ ] `analysis/tables/table2_w3w4_comparison.rds` — Table 2 data
- [ ] `analysis/tables/table3_four_wave_trajectory.rds` — Table 3 data
- [ ] `analysis/figures/fig1_trend_panels.pdf` (and .png) — Figure 1
- [ ] Verification report: list of any stats in the manuscript that
      don't match the data, with correct values and line numbers
- [ ] Variable name mapping: prospecting memo names → actual column names

All tables should also be renderable in the manuscript qmd via
kableExtra or gt. Consider saving render-ready table objects as .rds
so the manuscript can just load and display them.

---

## Task 7: Check valid-N variance across variables within each wave

For each wave, compute the valid (non-missing) N for every variable
that appears in the tables. Report:

1. The min and max valid N per wave
2. Which variables, if any, have notably higher missingness
   (e.g., >10% below the wave-level max)
3. Whether the internet news variable (news_internet) has
   substantially different missingness than other variables,
   given its harmonization changes

**Decision rule for table formatting**:
- If valid N is tight within each wave (all cells within ~5% of
  each other), use a single wave-level N in the column header
  plus a table note: "Valid N ranges from X to Y per wave;
  full cell-level counts available on request."
- If any variable has notably higher missingness (>10% gap),
  report per-cell valid N in parentheses after each mean,
  OR add a dedicated N row for that variable with a footnote.

Report findings so Jeff can decide which approach to use.

## Updated Output Checklist

- [ ] `analysis/tables/table1_w2_baseline.rds`
- [ ] `analysis/tables/table2_w3w4_comparison.rds`
- [ ] `analysis/tables/table3_four_wave_trajectory.rds`
- [ ] `analysis/figures/fig1_trend_panels.pdf` (and .png)
- [ ] Verification report (stat mismatches + line numbers)
- [ ] Variable name mapping
- [ ] Valid-N variance report per wave (Task 7)
