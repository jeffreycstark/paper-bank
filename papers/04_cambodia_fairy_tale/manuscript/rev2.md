# Tasks for CC — Reviewer 2 Response

**Priority order. Items 1–3 are blocking; 4–6 can be done in parallel or after.**

---

## 1. W3→W4 Partisan Decomposition (CRITICAL — highest priority)

The reviewer correctly identified that the W4→W6 partisan decomposition shows uniform effects across groups, and that the paper reframes this as "field-level" rather than "party-level" — which risks unfalsifiability. The fix is to show the *contrast*: if W3→W4 (when partisan categories were still clean) shows differential effects by party, but W4→W6 (when categories are endogenous to dissolution) shows uniform effects, the before/after comparison becomes strong evidence.

**What to produce:**

Replicate the W4→W6 partisan decomposition table (Section 1.1.9) for the W3→W4 period. Same structure, same variables:

- Contact with influential persons (gate %)
- Democratic future expectations (0–10 mean)
- Single-party rule acceptance (1–4 mean)
- Political interest (1–4 mean)
- Witnessed corruption (binary %)
- Democracy satisfaction (1–4 mean)

Broken out by W3 vote choice categories: CPP voter, CNRP/SRP voter, nonvoter, DK/Refuse.

Report point estimates with 95% CIs and Δ(W4−W3) for each group. Flag any statistically significant between-group differences in the magnitude of change (i.e., test whether CNRP voters' decline on democratic future expectations is significantly steeper than CPP voters' decline).

**Why this matters:** If CNRP supporters show steeper attitudinal shifts than CPP supporters in the W3→W4 period — when the CNRP was under pressure but still intact — and then the W4→W6 comparison shows uniformity after the dissolution destroyed the partisan categories, the contrast is the paper's single strongest piece of evidence for the subtraction mechanism.

---

## 2. W6 DK/Refuse Group Profile (HIGH PRIORITY)

The reviewer flagged that the DK/Refuse category tripled from 7.2% to 18.8% between W4 and W6, and likely contains resistant former CNRP supporters. The paper gestures at this but doesn't systematically analyze it.

**What to produce:**

Full attitudinal profile of the W6 DK/Refuse group (N ≈ 233) compared to W6 CPP voters and W6 residual CNRP voters (if N is sufficient):

- All four democratic orientation measures (forced-choice preference distribution, dem_best_form mean, dem-vs-equality mean, satisfaction mean)
- Democratic expectations (future, present, past — means among respondents)
- Nonresponse rates on democratic expectation items
- Authoritarian preference means (all four items)
- Contact with influential persons, political interest, follows political news

The hypothesis: DK/Refuse in W6 should look like "suppressed CNRP supporters" — lower democratic commitment than W4 CNRP voters but higher than W6 CPP voters, and higher nonresponse rates on sensitive items. If confirmed, this is evidence of residual partisan heterogeneity surviving through the only available channel (refusal to identify).

---

## 3. OLS Models for Appendix (HIGH PRIORITY)

The preference falsification section (pp. 48–49) references OLS models and interaction terms but doesn't show them. The empty appendix is a problem. These need to be fully reported.

**What to produce:**

**Model Set A — Attenuation tests:**
Three OLS models predicting (a) democratic future expectations, (b) democracy satisfaction, (c) forced-choice democratic preference. Each model run twice: (i) wave dummies only; (ii) wave dummies + perceived freedom of speech + perceived freedom to organize. Report coefficients, SEs, and the percentage attenuation of wave coefficients when freedom perceptions are added.

**Model Set B — Interaction models:**
Same three DVs. Wave dummies × perceived freedom of speech interaction. Report the interaction coefficients and significance levels referenced on p. 49 (Wave 4 interaction: β = −1.88; Wave 6: β = −1.18).

**Model Set C — Nonresponse predictors:**
Logistic regression predicting nonresponse on democratic future item in W6. IVs: perceived freedom of speech, perceived freedom to organize, education, urbanicity. Report ORs and p-values (the paper cites OR = 1.01, p = 0.95 for speech and OR = 0.9, p = 0.36 for organizing — these need to be in a table).

Format all as publication-ready tables for the appendix.

---

## 4. Bounds Analysis Table (MODERATE PRIORITY)

The bounds analysis (p. 51) is referenced but not shown. Produce a table showing:

- Observed W3 mean on democratic future (9.58)
- Observed W6 mean among respondents (6.67)
- W6 mean with all nonrespondents imputed at scale maximum (10)
- W6 mean with all nonrespondents imputed at scale minimum (0)
- W6 mean with nonrespondents imputed at W3 population mean (9.58)
- Resulting Δ from W3 under each scenario

This is a simple calculation but it needs to be in the appendix as a table.

---

## 5. Updated Figure 1

Add two series to the Democratic Expectations panel (rename it "Democratic Orientations"):

- "Democracy is best form of government" (1–4 mean) — W3, W4, W6
- Democracy-vs-equality trade-off (1–5 mean) — W3, W4, W6

These should be visually distinguishable from the existing expectations lines. The point is to show the *flat* commitment line diverging from the *declining* prioritization and expectations lines — this is the visual core of the revised argument.

Keep the existing democratic future and democratic present series. Consider whether the panel needs its own y-axis treatment given the different scales (0–10 vs. 1–4 vs. 1–5), or whether it should be split into two sub-panels.

---

## 6. Δ W4→W6 Column in Table 3

Add a column to Table 3 reporting W4→W6 change alongside the existing W3→W6 column. The post-dissolution window (W4→W6) is theoretically primary — the reviewer is right that it should be visible in the main table without requiring the reader to compute it mentally.

---

## Formatting notes

- All new tables should match existing format: point estimates with 95% CIs in parentheses, Wilson intervals for proportions, t-based intervals for means.
- New appendix tables should be numbered sequentially following existing appendix tables (A4, A5, etc.).
- Flag any data issues or unexpected patterns — especially if the W3→W4 partisan decomposition shows *no* differential effects, since that changes the argumentative strategy.
