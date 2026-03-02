# CC Marching Orders — paper 03b (The Satisfaction Paradox)

---

## SUPPLEMENTARY: Reviewer Response Items

*Added 2026-03-02. These address a simulated reviewer report. Most are small
additions to analyses already planned or already existing in 03's appendix.*

---

### Priority Order

1. **R4** — Robust SEs (changes all reported models; do first)
2. **R1** — Identity-signaling evidence (highest-impact revision)
3. **R2** — Polychoric correlation matrix (new but quick)
4. **R3** — Cronbach's alpha / McDonald's omega (quick)
5. **R6** — Yoon crisis citations (author task, not CC)
6. **R7** — Terminology consistency (find-and-replace)
7. **R5** — Incumbent control (data-dependent)
8. **R8** — Sample size note (one sentence)
9. **R9** — Taiwan econ descriptives (one sentence / table)

---

### R1. Promote identity-signaling evidence to main text

**Source**: 03's Appendix F.2 — China threat moderation analysis.
Tests whether econ → "always preferable" is stronger among Taiwanese who
perceive greater China threat. This is the empirical test for the
"identity signaling" claim.

**Action**:
- Port the F.2 analysis to 03b (new QMD or add to existing auth/item-spec script).
- In `skdr-manuscript.qmd`, add 1–2 sentences at end of Section 4.4 (Taiwan comparison):

  > "Consistent with the identity-signaling interpretation, the economic →
  > abstract preference relationship in Taiwan is moderated by China threat
  > perception: the critical citizens pattern is significantly stronger among
  > respondents who perceive greater threat from the PRC (interaction β = X.XX,
  > p = X.XX; see Appendix F)."

- Brief reference to national pride moderation (F.1/F.3) — one sentence sufficient.
- Add results to **Appendix F** (new section or subsection).

---

### R2. Polychoric correlation matrix: abstract item × auth rejection

**New analysis** → **Appendix I** (item specificity diagnostics), new table.

Compute polychoric correlations between `dem_always_preferable` and each of the
four auth rejection items, separately by country × wave.

**Prediction**: Correlations weaker in Korea than Taiwan, supporting the claim
that items tap different constructs in Korea.

```r
library(polycor)

# For each country × wave:
cor_matrix <- hetcor(
  dat_sub[, c("dem_always_preferable", "strongman_reject",
              "military_reject", "expert_reject", "singleparty_reject")],
  type = "polychoric"
)
```

Present as a compact table in Appendix I (Table I.X). Variable names to
confirm in `analysis_data.rds` — may be `auth_reject_strongman` etc.

---

### R3. Cronbach's alpha / McDonald's omega for indices

**Add to** data preparation QMD or Appendix A/C.

Report reliability for:
- Economic evaluation index (6 items) — by country × wave
- Authoritarian rejection index (4 items) — by country × wave

```r
library(psych)
alpha_result <- psych::alpha(dat_sub[, econ_items])
omega_result <- psych::omega(dat_sub[, auth_items], nfactors = 1)
```

Present as a compact table (country × wave rows, alpha / omega cols).

---

### R4. Robust / clustered SEs *(do first — changes all reported models)*

**Check**: Does ABS provide PSU / strata identifiers in the harmonized data?
If yes → cluster-robust SEs at PSU level.
If no → HC2 robust SEs (sandwich estimator) as default throughout.

```r
library(sandwich)
library(lmtest)
coeftest(model, vcov = vcovHC(model, type = "HC2"))
```

**This should be the DEFAULT for all reported models in 03b**, not a robustness
check. Update `analysis/02_models.qmd` accordingly.

Note in Methods section which SE type is used.

---

### R5. Incumbent evaluation horse race *(data-dependent)*

Check ABS harmonized data for these variables across waves:
- Presidential/PM approval
- Party identification / party closeness
- Retrospective vote choice

**If available in ≥3 waves**: Run satisfaction model with `econ_index` AND
incumbent approval as competing predictors. Show `econ_index` survives.

**If not available**: Add to limitations:
> "Economic evaluations may partially proxy incumbent evaluation, though the
> null on abstract preference — where partisan sorting should operate similarly
> — argues against this interpretation."

---

### R6. Yoon crisis citations *(author task)*

Add 3–4 footnotes to the introduction vignette with specific sources:

1. **Martial law declaration, December 3, 2024**: Yonhap News Agency wire;
   Reuters/AP coverage.
2. **National Assembly vote to overturn (within 6 hours)**: National Assembly
   official record; Hankyoreh or JoongAng Daily.
3. **Impeachment**: Constitutional Court of Korea ruling.
4. **Life sentence, February 2026**: Seoul Central District Court (case number
   if available); wire service confirmation.

Also add one explicit sentence after the vignette:
> "The ABS observation window closes in 2022; the martial law episode is
> invoked as interpretive context, not as evidence subject to the empirical
> analysis."

---

### R7. Terminology consistency

Pick ONE term and use it throughout: **"authoritarian rejection"**
(positive framing; matches variable direction after reversal).

- Do NOT use "authoritarian detachment" (Chu et al.'s term) after first
  introduction.
- Do NOT use "authoritarian acceptance" (confusing sign direction).

Add a footnote at first use:
> "I use 'authoritarian rejection' to describe what Chu et al. (2008) termed
> 'authoritarian detachment.' Higher values indicate stronger rejection of
> authoritarian alternatives."

**Action**: Find-and-replace in `skdr-manuscript.qmd` and
`skdr-online-appendix.qmd`. Search for "detachment" and "acceptance" in
context of authoritarianism.

---

### R8. Sample size note

Add to Methods (Section 3.1), one sentence:
> "Effective sample sizes vary across outcomes due to item nonresponse; exact
> N is reported in each table."

---

### R9. Taiwan econ descriptives

Add Taiwan economic evaluation index descriptive statistics to the data
description (main text or appendix). Currently only Korea econ descriptives
are shown. Add wave-level means/SDs for Taiwan's econ index to match the
Korea table in Appendix A.3.
