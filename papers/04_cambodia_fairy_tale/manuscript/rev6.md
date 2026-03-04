# Revision Memos: Response to Reviewer 2 Critique

---

## Memo 1: Bounds Analysis — Move A9 Into Main Text

**Location:** Limitations paragraph, manuscript lines ~484–486 (the passage beginning "A further caution concerns the `r round(stats$na_dem_future_w6, 0)` percent nonresponse...")

**Action:** Replace the current two-sentence treatment of W6 nonresponse as a "limitation-that-doubles-as-finding" with an expanded passage that incorporates the bounds analysis directly, then cites A9 for the full table.

**Proposed replacement prose:**

---

A further caution concerns the elevated nonresponse on the democratic future item in Wave 6: the 2021 mean is, in the strictest sense, a survivor statistic. Three imputation scenarios address the concern directly. If the `r round(stats$na_dem_future_w6, 0)` percent who declined to answer are assigned the scale maximum — treated, that is, as maximally pro-democratic respondents who withheld their views out of fear — the W6 mean rises, but the wave-over-wave decline from the 2012 peak narrows to a still-substantial negative value rather than disappearing. Assigning nonrespondents the W3 population mean — the most conservative scenario possible, treating the missing as a frozen image of peak democratic optimism — produces a similar result: the direction of change is preserved, the magnitude reduced. Only the most extreme assumption, assigning all nonrespondents a score of zero, could mechanically reverse the trajectory, and that assumption requires believing that the missing respondents are, without exception, deeply anti-democratic — the opposite of the fear-concealment hypothesis. The full bounds table is reported in Appendix Table A9.

The resistance of the directional finding to these scenarios does not dissolve the limitation, but it does specify its form. The question is not whether a decline occurred — under all plausible assumptions, it did — but how large that decline was. The reported 2021 mean is best read as an estimate of the attitudinal floor of the responding population, with the silent majority contributing an unknown quantity that bounds analysis constrains but cannot pin down. That `r round(stats$na_dem_future_w6, 0)` percent of respondents could not or would not venture an opinion about their country's democratic future is itself the condition demobilization by subtraction predicts. The silence and the diminished expectations of those who did answer are complementary expressions of the same process.

---

**Notes for implementation:**
- The inline `r round(stats$na_dem_future_w6, 0)` calls carry over directly from existing code.
- The specific numerical bounds values (the "still-substantial negative value" and "narrowed" figure) should be pulled from the A9 table once CC's updated output is available; placeholder language is used here.
- Delete the current two-sentence "further caution" passage (lines ~486–487) in full; this memo replaces it.

---

## Memo 2: Placebo Battery — Move Domain-Specificity Argument Forward

**Location:** The current placebo battery result is buried in footnote 21 and lightly referenced in the temporal-resolution section (~line 458). The domain-specificity argument appears in the alternative mechanisms passage but is underpowered.

**Action:** Strengthen the domain-specificity argument in the main alternative-mechanisms passage (the "temporal resolution" section, ~lines 456–458) by foregrounding the placebo battery logic as affirmative evidence, not merely a robustness check.

**Proposed addition** — insert as a new paragraph *after* the sentence ending "...even as the accompanying repressive measures amplified and accelerated those adjustments" (~line 458), before the tech-substitution discussion:

---

The domain-specificity pattern deserves more direct treatment than it has received, because it constitutes the strongest available evidence against a purely fear-centered interpretation. If generalized intimidation were the primary mechanism, its effects should register broadly: a population that is simply afraid of surveyors would be expected to express socially safe answers across the board, producing declines in reported opposition, increases in pro-regime sentiment, and suppression of any attitude touching governance. But that is not what the data show. National pride held stable across all four waves. Family economic assessment and perceptions of personal economic change are statistically indistinguishable between Wave 4 and Wave 6. Interpersonal trust declined, but the decline is consistent with documented pandemic-era patterns globally and may reflect social atomization rather than survey reticence. The shifts that are large, directional, and statistically unambiguous are concentrated precisely in the domain that the dissolution restructured: political aspiration, civic engagement, and willingness to prioritize democratic values against competing goods. A blanket fear story would produce a different cross-domain signature. What the data show instead is selectivity — the fingerprint of a mechanism that operated on the structure of political alternatives rather than on the willingness to speak. Full placebo battery results are reported in Appendix Table A3.

---

**Notes for implementation:**
- This paragraph makes the domain-specificity argument that currently exists only implicitly explicit, and repositions the A3 reference from footnote to main-text citation.
- The footnote 21 content on interpersonal trust can be shortened; the main-text paragraph above now carries that weight.

---

## Memo 3: Floor Effect Reframe — Adaptive Preference and the 2.2 Mean

**Location:** The democracy satisfaction finding is currently dispersed. It appears in the W4 section (~line 303: "Democracy satisfaction fell to 2.71"), in the W6 section (referenced in Table 3), and in the Conclusion (~line 474: "Democracy satisfaction — paradoxically — rose to its highest recorded level"). The rise from 2.71 to 3.10 is never framed as the paper's most reviewer-proof finding.

**Action:** Add a short framing move in the W6 section and reinforce it in the Conclusion. The authoritarian-preference floor-effect concern (mean ~2.2 on a 4-point scale is "mild" per the reviewer) can be addressed in the same passage.

**Proposed addition** — insert after the current Table 3 discussion in the W6 section, before the transition to alternative mechanisms:

---

Two findings from 2021 deserve particular attention before turning to alternative explanations. The first is the rise in democracy satisfaction, which climbed from its Wave 4 trough of 2.71 to 3.10 by 2021 — the highest value in the four-wave series. The item wording is identical across waves, nonresponse is low (6.4 percent in Wave 6), and the response is affirmative rather than oppositional: respondents reported being *satisfied* with how democracy functions in Cambodia. This is the hardest single data point in the analysis to explain as an artifact of fear or social desirability bias. A population censoring itself out of regime apprehension would be expected to avoid any politically sensitive judgment; satisfaction with the democratic system is both politically sensitive and positive toward the regime. The fact that it rose — and that it rose from a low baseline during the period of peak democratic aspiration — is the empirical signature of adaptive preference formation. Citizens did not become more optimistic about democracy's trajectory; they became more accepting of the reality that was on offer.

The second finding concerns the rise in authoritarian governance preferences. The absolute values warrant acknowledgment: at a mean of approximately 2.2 on a four-point scale, these remain well below the scale midpoint, and the Cambodian data do not support a reading of strong ideological conversion to authoritarianism. That is not, however, what demobilization by subtraction predicts. The mechanism does not require enthusiastic endorsement of authoritarian rule; it requires only the systematic erosion of the expectation that alternatives exist and the progressive disengagement from the behaviors through which those alternatives might be pursued. A mean of 2.2 — statistically elevated, substantively mild, uniformly distributed across demographic groups — is precisely the attitudinal signature of a population that has not converted to authoritarianism but has stopped believing that resisting it serves any purpose. The rise is the floor of normalization, not its ceiling.

---

**Notes for implementation:**
- The specific inline stats for satisfaction (2.71 → 3.10) and authoritarian preference mean (~2.2) should be confirmed against CC's output and converted to `r round(stats$...)` calls.
- This passage can absorb the existing sentence in the Conclusion about satisfaction rising "paradoxically"; that sentence can be cut or shortened once this section carries the argument.

---

## Memo 4: Thailand/Navalny — Comparative Extension Paragraph

**Location:** The Conclusion currently has a comparative gesture at ~line 490 ("The Cambodian sequence ... generates a testable temporal ordering applicable wherever credible alternatives have been eliminated: Turkey after the HDP suppression, Russia after the dismantling of Navalny's network..."). This is one sentence.

**Action:** Expand to a dedicated short paragraph that makes the diagnostic claim concrete and positions "the Cambodian signature" as a generalizable research tool.

**Proposed replacement** — replace the single comparative sentence with:

---

The most productive response to any single-country study is comparative extension. The Cambodian sequence — declining expectations, collapsing independent civic engagement, rising democratic satisfaction amid falling democratic aspiration, and the diagnostic turnout-participation divergence — generates a testable temporal ordering. Its application requires no bespoke instrument. Where Asian Barometer waves overlap with documented moments of opposition elimination — Thailand after the dissolution of Move Forward in 2024, the Philippines during Duterte's consolidation — the framework predicts a specific cross-domain pattern: political attitudes shifting while non-political attitudes hold, formal participation rising while independent engagement collapses, and satisfaction with democratic functioning rising even as expectations of democratic improvement fall. Russia's trajectory after the systematic dismantling of Navalny's organizational network from 2021 onward offers a harder test, where the suppression was more overtly repressive and the civic ecosystem thinner; whether the domain-specificity signature survives in a setting where generalized intimidation is more plausible is precisely the kind of cross-case variation that would allow the subtraction and repression mechanisms to be distinguished. The turnout-participation divergence is the most tractable single diagnostic, requiring only two commonly available survey measures and carrying no requirement that a credible opposition party ever existed — only that one was removed.

---

**Notes for implementation:**
- Move Forward was dissolved by Thailand's Constitutional Court in August 2024, which is after the manuscript's likely initial submission but is now an on-point example. Worth including.
- The existing one-sentence comparative gesture at line 490 should be deleted in full and replaced with this paragraph.
- This memo is low-priority relative to Memos 1–3 and should be implemented last.

---

## Memo 5: Scope Condition — When Subtraction Dominates Repression

**Location:** Introduction, mechanism-definition section. Insert as a new paragraph after the existing paragraph ending "...visible across multiple survey dimensions simultaneously" (the paragraph ending with footnote [^5], ~line 101), before the roadmap paragraph ("The remainder of this article proceeds as follows...").

**Action:** Add a paragraph that specifies the observable implications distinguishing subtraction from repression, frames the satisfaction finding as a formal adjudication test, and states the two scope conditions under which subtraction is expected to dominate.

**Proposed new paragraph:**

---

The distinction between subtraction and repression is not merely definitional; it generates different observable implications that allow empirical adjudication. Repression-driven demobilization operates through fear: the costs of political expression are raised, producing a population that self-censors, straightlines sensitive survey items, and suppresses positive evaluations of any governance arrangement. The observable signature is broad-spectrum attenuation — responses cluster toward the safe middle, nonresponse rises uniformly across sensitive batteries, and satisfaction with governance declines as citizens conceal their views. Demobilization by subtraction produces a different pattern. Because the mechanism operates through the removal of the political alternative rather than the elevation of punishment, it predicts domain-specific rather than broad-spectrum attenuation: political aspiration and independent engagement collapse while non-political attitudes remain stable, and satisfaction with democracy may actually *rise* as citizens recalibrate their evaluative standards against a narrowed horizon of possibility. The Cambodian data are consistent with the subtraction signature and inconsistent with the pure repression signature on both counts — domain-specificity holds across the placebo battery, and democracy satisfaction rose to its highest recorded value in the least democratic moment of the study period. Subtraction is expected to dominate repression as a demobilization mechanism under two conditions: when the opposition served as the primary focal organization for civic coordination, such that its removal collapses the informational and mobilizational infrastructure of collective action; and when the regime's repressive apparatus is selective rather than generalized, targeting the opposition and its organizational ecosystem rather than the citizenry at large. Cambodia satisfies both conditions. Where repression is generalized and mass terror is deployed, subtraction and repression are likely to operate simultaneously and their effects become difficult to separate; the Cambodian case, where the crackdown was organizationally targeted rather than population-wide, offers a cleaner window into the subtraction mechanism in relative isolation.

---

**Notes for implementation:**
- Insert as a standalone paragraph — no deletions required, this is purely additive.
- The satisfaction finding referenced here ("rose to its highest recorded value") is already supported by the wired inline stats in the W6 section; no new data work needed.
- This paragraph does double duty: it satisfies the reviewer's request for a formal scope condition and also front-loads the adjudication logic that currently appears only in the alternative mechanisms section late in the paper.
