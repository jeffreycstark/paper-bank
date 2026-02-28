# What Does Democracy Mean to Losers? Electoral Status and Conceptions of Democracy in Asia

## Overview

This paper examines how electoral status (winner vs. loser) shapes citizens' conceptions of democracy across Asia, using four waves of Asian Barometer data (2005-2022, N = 34,035).

See `manuscript/manuscript.qmd` for the full abstract and paper draft.

---

## Key Findings Summary

### Overall Loser Effect (Pooled)
- Losers 19% more likely to prioritize procedural democracy (OR = 1.19, p < 10⁻¹³)
- Effect significant in W2, W3, W4; disappears in W6

### By Wave
| Wave | Years | Loser Effect | p-value | N |
|------|-------|--------------|---------|---|
| W2 | 2005-08 | +6.5 pp | < 0.001 | 9,208 |
| W3 | 2010-12 | +4.3 pp | < 0.001 | 8,237 |
| W4 | 2014-16 | +5.1 pp | < 0.001 | 9,828 |
| W6 | 2019-22 | -1.5 pp | 0.26 | 6,762 |

### Key Country Trajectories
| Country | First Wave | Last Wave | Change |
|---------|------------|-----------|--------|
| Thailand | -0.9 pp (2006) | +17.5 pp (2020) | **+18.4 pp** |
| South Korea | +24.5 pp (2010) | -4.4 pp (2020) | **-28.9 pp** |
| Taiwan | +6.3 pp (2006) | -5.6 pp (2020) | -11.9 pp |

### Thailand Context
- 2006 (W2): Post-coup, 95% "winners," no loser effect
- 2010 (W3): Democrat govt, 49% winners, +4.6 pp effect
- 2014 (W4): Coup year, 45% winners, +10.6 pp effect
- 2020 (W6): Military-backed rule, 32% winners, +17.5 pp effect

---

## Target Journals
- Comparative Politics
- Democratization
- Journal of Democracy
- Party Politics
- (Stretch: World Politics, APSR)

---

## Data
- Asian Barometer Survey Waves 2, 3, 4, 6
- W2: q92 (single-item procedural/substantive)
- W3/W4/W6: q85-88 or q88-91 (4-set battery)
- Winner/loser: q39a (W2), q33a (W3), q34a (W4/W6)

---

## Project Structure

```
meaning-of-democracy/
├── README.md                    # This file
├── manuscript/
│   ├── manuscript.qmd           # Main paper draft
│   ├── references.bib           # Bibliography
│   └── apa.csl                  # Citation style
├── analysis/
│   ├── winner_loser_4waves.R    # Main analysis (4 waves)
│   ├── visualizations_4waves.R  # R visualization script
│   ├── visualizations_4waves.py # Python visualization script
│   ├── winner_loser_4waves.rds  # Combined dataset
│   ├── loser_effect_by_country_4waves.csv
│   ├── loser_effect_trajectory_4waves.csv
│   ├── fig_thailand_trajectory.png/pdf
│   ├── fig_thailand_dual.png/pdf
│   ├── fig_multicountry_trajectory.png/pdf
│   ├── fig_slope_change.png/pdf
│   └── fig_wave_losereffect.png/pdf
└── output/
    └── (rendered outputs)
```
