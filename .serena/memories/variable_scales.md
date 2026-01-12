# Common Variable Scales and Conventions

## Political Action Variables (action_*)
**Scale**: 1-5 (harmonized so higher = more participation)
| Code | Label |
|------|-------|
| 1 | Would never do |
| 2 | Have not done, but might |
| 3 | Have done once |
| 4 | Have done 2-3 times |
| 5 | Have done 4+ times |

**Key variables**: action_demonstration, action_petition, action_contact_elected, action_join_cause

**Filtering for actual participation**: codes 3, 4, or 5
```r
did_participate <- d$action_demonstration %in% c(3, 4, 5)
```

## Trust Variables (trust_*)
**Scale**: 1-4 (harmonized so higher = more trust)
| Code | Label |
|------|-------|
| 1 | No trust at all |
| 2 | Not very much trust |
| 3 | Quite a lot of trust |
| 4 | A great deal of trust |

## Economic Evaluation (econ_*)
**Scale**: 1-5 (harmonized so higher = more positive)
| Code | Label |
|------|-------|
| 1 | Very bad |
| 2 | Bad |
| 3 | So so |
| 4 | Good |
| 5 | Very good |

## Democracy Satisfaction (dem_sat_*)
**Scale**: 1-4 (harmonized so higher = more satisfied)
| Code | Label |
|------|-------|
| 1 | Not satisfied at all |
| 2 | Not very satisfied |
| 3 | Fairly satisfied |
| 4 | Very satisfied |

## Binary Variables (Yes/No)
**Scale**: 1-2
| Code | Label |
|------|-------|
| 1 | Yes |
| 2 | No |

Examples: gender (1=Male, 2=Female), urban_rural (1=Urban, 2=Rural), employed, sm_use_*

## Raw Data Note
Raw data often has REVERSED scales (e.g., 1=Very good, 5=Very bad). The harmonization applies `safe_reverse_*pt()` functions to standardize so higher values = more positive/active.

## Missing Value Codes
Standard missing codes converted to NA: -1, 0, 7, 8, 9, 97, 98, 99
