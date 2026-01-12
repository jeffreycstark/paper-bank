# Wave 6 Case Sensitivity Issues

## Problem
Wave 6 (W6) data files often have column names with different capitalization than earlier waves. This causes 100% missing data for affected variables if the YAML spec uses the wrong case.

## Common Patterns Fixed

### Demographics (demographics.yml)
| Variable | Wrong | Correct |
|----------|-------|---------|
| urban_rural | LEVEL | level |
| gender | SE2 | se2 |
| age | SE3_1 | se3_1 |

### Social Media Platforms (social_media.yml)
| Variable | Wrong | Correct |
|----------|-------|---------|
| sm_use_facebook | q51a_Facebook | q51a_facebook |
| sm_use_twitter | q51b_Twitter | q51b_twitter |
| sm_use_instagram | q51c_Instagram | q51c_instagram |
| sm_use_youtube | q51d_Youtube | q51a_youtube |
| sm_use_tiktok | q51e_Tiktok | q51e_tiktok |
| sm_use_messenger | q51f_messenger | q51g_philippines_messenger |
| sm_use_whatsapp | q51i_Whatsapp | q51e_whatsapp |

## How to Diagnose
If a variable shows 0% or 100% missing for W6 specifically:

```r
# Load raw W6 data and search for column
w6 <- readRDS("data/processed/w6.rds")
grep("pattern", names(w6), ignore.case = TRUE, value = TRUE)

# Check valid values
table(w6$column_name, useNA = "ifany")
```

## Prevention
When adding new W6 variables, always verify column names directly from the raw data file:
```r
w6 <- haven::read_sav("data/raw/wave6/W6_[Country]_Release_*.sav")
names(w6)
```

## Note
W6 generally uses lowercase column names, while YAML specs often have mixed case from earlier waves.
