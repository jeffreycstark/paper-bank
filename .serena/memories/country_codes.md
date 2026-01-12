# Country Codes Reference

## Numeric Codes (used in `country` variable)

| Code | Country | Notes |
|------|---------|-------|
| 1 | Japan | All waves |
| 2 | Hong Kong | All waves |
| 3 | Korea (South) | All waves |
| 4 | China (Mainland) | All waves |
| 5 | Mongolia | All waves |
| 6 | Philippines | All waves |
| 7 | Taiwan | All waves |
| 8 | Thailand | All waves |
| 9 | Indonesia | All waves |
| 10 | Singapore | All waves |
| 11 | Vietnam | All waves |
| 12 | Cambodia | W2+ |
| 13 | Malaysia | W2+ |
| 14 | Myanmar | W3+ |
| 15 | Australia | W5+ |
| 18 | India | W5 only |

**Note**: Codes 16-17 are unused. Code 18 (India) only appears in W5.

## Wave 6 Special Handling
W6 uses text country names in raw data that are mapped to numeric codes via `recode_w6_country()` function in the harmonization.

## Quick R Reference
```r
# Filter to specific country
korea <- d[d$country == 3, ]

# Country labels for tables
country_labels <- c(
  "1" = "Japan", "2" = "Hong Kong", "3" = "Korea", "4" = "China",
  "5" = "Mongolia", "6" = "Philippines", "7" = "Taiwan", "8" = "Thailand",
  "9" = "Indonesia", "10" = "Singapore", "11" = "Vietnam", "12" = "Cambodia",
  "13" = "Malaysia", "14" = "Myanmar", "15" = "Australia", "18" = "India"
)

# Sample sizes by country and wave
table(d$country, d$wave)
```
