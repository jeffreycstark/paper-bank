# Load libraries
library(haven)
library(dplyr)

# --- 1. Load the two conflicting files ---
df_australia <- read_sav("/data/raw/wave6/W6_15_Australia_Release_20250305.sav")
df_vietnam <- read_sav("/data/raw/wave6/W6_11_Vietnam_Release_20250117.sav")
df_cambodia <- read_sav("/data/raw/wave6/W6_Cambodia_Release_20240819.sav")
df_thailand <- read_sav("/data/raw/wave6/W6_8_Thailand_Release_20250108.sav")

# --- 2. Get the value labels for q18 from each file ---
print("--- Vietnam q18 Labels ---")
print(attr(df_vietnam$q18, "labels"))

print("--- Cambodia q18 Labels ---")
print(attr(df_cambodia$q18, "labels"))

print("--- Thailand q18 Labels ---")
print(attr(df_thailand$q18, "labels"))

# --- 3. Compare them! ---
# The warnings said the conflicts were on values: 4, 12, 15, and 19
# Look at the output for those numbers.

# (Just add View() to the end of your command)
df_vietnam %>%
  count(q18, sort = TRUE) %>%
  print(n = Inf)
df_cambodia %>%
  count(q18, sort = TRUE) %>%
  print(n = Inf)
df_thailand %>%
  count(q18, sort = TRUE) %>%
  print(n = Inf)

