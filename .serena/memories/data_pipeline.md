# Data Pipeline - Harmonization Workflow

## Full Pipeline Execution
Run these scripts in order to regenerate the harmonized dataset:

```bash
# Step 1: Load raw wave data
Rscript src/r/data_prep_modules/0_load_waves.R

# Step 2: Run harmonization (27 YAML specs, 330 variables)
Rscript src/r/data_prep_modules/2_harmonize_all.R

# Step 3: Create combined 6-wave dataset
Rscript src/r/data_prep_modules/99_create_final_dataset.R

# Step 4: Generate CSV and Parquet (run in R)
library(arrow)
d <- readRDS("data/processed/abs_econdev_authpref.rds")
write.csv(d, "data/processed/abs_econdev_authpref.csv", row.names = FALSE)
write_parquet(d, "data/processed/abs_econdev_authpref.parquet")
```

## Output Files
| File | Location | Size |
|------|----------|------|
| Per-wave masters | `outputs/master_w[1-6].rds` | ~2-5MB each |
| Combined RDS | `data/processed/abs_econdev_authpref.rds` | ~6MB |
| Combined CSV | `data/processed/abs_econdev_authpref.csv` | ~91MB (in .gitignore) |
| Combined Parquet | `data/processed/abs_econdev_authpref.parquet` | ~6MB |

## Dataset Statistics (as of 2025-01-12)
- **Total rows**: 110,721
- **Total columns**: 332 (330 variables + wave + idnumber)
- **YAML specs**: 27
- **Waves**: w1 (12,217), w2 (19,798), w3 (19,436), w4 (20,667), w5 (26,951), w6 (11,652)

## YAML Specification Location
All harmonization specs are in: `src/config/harmonize_validated/*.yml`

## Key Scripts
- `src/r/harmonize/harmonize.R` - Core harmonization engine
- `src/r/utils/recoding.R` - Recoding functions (safe_reverse_*, etc.)
- `src/r/harmonize/validate_spec.R` - YAML validation
