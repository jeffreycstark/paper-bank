# econdev-authpref Project - Claude Navigation Guide

**Project**: Democracy & Economic Development Research
**Current Branch**: `main`
**Status**: Active development - Harmonization complete (330 variables, 6 waves)

---

## 🔧 Available MCP Servers

Several MCP servers are available to enhance Claude's capabilities on this project:

| Server | Purpose | Key Uses |
|--------|---------|----------|
| **Serena** | Semantic code understanding + project memory | Symbol operations, session persistence, **read project memories first** |
| **Context7** | Library documentation lookup | R/Python package docs, framework patterns |
| **Memory** | Knowledge graph storage | Entity relationships, cross-session learning |
| **Morphllm** | Bulk code transformations | Pattern-based edits across multiple files |
| **Sequential Thinking** | Multi-step reasoning | Complex debugging, architectural analysis |
| **Magic (21st.dev)** | UI component generation | If any frontend work needed |
| **Tavily** | Web search | Current documentation, research |
| **Git** | Git operations | Commits, branches, diffs |
| **Filesystem** | File operations | Read/write outside project |

### Serena Project Memories
**Always check these first** - they contain project-specific knowledge:
```
list_memories() → Read relevant ones before starting work
```

| Memory | Contents |
|--------|----------|
| `project_overview` | Tech stack, structure, key packages |
| `data_pipeline` | How to run harmonization pipeline |
| `w6_case_sensitivity` | Common W6 column name issues and fixes |
| `country_codes` | All 16 country numeric codes |
| `variable_scales` | Scale conventions (1-5 political action, 1-4 trust, etc.) |
| `common_analyses` | R code patterns for filtering, cross-tabs, cohorts |
| `r_utility_functions` | Available R helper functions |
| `style_conventions` | Code style guidelines |
| `suggested_commands` | Useful shell commands |

### Note on R
No R MCP is currently configured. R code is executed via `Rscript` in Bash. If an R MCP would be useful for interactive R sessions, ask the user.

---

## 🎯 Quick Skills Reference

### Codebook YAML Entry Creation Skills
**Location**: `src/r/codebook/`  
**Purpose**: Automate YAML template generation from ABS survey search results

#### Main Entry Points
```r
# Load the workflow module
source("src/r/codebook/codebook_workflow.R")

# Generate YAML from search results (3-liner)
results <- extract_matches("search term", w1, w2, w3, w4, w5, w6)
yaml_str <- generate_codebook_yaml(results, concept = "economy")
cat(yaml_str)  # Review, then save
```

#### Key Functions
| Function | Purpose | File |
|----------|---------|------|
| `generate_codebook_yaml()` | Main entry: search results → YAML | codebook_workflow.R |
| `detect_scale_type()` | Identify 5pt/4pt/6pt scales | codebook_analysis.R |
| `detect_reversals()` | Find waves with flipped semantics | codebook_analysis.R |
| `analyze_search_results()` | Generate markdown analysis report | codebook_workflow.R |
| `batch_generate_yaml()` | Process multiple concepts at once | codebook_workflow.R |

#### Documentation Files (Read in This Order)
1. **QUICK_REFERENCE.md** - Fast lookup, one-liners, cheat sheet
2. **SKILL_SEARCH_AND_ANALYZE.md** - Complete skill documentation with examples
3. **README.md** - Detailed technical reference
4. **REAL_EXAMPLE_WALKTHROUGH.md** - Real data example walkthrough
5. **BUILD_SUMMARY.md** - What was built, statistics, completeness

---

## 📁 Project Structure

### Source Code Organization

```
src/
├── r/                           # R code
│   ├── codebook/               # ⭐ CODEBOOK SKILLS (see above)
│   │   ├── codebook_analysis.R
│   │   ├── codebook_workflow.R
│   │   ├── test_codebook.R
│   │   ├── example_satisfaction_democracy.R
│   │   ├── SKILL_SEARCH_AND_ANALYZE.md
│   │   ├── QUICK_REFERENCE.md
│   │   ├── README.md
│   │   ├── INDEX.md
│   │   ├── REAL_EXAMPLE_WALKTHROUGH.md
│   │   └── BUILD_SUMMARY.md
│   │
│   ├── harmonize/              # Harmonization engine
│   │   ├── harmonize.R
│   │   ├── validate_spec.R
│   │   ├── report_harmonization.R
│   │   └── README.md
│   │
│   ├── survey/                 # Survey utilities
│   │   ├── codebook_tools.R
│   │   ├── harmonize_vars.R
│   │   └── load_data.R
│   │
│   ├── utils/                  # General utilities
│   │   ├── search.R            # extract_matches() function
│   │   ├── recoding.R
│   │   ├── validation.R
│   │   └── ...
│   │
│   └── models/                 # Statistical models
│
├── config/
│   └── harmonize/              # Generated YAML specs (output location)
│       ├── economy.yml
│       └── democracy_satisfaction.yml
│
├── python/                     # Python utilities
│   ├── ingest/
│   ├── export/
│   └── validation/
│
└── scripts/
    └── harmonize_democracy_satisfaction.R

data/
├── raw/                        # Original survey data
│   ├── wave1/, wave2/, ..., wave6/
│   └── README.md
├── interim/                    # Intermediate processing
└── processed/                  # Final harmonized data

outputs/
├── figures/
├── tables/
└── HARMONIZATION_RESULTS_SUMMARY.md
```

---

## 🔄 Data Processing Pipeline

```
Raw Survey Data (data/raw/)
           ↓
    Load & Parse (src/r/survey/load_data.R)
           ↓
  Search Variables (extract_matches in src/r/utils/search.R)
           ↓
⭐ Generate YAML (src/r/codebook/codebook_workflow.R) ⭐
           ↓
  User Reviews YAML (edit src/config/harmonize/*.yml)
           ↓
  Harmonize Data (src/r/harmonize/harmonize.R)
           ↓
  Validate Results (src/r/harmonize/validate_spec.R)
           ↓
Harmonized Output (data/processed/)
           ↓
Generate Reports (src/r/harmonize/report_harmonization.R)
```

---

## 📊 Key Datasets

### Survey Data (6 waves)
| Wave | Country/Focus | Data File | Processing |
|------|---------------|-----------|------------|
| W1 | Base survey | `data/raw/wave1/` | Parsed in w1 |
| W2 | Follow-up | `data/raw/wave2/` | Parsed in w2 |
| W3 | Expansion | `data/raw/wave3/` | Parsed in w3 |
| W4 | Extended | `data/raw/wave4/` | Parsed in w4 |
| W5 | Update | `data/raw/wave5/` | Parsed in w5 |
| W6 | Recent | `data/raw/wave6/` | Parsed in w6 |

### Loading Data in R
```r
# After survey data is loaded into w1, w2, ..., w6
# Use in codebook skills:
results <- extract_matches("search term", w1, w2, w3, w4, w5, w6)
```

---

## 🛠️ Common Workflows

### Workflow 1: Generate YAML for New Concept
```r
# 1. Load codebook module
source("src/r/codebook/codebook_workflow.R")

# 2. Search
results <- extract_matches("your concept here", w1, w2, w3, w4, w5, w6)

# 3. Generate YAML
yaml_str <- generate_codebook_yaml(results, concept = "concept_name")

# 4. Review (look for scale/reversal detection)
cat(yaml_str)

# 5. Save to config
writeLines(yaml_str, "src/config/harmonize/concept_name.yml")

# 6. Edit manually: fill id, confirm harmonize methods

# 7. Use in harmonization
spec <- yaml::read_yaml("src/config/harmonize/concept_name.yml")
harmonized <- harmonize_all(spec, waves = list(w1, w2, w3, w4, w5, w6))
```

### Workflow 2: Batch Process Multiple Concepts
```r
source("src/r/codebook/codebook_workflow.R")

results_list <- list(
  economy = extract_matches("economic condition", w1, w2, w3, w4, w5, w6),
  politics = extract_matches("trust government", w1, w2, w3, w4, w5, w6),
  satisfaction = extract_matches("democracy satisfaction", w1, w2, w3, w4, w5, w6)
)

batch_generate_yaml(results_list, output_dir = "src/config/harmonize/")
# Creates: economy.yml, politics.yml, satisfaction.yml
```

### Workflow 3: Test & Debug
```r
source("src/r/codebook/test_codebook.R")

# Run all tests
testthat::test_file("src/r/codebook/test_codebook.R")

# Or test specific function
results <- data.frame(...)
detected <- detect_scale_type(1:5, c("Bad","Poor","Neutral","Good","Excellent"))
print(detected)  # Check confidence, type, direction
```

---

## 🧪 Testing

### Codebook Module Tests
```bash
# In R console
source("src/r/codebook/test_codebook.R")
testthat::test_file("src/r/codebook/test_codebook.R")
```

**Coverage**: 40+ tests across 8 categories
- Scale detection (5pt, 4pt, 6pt, continuous)
- Direction detection (ascending/descending)
- Reversal detection (semantic opposites)
- Question grouping (q1/q001 variants)
- YAML generation
- Batch processing
- Error handling

---

## 📝 Documentation Map

### By Use Case

**"I want a quick example"**
→ Read `src/r/codebook/QUICK_REFERENCE.md`

**"I want to understand the skill"**
→ Read `src/r/codebook/SKILL_SEARCH_AND_ANALYZE.md`

**"I want complete technical details"**
→ Read `src/r/codebook/README.md`

**"I want to see it work with real data"**
→ Read `src/r/codebook/REAL_EXAMPLE_WALKTHROUGH.md`

**"I want to know what was built and why"**
→ Read `src/r/codebook/BUILD_SUMMARY.md`

**"I want a function index"**
→ Read `src/r/codebook/INDEX.md`

---

## 🔗 Integration Points

### Upstream Dependencies
- `extract_matches()` - From `src/r/utils/search.R`
- Wave data: `w1`, `w2`, `w3`, `w4`, `w5`, `w6` (loaded survey data)
- SPSS/SPAV files in `data/raw/`

### Downstream Consumers
- `harmonize_all()` - In `src/r/harmonize/harmonize.R`
- `validate_harmonize_spec()` - Validates YAML specs
- `report_harmonization()` - QC reports on results

### Related Files
- `src/config/harmonize/*.yml` - Generated YAML config files (output location)
- `src/r/utils/recoding.R` - Recoding functions referenced in YAML
- `src/r/survey/codebook_tools.R` - Other codebook utilities

---

## ⚙️ Configuration

### Default Behavior
- Scale detection: Automatically identifies 5pt, 4pt, 6pt, 0-10, continuous
- Reversal detection: Checks semantic keywords (bad/good/agree/disagree)
- Confidence scoring: All detections include 0-1 confidence metric
- Output format: YAML with comments and TODO sections

### Customization
Most functions work with no configuration. If you need to:
- Adjust confidence thresholds → Edit YAML generation comment thresholds
- Change keywords → Modify `detect_label_direction()` in `codebook_analysis.R`
- Adjust scale types → Modify `detect_scale_type()` mapping logic

---

## 🚀 Performance Notes

| Operation | Time (6 waves, 4 Qs) |
|-----------|---------------------|
| Parse results | <1ms |
| Detect scales | ~4ms |
| Detect reversals | ~5ms |
| Group questions | <1ms |
| Generate YAML | ~10ms |
| **Total** | **~50ms** |

Scales linearly with number of matches.

---

## ✅ Completeness Status

### Codebook Module
- [x] Core detection functions (scale, direction, reversal)
- [x] YAML generation with auto-fill
- [x] Batch processing
- [x] Comprehensive test suite (40+ tests)
- [x] Complete documentation (5 guides)
- [x] Real data example
- [x] Production ready

### Project Status
- [x] Data loaded (6 waves: 110,721 total respondents)
- [x] Codebook module complete
- [x] Harmonization engine functional
- [x] Full dataset harmonized (330 variables across 27 YAML specs)
- [x] Output files: RDS, CSV, Parquet in `data/processed/`

---

## 📞 Troubleshooting

### "Function not found"
```r
source("src/r/codebook/codebook_workflow.R")
source("src/r/codebook/codebook_analysis.R")
```

### "Results format wrong"
→ Use `extract_matches()` from `src/r/utils/search.R`  
→ Or check format in `SKILL_SEARCH_AND_ANALYZE.md`

### "Confidence score too low"
→ Check value labels are present in data  
→ May indicate sparse values or ambiguous scale

### "Reversal detection wrong"
→ Look at YAML comments with confidence scores  
→ Edit YAML manually if semantic detection fails  
→ Update keywords in `detect_label_direction()` if pattern is new

---

## 🎓 Learning Path

1. **Start here**: `src/r/codebook/QUICK_REFERENCE.md` (5 min)
2. **Then**: `src/r/codebook/REAL_EXAMPLE_WALKTHROUGH.md` (10 min)
3. **Deep dive**: `src/r/codebook/SKILL_SEARCH_AND_ANALYZE.md` (20 min)
4. **Reference**: `src/r/codebook/README.md` (as needed)
5. **Code**: Look at `codebook_workflow.R` and `codebook_analysis.R` (20 min)
6. **Test**: Run `test_codebook.R` to see it work (5 min)

---

## 🔮 Future Skills & Development

### In Progress
- [ ] Direct SPSS codebook parsing
- [ ] Interactive refinement mode
- [ ] Multi-language support

### Planned
- [ ] Machine learning improver (learn from past edits)
- [ ] Question registry (known renames)
- [ ] Visualization tools
- [ ] API wrapper

---

## Last Updated
- **Date**: 2025-01-12
- **By**: Claude Code
- **Branch**: main
- **Dataset**: 110,721 rows × 332 columns (330 harmonized variables + wave + id)

---

## Quick Links

| Need | Location |
|------|----------|
| Load codebook | `source("src/r/codebook/codebook_workflow.R")` |
| YAML examples | `src/config/harmonize/democracy_satisfaction.yml` |
| Test data | `src/r/codebook/example_satisfaction_democracy.R` |
| How-to guide | `src/r/codebook/SKILL_SEARCH_AND_ANALYZE.md` |
| Function list | `src/r/codebook/INDEX.md` |
| Build notes | `src/r/codebook/BUILD_SUMMARY.md` |

---

## 📊 Harmonized Dataset Quick Reference

### Loading the Data
```r
d <- readRDS("data/processed/abs_econdev_authpref.rds")
# Or: arrow::read_parquet("data/processed/abs_econdev_authpref.parquet")
```

### Key Variables
| Category | Examples |
|----------|----------|
| Demographics | country, age, gender, urban_rural, education_level |
| Political Action | action_demonstration, action_petition (scale 1-5, higher=more active) |
| Trust | trust_president, trust_parliament, trust_police (scale 1-4, higher=more trust) |
| Democracy | dem_sat_national, dem_best_form, dem_always_preferable |
| Economy | econ_national_now, econ_family_now (scale 1-5, higher=better) |
| Social Media | sm_use_facebook, sm_use_twitter (1=Yes, 2=No, W6 only) |

### Country Codes
1=Japan, 2=Hong Kong, 3=Korea, 4=China, 5=Mongolia, 6=Philippines, 7=Taiwan, 8=Thailand, 9=Indonesia, 10=Singapore, 11=Vietnam, 12=Cambodia, 13=Malaysia, 14=Myanmar, 15=Australia, 18=India

### Running the Pipeline
```bash
Rscript src/r/data_prep_modules/0_load_waves.R
Rscript src/r/data_prep_modules/2_harmonize_all.R
Rscript src/r/data_prep_modules/99_create_final_dataset.R
```
