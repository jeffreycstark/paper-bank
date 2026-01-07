# Intelligent Scale Detection Logic

## Overview
Implemented `detect_scale()` function in `src/r/data_prep_modules/1_harmonize_funs.R` that automatically identifies Likert scale ranges from raw survey data by recognizing standard missing code conventions.

## Algorithm

### Step 1: Separate Special Codes
- Any value < 1 is treated as a special/missing code (e.g., -1, -2)
- These are excluded from scale detection but recorded as missing codes

### Step 2: Find the Gap
Look for discontinuities in positive values (1, 2, 3, ...):
- **Gap found**: Scale ends at last value before gap
- **No gap**: Check for reserved missing code patterns

### Step 3: Reserved Missing Code Patterns

#### Pattern 1: High Missing Codes (97, 98, 99)
- Indicates survey uses standard missing codes
- Scale = highest value < 97
- Missing codes = 97, 98, 99
- **Example**: Values {1,2,3,4,5,6,97,98,99} → 6-point scale

#### Pattern 2: Low Missing Codes (7, 8, 9)  
- Used for small scales (≤ 6 points)
- **Rule**: If max(scale) ≤ 6 AND 7, 8, 9 appear → they're reserved missing codes
- Scale = max value before 7
- Missing codes = 7, 8, 9
- **Example**: Values {1,2,3,4,5,6,7,8,9} → 6-point scale with 7-9 as NA

#### Pattern 3: No Reserved Codes
- If no gap and no reserved patterns detected
- All positive values are part of scale
- Special codes only are missing

## Test Results

### ✅ 6-point scale with 7, 8, 9 reserved
```
Values: 1, 2, 3, 4, 5, 6, 7, 8, 9
Detection: 6-point scale (missing: 7, 8, 9)
```

### ✅ 10-point scale with 97, 98, 99 reserved
```
Values: 1-10, 97, 98, 99
Detection: 10-point scale (missing: 97, 98, 99)
```

### ✅ 10-point scale with special negative code
```
Values: -1, 1-10, 97, 98, 99
Detection: 10-point scale (missing: -1, 97, 98, 99)
```

### ✅ 4-point Likert scale (no missing codes in data)
```
Values: 1, 2, 3, 4
Detection: 4-point scale (missing: none)
```

## Function Signature

```r
detect_scale(x, var_name = "variable")
```

**Input**: 
- `x`: Numeric vector (raw with missing codes included)
- `var_name`: String for diagnostic messages

**Output**: 
List containing:
- `detected_scale`: Integer (1-10, 1-100, etc.)
- `missing_codes`: Numeric vector of identified missing code values
- `real_range`: Vector c(1, detected_scale) - always starts at 1

## Usage in Harmonization

The function provides diagnostic output and can be called before harmonization:

```r
scale_info <- detect_scale(raw_vector, var_name = "q97_W2")
cat(sprintf("Detected: %d-point scale\n", scale_info$detected_scale))
cat(sprintf("Missing codes: %s\n", paste(scale_info$missing_codes, collapse=", ")))
```

## Design Principles

1. **Data-driven detection**: Uses actual value distribution, not assumptions
2. **Convention-aware**: Recognizes standard survey missing code patterns
3. **Gap-based**: Looks for discontinuities as scale boundaries
4. **Special code handling**: Treats negative codes as always missing (not scale)
5. **Diagnostic output**: Provides clear feedback about detected patterns

## Integration with Harmonization Pipelines

This function can be integrated into data prep modules to:
- Auto-validate expected scales
- Identify unexpected scale ranges
- Automatically determine recoding strategies
- Generate comprehensive QC reports

The function is production-ready and handles the complexity of real survey data with mixed missing code conventions.
