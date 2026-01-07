# Task Completion Checklist

## Before Marking Task Complete

### Code Quality
1. **R code**:
   - Run lintr: `Rscript src/r/utils/lint.R`
   - Check for proper indentation (2 spaces)
   - Verify snake_case naming
   - Add roxygen documentation for new functions

2. **Python code**:
   - Format: `black src/python/`
   - Lint: `ruff check src/python/`
   - Type check: `mypy src/python/`

3. **Pre-commit hooks**:
   ```bash
   pre-commit run --all-files
   ```

### Document Rendering
If changes affect Quarto documents:
```bash
# Quick check with dev profile (verbose output)
quarto render --profile dev

# Full render check
quarto render
```

### Data Pipeline
If changes affect data processing:
```bash
# Run R pipeline
Rscript scripts/build.R

# Run Python pipeline (if applicable)
python scripts/build.py
```

### Validation
- [ ] New variables are properly validated (range checks)
- [ ] Composites have reliability tests (Cronbach's α)
- [ ] Missing data patterns are documented
- [ ] Results are reproducible

### Testing
- [ ] R functions work as expected
- [ ] Python tests pass: `pytest`

### Documentation
- [ ] New functions have roxygen/docstring documentation
- [ ] Significant changes noted in comments
- [ ] Paper methodology matches code implementation

### Git Hygiene
- [ ] Changes on feature branch (not main)
- [ ] Meaningful commit messages
- [ ] No sensitive data committed
- [ ] Pre-commit hooks pass

## After Completion
1. Run full pre-commit check
2. Verify quarto renders successfully
3. Commit with descriptive message
4. Consider PR if substantial changes
