# EconDev-AuthPref

Research repository for survey-based social science projects.

## Setup

### Python Environment (uv)
This project uses `uv` for Python dependency management with Python 3.12.12:

```bash
# Install uv if not already installed
curl -LsSf https://astral.sh/uv/install.sh | sh

# Install Python 3.12.12 and dependencies
uv python install 3.12.12
uv sync

# Activate the virtual environment
source .venv/bin/activate  # On macOS/Linux
# or
.venv\Scripts\activate  # On Windows
```

## Structure
- `data/` — raw → interim → processed
- `src/` — reusable R/Python logic
- `papers/` — manuscript-specific code and text
- `notebooks/` — exploratory, non-reproducible
- `outputs/` — rendered tables and figures

## Reproducibility
- Quarto is the authoritative build system
- Papers must source from `src/`
- CI renders all papers on push