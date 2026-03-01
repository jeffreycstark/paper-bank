# R/helpers.R — Paper-level utilities
# Sources shared utilities from analysis/R/utils/

paper_dir <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."),
                           mustWork = FALSE)
if (!nzchar(paper_dir) || !dir.exists(paper_dir)) {
  paper_dir <- "/Users/jeffreystark/Development/Research/paper-bank/papers/01_vietnam_covid_paradox"
}

utils_dir <- file.path(paper_dir, "analysis", "R", "utils")
if (dir.exists(utils_dir)) {
  for (f in list.files(utils_dir, pattern = "\\.R$", full.names = TRUE)) {
    source(f)
  }
}
