library(haven)
library(tidyverse)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }
  getwd()
}

analysis_dir <- get_script_dir()
project_root <- normalizePath(file.path(analysis_dir, "..", "..", ".."))

# Load WVS
alt_paths <- list.files(
  file.path(project_root, "data", "raw", "wvs_wave7"),
  pattern = "\\.dta$",
  full.names = TRUE
)
wvs_path <- alt_paths[1]
cat("Loading:", wvs_path, "\n")

wvs_raw <- read_dta(wvs_path, encoding = "latin1")
hk <- wvs_raw |> filter(B_COUNTRY_ALPHA == "HKG" | B_COUNTRY == 344)

# Check Q235 label and distribution
cat("\n=== Q235 ===\n")
cat("Label:", attr(hk$Q235, "label"), "\n")
cat("Distribution:\n")
print(table(as.numeric(hk$Q235), useNA = "always"))

# Also check Q250 which is sometimes the democracy importance item
if ("Q250" %in% names(hk)) {
  cat("\n=== Q250 ===\n")
  cat("Label:", attr(hk$Q250, "label"), "\n")
  cat("Distribution:\n")
  print(table(as.numeric(hk$Q250), useNA = "always"))
}

# Search for democracy-related variables
cat("\n=== Variables with 'democr' in label ===\n")
for (v in names(hk)) {
  lab <- attr(hk[[v]], "label")
  if (!is.null(lab) && grepl("democr", lab, ignore.case = TRUE)) {
    cat(v, ":", lab, "\n")
    vals <- as.numeric(hk[[v]])
    vals_valid <- vals[vals > 0]
    if (length(vals_valid) > 0) {
      cat("  Valid N:", length(vals_valid), " Mean:", round(mean(vals_valid), 2),
          " Range:", min(vals_valid), "-", max(vals_valid), "\n")
    }
  }
}

# Also check confidence variables
cat("\n=== Confidence variable labels ===\n")
for (v in c("Q65", "Q69", "Q70", "Q71", "Q72")) {
  if (v %in% names(hk)) {
    lab <- attr(hk[[v]], "label")
    cat(v, ":", lab, "\n")
    vals <- as.numeric(hk[[v]])
    vals_valid <- vals[vals > 0]
    cat("  Valid N:", length(vals_valid), " Mean:", round(mean(vals_valid), 2), "\n")
  }
}
