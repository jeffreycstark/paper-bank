# src/r/harmonize/harmonize.R
# Core functions for cross-wave variable harmonization

# ==============================================================================
# HELPER OPERATORS AND FUNCTIONS
# ==============================================================================

#' Null coalescing operator
#'
#' Returns first non-null value (alternative if left side is NULL)
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Clean missing codes to NA
#'
#' @param x Numeric vector
#' @param missing_codes Codes to treat as NA
#' @return Vector with missing codes converted to NA
apply_missing <- function(x, missing_codes) {
  if (length(missing_codes) == 0) {
    return(x)
  }
  x[x %in% missing_codes] <- NA_real_
  x
}

# ==============================================================================
# MAIN HARMONIZATION ENGINE
# ==============================================================================

#' Harmonize a variable across waves using YAML specification
#'
#' Takes a variable specification (from YAML) and applies harmonization rules
#' to produce a harmonized variable across all waves.
#'
#' @param var_spec List: one YAML variable entry with fields:
#'   - id: variable identifier
#'   - source: list(w1="q001", w2="q1", ...)
#'   - missing: list(use_convention="treat_as_na")
#'   - harmonize: list(default=..., by_wave=...)
#'   - qc: list(valid_range_by_wave=...)
#'
#' @param waves List of named data frames: list(w1=df1, w2=df2, ...)
#'
#' @param missing_conventions Named list of missing code vectors
#'   e.g. list(treat_as_na = c(-1, 0, 7, 8, 9))
#'
#' @return List of harmonized vectors, one per wave:
#'   list(w1 = numeric(n1), w2 = numeric(n2), ...)
#'
#' @details
#' Harmonization workflow:
#' 1. Extract source variable from wave data
#' 2. Convert to numeric, apply missing code handling
#' 3. Select harmonization rule (default or wave-specific)
#' 4. Apply method:
#'    - "identity": pass through
#'    - "r_function": call recoding function (e.g., safe_reverse_5pt)
#' 5. Optional QC: check valid range (warn only, don't coerce)
#'
#' @examples
#' \dontrun{
#' # Load YAML spec and wave data
#' spec <- yaml::read_yaml("src/config/harmonize/economy.yml")
#' waves <- list(
#'   w1 = readRDS("data/processed/w1.rds"),
#'   w2 = readRDS("data/processed/w2.rds")
#' )
#'
#' # Harmonize one variable
#' econ <- harmonize_variable(
#'   var_spec = spec$variables[["econ_national_now"]],
#'   waves = waves,
#'   missing_conventions = spec$missing_conventions
#' )
#'
#' # econ$w1, econ$w2, ... are harmonized vectors
#' }
#'
#' @export
harmonize_variable <- function(
  var_spec,
  waves,
  missing_conventions
) {

  out <- list()

  for (wave_name in names(waves)) {

    df <- waves[[wave_name]]

    # ---- extract source variable ----
    src <- var_spec$source[[wave_name]]

    if (is.null(src) || !src %in% names(df)) {
      # Source variable doesn't exist in this wave - return all NA
      out[[wave_name]] <- rep(NA_real_, nrow(df))
      next
    }

    # Convert to numeric (suppress warnings about coercion)
    x <- suppressWarnings(as.numeric(df[[src]]))

    # ---- apply missing code handling ----
    miss_convention_key <- var_spec$missing$use_convention %||% NULL
    missing_codes <- if (!is.null(miss_convention_key)) {
      convention <- missing_conventions[[miss_convention_key]]
      # Handle both direct vector and nested structure with 'codes' field
      if (is.list(convention) && !is.null(convention$codes)) {
        as.numeric(convention$codes)
      } else {
        as.numeric(convention)
      }
    } else {
      numeric(0)
    }

    x <- apply_missing(x, missing_codes)

    # ---- select harmonization rule ----
    default_rule <- var_spec$harmonize$default %||% list(method = "identity")
    wave_rule <- var_spec$harmonize$by_wave[[wave_name]] %||% default_rule

    # ---- apply harmonization method ----
    if (wave_rule$method == "identity") {

      # No transformation
      x_harm <- x

    } else if (wave_rule$method == "r_function") {

      fn_name <- wave_rule$fn
      if (!exists(fn_name, mode = "function")) {
        stop("❌ Recoding function not found: ", fn_name)
      }

      fn <- get(fn_name, mode = "function")

      # Call function with full wave data for semantic validation
      x_harm <- fn(
        x,
        data = df,
        var_name = src,
        validate_all = wave_rule$validate_all %||% NULL
      )

    } else {
      stop("❌ Unknown harmonization method: ", wave_rule$method)
    }

    # ---- optional QC: range check (warn, don't coerce) ----
    vr <- var_spec$qc$valid_range_by_wave[[wave_name]] %||% NULL
    if (!is.null(vr)) {
      bad <- !is.na(x_harm) & (x_harm < vr[1] | x_harm > vr[2])
      if (any(bad)) {
        warning(
          sprintf(
            "⚠️  %s (%s): %d values outside valid range [%s,%s]",
            var_spec$id, wave_name, sum(bad), vr[1], vr[2]
          ),
          call. = FALSE
        )
      }
    }

    out[[wave_name]] <- x_harm
  }

  out
}

#' Harmonize multiple variables from YAML specification
#'
#' Applies harmonize_variable() to all variables in a YAML spec,
#' returning a list with one element per variable.
#'
#' @param spec List: parsed YAML specification
#' @param waves List of named data frames
#' @param silent Logical: suppress messages?
#'
#' @return List of harmonized variables:
#'   list(econ_national_now = list(w1=..., w2=..., ...),
#'        politics_trust = list(w1=..., w2=..., ...))
#'
#' @export
harmonize_all <- function(spec, waves, silent = FALSE) {

  results <- list()

  var_ids <- names(spec$variables)

  for (var_id in var_ids) {

    if (!silent) {
      message(sprintf("Harmonizing: %s", var_id))
    }

    tryCatch({
      results[[var_id]] <- harmonize_variable(
        var_spec = spec$variables[[var_id]],
        waves = waves,
        missing_conventions = spec$missing_conventions
      )
    }, error = function(e) {
      warning(sprintf("❌ %s: %s", var_id, e$message), call. = FALSE)
    })
  }

  results
}
