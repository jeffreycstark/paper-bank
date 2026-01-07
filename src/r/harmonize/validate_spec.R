# src/r/harmonize/validate_spec.R
# YAML specification validation for harmonization

#' Validate harmonization YAML specification structure
#'
#' Checks that a parsed YAML specification has required fields and
#' correct types. Returns informative errors if validation fails.
#'
#' @param spec List: parsed YAML specification
#' @param var_id Optional: validate specific variable instead of all
#'
#' @return Invisibly returns TRUE if valid; stops with error otherwise
#'
#' @details
#' Top-level required fields:
#' - missing_conventions: named list of missing code vectors
#' - variables: list of variable specifications
#'
#' Per-variable required fields:
#' - id: character, unique identifier
#' - concept: character
#' - description: character
#' - source: named list (at least one wave required)
#' - type: character ("ordinal", "nominal", "continuous")
#' - harmonize: list with "default" method
#'
#' @export
validate_harmonize_spec <- function(spec, var_id = NULL) {

  errors <- list()

  # ---- Check top-level structure ----
  if (is.null(spec$missing_conventions)) {
    errors$missing_conventions <- "Required: missing_conventions"
  }

  if (is.null(spec$variables) || length(spec$variables) == 0) {
    errors$variables <- "Required: variables list (must have ≥1 variable)"
  }

  # ---- Check specific variable(s) ----
  var_ids <- if (!is.null(var_id)) {
    var_id
  } else {
    names(spec$variables) %||% character(0)
  }

  for (vid in var_ids) {

    var_spec <- spec$variables[[vid]]

    if (is.null(var_spec)) {
      errors[[paste0("variables.", vid)]] <- "Variable not found"
      next
    }

    # Required fields
    if (is.null(var_spec$id)) {
      errors[[paste0(vid, ".id")]] <- "Required: id"
    }

    if (is.null(var_spec$concept)) {
      errors[[paste0(vid, ".concept")]] <- "Required: concept"
    }

    if (is.null(var_spec$description)) {
      errors[[paste0(vid, ".description")]] <- "Required: description"
    }

    if (is.null(var_spec$source) || length(var_spec$source) == 0) {
      errors[[paste0(vid, ".source")]] <- "Required: source (named list, ≥1 wave)"
    }

    if (is.null(var_spec$type)) {
      errors[[paste0(vid, ".type")]] <- "Required: type"
    } else if (!var_spec$type %in% c("ordinal", "nominal", "continuous")) {
      errors[[paste0(vid, ".type")]] <- paste(
        "Invalid type. Must be: ordinal, nominal, continuous"
      )
    }

    # Harmonization spec
    if (is.null(var_spec$harmonize)) {
      errors[[paste0(vid, ".harmonize")]] <- "Required: harmonize"
    } else {
      if (is.null(var_spec$harmonize$default)) {
        errors[[paste0(vid, ".harmonize.default")]] <- "Required: default method"
      } else {
        method <- var_spec$harmonize$default$method
        if (!method %in% c("identity", "r_function")) {
          errors[[paste0(vid, ".harmonize.default.method")]] <- paste(
            "Invalid method. Must be: identity, r_function"
          )
        }
      }
    }

    # QC if present
    if (!is.null(var_spec$qc)) {
      if (!is.null(var_spec$qc$valid_range_by_wave)) {
        vr_list <- var_spec$qc$valid_range_by_wave

        for (wave_name in names(vr_list)) {
          vr <- vr_list[[wave_name]]

          if (!is.numeric(vr) || length(vr) != 2) {
            errors[[paste0(vid, ".qc.valid_range.", wave_name)]] <- paste(
              "Invalid range. Must be numeric vector of length 2: [min, max]"
            )
          }
        }
      }
    }
  }

  # ---- Report errors ----
  if (length(errors) > 0) {

    error_msg <- "❌ Specification validation failed:\n\n"

    for (i in seq_along(errors)) {
      error_msg <- paste0(
        error_msg,
        sprintf("  %d. %s: %s\n", i, names(errors)[i], errors[[i]])
      )
    }

    stop(error_msg, call. = FALSE)
  }

  invisible(TRUE)
}

#' Check if recoding functions exist
#'
#' Validates that all r_function harmonization methods have
#' corresponding functions loaded in environment.
#'
#' @param spec List: parsed YAML specification
#'
#' @return List of missing functions, or NULL if all present
#'
#' @export
check_recoding_functions <- function(spec) {

  missing_fns <- character()

  for (var_id in names(spec$variables)) {

    var_spec <- spec$variables[[var_id]]

    # Check default method
    if (var_spec$harmonize$default$method == "r_function") {
      fn_name <- var_spec$harmonize$default$fn

      if (!exists(fn_name, mode = "function")) {
        missing_fns <- c(missing_fns, fn_name)
      }
    }

    # Check wave-specific methods
    for (wave_name in names(var_spec$harmonize$by_wave %||% list())) {

      wave_rule <- var_spec$harmonize$by_wave[[wave_name]]

      if (wave_rule$method == "r_function") {
        fn_name <- wave_rule$fn

        if (!exists(fn_name, mode = "function")) {
          missing_fns <- c(missing_fns, fn_name)
        }
      }
    }
  }

  unique(missing_fns)
}
