# R/recoding.R
# Recoding functions for Asian Barometer analysis

safe_reverse_3pt <- function(x,
                              data = NULL,
                              var_name = NULL,
                              missing_codes = c(-1, 0, 7, 8, 9),
                              validate_all = NULL) {

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # ---- reversal logic ----
  dplyr::case_when(
    x %in% 1:3 ~ 4 - x,
    x %in% missing_codes ~ NA_real_,
    TRUE ~ NA_real_
  )
}

safe_reverse_4pt <- function(x,
                              data = NULL,
                              var_name = NULL,
                              missing_codes = c(-1, 0, 7, 8, 9),
                              validate_all = NULL) {

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # ---- reversal logic ----
  dplyr::case_when(
    x %in% 1:4 ~ 5 - x,
    x %in% missing_codes ~ NA_real_,
    TRUE ~ NA_real_
  )
}

safe_reverse_5pt <- function(x,
                              data = NULL,
                              var_name = NULL,
                              missing_codes = c(-1, 0, 7, 8, 9),
                              validate_all = NULL) {

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # ---- reversal logic ----
  dplyr::case_when(
    x %in% 1:5 ~ 6 - x,
    x %in% missing_codes ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# ==============================================================================
# IDENTITY FUNCTIONS (NO REVERSAL, ONLY MISSING CODE HANDLING)
# ==============================================================================

safe_3pt_none <- function(x,
                           data = NULL,
                           var_name = NULL,
                           missing_codes = c(-1, 0, 7, 8, 9),
                           validate_all = NULL) {

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # ---- identity logic (no reversal) ----
  dplyr::case_when(
    x %in% 1:3 ~ as.numeric(x),
    x %in% missing_codes ~ NA_real_,
    TRUE ~ NA_real_
  )
}

safe_4pt_none <- function(x,
                           data = NULL,
                           var_name = NULL,
                           missing_codes = c(-1, 0, 7, 8, 9),
                           validate_all = NULL) {

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # ---- identity logic (no reversal) ----
  dplyr::case_when(
    x %in% 1:4 ~ as.numeric(x),
    x %in% missing_codes ~ NA_real_,
    TRUE ~ NA_real_
  )
}

safe_5pt_none <- function(x,
                           data = NULL,
                           var_name = NULL,
                           missing_codes = c(-1, 0, 7, 8, 9),
                           validate_all = NULL) {

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # ---- identity logic (no reversal) ----
  dplyr::case_when(
    x %in% 1:5 ~ as.numeric(x),
    x %in% missing_codes ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# ==============================================================================
# 6-POINT SCALE FUNCTIONS
# ==============================================================================

safe_reverse_6pt <- function(x,
                              data = NULL,
                              var_name = NULL,
                              missing_codes = c(-1, 0, 7, 8, 9, 97, 98, 99),
                              validate_all = NULL) {

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # ---- reversal logic ----
  dplyr::case_when(
    x %in% 1:6 ~ 7 - x,
    x %in% missing_codes ~ NA_real_,
    TRUE ~ NA_real_
  )
}

safe_6pt_none <- function(x,
                           data = NULL,
                           var_name = NULL,
                           missing_codes = c(-1, 0, 7, 8, 9, 97, 98, 99),
                           validate_all = NULL) {

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # ---- identity logic (no reversal) ----
  dplyr::case_when(
    x %in% 1:6 ~ as.numeric(x),
    x %in% missing_codes ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# ==============================================================================
# 6-POINT TO 4-POINT COLLAPSE (FOR ABS WAVE 5)
# ==============================================================================

safe_6pt_to_4pt <- function(x,
                             data = NULL,
                             var_name = NULL,
                             missing_codes = c(-1, 0, 97, 98, 99),
                             needs_reversal = TRUE,
                             validate_all = NULL) {
  #' Collapse 6-point scale to 4-point (for ABS Wave 5)
  #'

  #' This function handles Wave 5's 6-point trust scale and collapses it to 4-point
  #' to match other waves.
  #'
  #' Wave 5 scale (before reversal):
  #'   1=Trust fully, 2=Trust a lot, 3=Trust somewhat,
  #'   4=Distrust somewhat, 5=Distrust a lot, 6=Distrust fully
  #'
  #' After reversal (if needs_reversal=TRUE):
  #'   6=Trust fully, 5=Trust a lot, 4=Trust somewhat,
  #'   3=Distrust somewhat, 2=Distrust a lot, 1=Distrust fully
  #'
  #' Collapse to 4-point:
  #'   6,5 → 4 (A great deal of trust)
  #'   4   → 3 (Quite a lot of trust)
  #'   3   → 2 (Not very much trust)
  #'   2,1 → 1 (None at all)

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # ---- first handle missing codes ----
  x_clean <- dplyr::case_when(
    x %in% missing_codes ~ NA_real_,
    TRUE ~ x
  )

  # ---- if needs reversal, reverse first ----
  # Wave 5 is coded 1=high trust, 6=low trust, so we reverse to match other waves
  if (needs_reversal) {
    x_clean <- dplyr::case_when(
      !is.na(x_clean) ~ 7 - x_clean,
      TRUE ~ NA_real_
    )
  }

  # ---- collapse 6-point to 4-point ----
  # After reversal: 6,5=highest trust -> 4
  #                 4=moderate-high -> 3
  #                 3=moderate-low -> 2
  #                 2,1=lowest trust -> 1
  dplyr::case_when(
    x_clean %in% c(6, 5) ~ 4,
    x_clean == 4 ~ 3,
    x_clean == 3 ~ 2,
    x_clean %in% c(2, 1) ~ 1,
    TRUE ~ NA_real_
  )
}

# ==============================================================================
# PARTY IDENTIFICATION FUNCTIONS
# ==============================================================================

recode_has_party <- function(x,
                              data = NULL,
                              var_name = NULL,
                              missing_codes = c(-1, 0, 97, 98, 99),
                              validate_all = NULL) {
  #' Recode party ID to binary has_party indicator
  #'
  #' Converts party identification variable to binary:
  #'   - 90, 1595, 1597 (no party codes) -> 0
  #'   - Other valid party codes -> 1
  #'   - Missing codes -> NA

  # ---- semantic validation (optional but recommended) ----
  if (!is.null(validate_all)) {

    if (is.null(data) || is.null(var_name)) {
      stop("❌ validate_all requires both `data` and `var_name`")
    }

    if (!var_name %in% names(data)) {
      stop(glue::glue("❌ {var_name}: variable not found in data"))
    }

    qtext <- attr(data[[var_name]], "label")

    if (is.null(qtext) || is.na(qtext) || !nzchar(qtext)) {
      stop(glue::glue("❌ {var_name}: missing question label for validation"))
    }

    for (pattern in validate_all) {
      if (!grepl(pattern, qtext, ignore.case = TRUE)) {
        stop(glue::glue(
          "❌ {var_name}: expected concept '{pattern}' not found in question text:\n'{qtext}'"
        ))
      }
    }
  }

  # No party codes: 90 = "Don't feel close to any political party"
  # Some waves may use country-specific "no party" codes (1595, 1597)
  no_party_codes <- c(90, 1595, 1597)

  # ---- recode to binary ----
  dplyr::case_when(
    x %in% missing_codes ~ NA_real_,
    x %in% no_party_codes ~ 0,
    x > 0 ~ 1,
    TRUE ~ NA_real_
  )
}
