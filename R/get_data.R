#' Get raw policy scores (2014–2021)
#'
#' Loads and returns the raw policy score table, optionally filtered by year and
#' reduced to selected columns.
#'
#' @param year Integer vector of survey years to keep. Default \code{NULL} keeps all years (2014--2021).
#' @param cols Character vector of column names to return. Default \code{NULL} returns all columns.
#'   Note: \code{case_id} and \code{year} are always included to support joins.
#'
#' @return A data frame containing raw policy scores.
#' @export
#'
#' @examples
#' # All years, all columns
#' df <- get_policy_raw()
#'
#' # Single year, subset of columns
#' df16 <- get_policy_raw(year = 2016, cols = c("immig", "guns"))
get_policy_raw <- function(year = NULL, cols = NULL) {
  data("policy_raw", package = "surveyMDV", envir = environment())

  df <- policy_raw

  if (!is.null(year)) {
    year <- as.integer(year)
    df <- df[df$year %in% year, , drop = FALSE]
  }

  if (!is.null(cols)) {
    cols <- unique(as.character(cols))
    keep <- unique(c("case_id", "year", cols))
    missing_cols <- setdiff(keep, names(df))
    if (length(missing_cols) > 0) {
      stop("Unknown column(s) in `cols`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    df <- df[, keep, drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("No rows returned. Check `year` (available: 2014–2021) and your filters.", call. = FALSE)
  }

  df
}



#' Get RF-predicted policy scores (2014–2021)
#'
#' Loads and returns the random-forest predicted policy score table, optionally
#' filtered by year and reduced to selected columns.
#'
#' @param year Integer vector of survey years to keep. Default \code{NULL} keeps all years (2014--2021).
#' @param cols Character vector of column names to return. Default \code{NULL} returns all columns.
#'   Note: \code{case_id} and \code{year} are always included to support joins.
#'
#' @return A data frame containing RF-predicted policy scores.
#' @export
#'
#' @examples
#' df <- get_policy_rf()
#' df21 <- get_policy_rf(year = 2021, cols = c("immig_rf", "guns_rf"))
get_policy_rf <- function(year = NULL, cols = NULL) {
  data("policy_rf", package = "surveyMDV", envir = environment())

  df <- policy_rf

  if (!is.null(year)) {
    year <- as.integer(year)
    df <- df[df$year %in% year, , drop = FALSE]
  }

  if (!is.null(cols)) {
    cols <- unique(as.character(cols))
    keep <- unique(c("case_id", "year", cols))
    missing_cols <- setdiff(keep, names(df))
    if (length(missing_cols) > 0) {
      stop("Unknown column(s) in `cols`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    df <- df[, keep, drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("No rows returned. Check `year` (available: 2014–2021) and your filters.", call. = FALSE)
  }

  df
}



#' Get respondent demographics (2014–2021)
#'
#' Loads and returns the demographics table, optionally filtered by year and
#' reduced to selected columns.
#'
#' @param year Integer vector of survey years to keep. Default \code{NULL} keeps all years (2014--2021).
#' @param cols Character vector of column names to return. Default \code{NULL} returns all columns.
#'   Note: \code{case_id} and \code{year} are always included to support joins.
#'
#' @return A data frame containing respondent demographics and weights.
#' @export
#'
#' @examples
#' demo <- get_demographics(year = 2018, cols = c("partisan", "race", "weight_cumulative"))
get_demographics <- function(year = NULL, cols = NULL) {
  data("demographics", package = "surveyMDV", envir = environment())

  df <- demographics

  if (!is.null(year)) {
    year <- as.integer(year)
    df <- df[df$year %in% year, , drop = FALSE]
  }

  if (!is.null(cols)) {
    cols <- unique(as.character(cols))
    keep <- unique(c("case_id", "year", cols))
    missing_cols <- setdiff(keep, names(df))
    if (length(missing_cols) > 0) {
      stop("Unknown column(s) in `cols`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    df <- df[, keep, drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("No rows returned. Check `year` (available: 2014–2021) and your filters.", call. = FALSE)
  }

  df
}
