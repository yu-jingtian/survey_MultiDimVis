#' Raw policy preference scores by year (2014–2021)
#'
#' A dataset of survey-based raw policy preference scores covering survey years
#' 2014 through 2021 (inclusive). Each row corresponds to one respondent
#' (identified by \code{case_id}) in one survey \code{year}.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{case_id}{Unique respondent identifier used for merging across datasets.}
#'   \item{year}{Integer survey year (2014--2021).}
#'   \item{immig}{Raw immigration policy preference score.}
#'   \item{enviro}{Raw environment policy preference score.}
#'   \item{abortion}{Raw abortion policy preference score.}
#'   \item{guns}{Raw gun policy preference score.}
#'   \item{healthcare}{Raw healthcare policy preference score.}
#'   \item{military}{Raw military policy preference score.}
#'   \item{spending}{Raw government spending policy preference score.}
#'   \item{trade}{Raw trade policy preference score.}
#' }
#'
#' @details
#' Join keys are \code{case_id} and \code{year}. Use \code{\link{policy_rf}} for
#' random-forest predicted scores, and \code{\link{demographics}} for covariates
#' and survey weights.
#'
#' @examples
#' data("policy_raw", package = "surveyMDV")
#' head(policy_raw)
"policy_raw"


#' Random-forest predicted policy scores by year (2014–2021)
#'
#' A dataset of random-forest (RF) predicted policy preference scores covering
#' survey years 2014 through 2021 (inclusive). Each row corresponds to one
#' respondent (identified by \code{case_id}) in one survey \code{year}.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{case_id}{Unique respondent identifier used for merging across datasets.}
#'   \item{year}{Integer survey year (2014--2021).}
#'   \item{immig_rf}{RF-predicted immigration policy preference score.}
#'   \item{enviro_rf}{RF-predicted environment policy preference score.}
#'   \item{abortion_rf}{RF-predicted abortion policy preference score.}
#'   \item{guns_rf}{RF-predicted gun policy preference score.}
#'   \item{healthcare_rf}{RF-predicted healthcare policy preference score.}
#'   \item{military_rf}{RF-predicted military policy preference score.}
#'   \item{spending_rf}{RF-predicted government spending policy preference score.}
#'   \item{trade_rf}{RF-predicted trade policy preference score.}
#' }
#'
#' @details
#' Predicted scores are produced from a random forest model using respondent
#' demographics. The sample weight column \code{weight_cumulative} is stored in
#' \code{\link{demographics}}. Join keys are \code{case_id} and \code{year}.
#'
#' @examples
#' data("policy_rf", package = "surveyMDV")
#' head(policy_rf)
"policy_rf"


#' Respondent demographics and weights by year (2014–2021)
#'
#' A dataset of respondent-level demographics (and survey weights) covering
#' survey years 2014 through 2021 (inclusive). Each row corresponds to one
#' respondent (identified by \code{case_id}) in one survey \code{year}.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{case_id}{Unique respondent identifier used for merging across datasets.}
#'   \item{year}{Integer survey year (2014--2021).}
#'   \item{partisan}{Partisanship coding used in analyses.}
#'   \item{race}{Race/ethnicity coding used in analyses.}
#'   \item{gender}{Gender coding used in analyses.}
#'   \item{age}{Respondent age (numeric or integer).}
#'   \item{educ}{Education coding used in analyses.}
#'   \item{rural_urban}{Rural/urban classification used in analyses.}
#'   \item{weight_cumulative}{Survey sample weight used (e.g., for RF fitting / weighted summaries).}
#' }
#'
#' @details
#' Join keys are \code{case_id} and \code{year}. Use \code{\link{policy_raw}} and
#' \code{\link{policy_rf}} for policy score tables.
#'
#' @examples
#' data("demographics", package = "surveyMDV")
#' head(demographics)
"demographics"
