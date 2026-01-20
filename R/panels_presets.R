# R/panels_presets.R
# Exported panel preset generators (Option 2).

#' Preset panels: Population + three parties
#'
#' Returns the standard four-row panel set: Population, Republican, Independent, Democrat.
#'
#' @return A list of panel specifications, each with elements \code{label} and \code{filter}.
#' @export
panels_partisan <- function() {
  list(
    list(label = "Population", filter = function(df) df),
    list(label = "Republican", filter = function(df) df[df$partisan == "Rep.", , drop = FALSE]),
    list(label = "Independent", filter = function(df) df[df$partisan == "Ind.", , drop = FALSE]),
    list(label = "Democrat",   filter = function(df) df[df$partisan == "Dem.", , drop = FALSE])
  )
}

.party_label <- function(party) {
  if (party == "Rep.") return("Rep.")
  if (party == "Dem.") return("Dem.")
  if (party == "Ind.") return("Ind.")
  party
}

.gender_is_female <- function(x) x %in% c("Female", "Woman", "Women")
.gender_is_male   <- function(x) x %in% c("Male", "Men", "Man")

.panels_party_decompose <- function(party) {
  lab <- .party_label(party)

  list(
    list(
      label = paste0(lab),
      filter = function(df) df[df$partisan == party, , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Big Metro"),
      filter = function(df) df[df$partisan == party & df$rural_urban %in% c(1), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Other Co."),
      filter = function(df) df[df$partisan == party & df$rural_urban %in% c(2,3,4,5,6,7,8,9), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Non-college"),
      filter = function(df) df[df$partisan == party & df$educ %in% c(1,2,3) & !is.na(df$partisan), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " College"),
      filter = function(df) df[df$partisan == party & df$educ %in% c(4,5,6) & !is.na(df$partisan), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Female"),
      filter = function(df) df[df$partisan == party & .gender_is_female(df$gender), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Male"),
      filter = function(df) df[df$partisan == party & .gender_is_male(df$gender), , drop = FALSE]
    )
  )
}

#' Preset panels: Republican decomposition
#'
#' @return A list of panel specifications.
#' @export
panels_rep_decompose <- function() {
  .panels_party_decompose("Rep.")
}

#' Preset panels: Democrat decomposition
#'
#' @return A list of panel specifications.
#' @export
panels_dem_decompose <- function() {
  .panels_party_decompose("Dem.")
}

#' Preset panels: Independent decomposition
#'
#' @return A list of panel specifications.
#' @export
panels_ind_decompose <- function() {
  .panels_party_decompose("Ind.")
}
