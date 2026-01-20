# R/heat_wrappers.R
# Exported user-facing plotting wrappers built on the core.

#' Multiyear (or single-year) heatmap grid for policy pairs
#'
#' Creates a grid of heatmaps comparing \code{years} (columns) across subgroup panels (rows),
#' for a pair of policy dimensions. Works for either raw scores (\code{type = "raw"}) or
#' RF-predicted scores (\code{type = "rf"}).
#'
#' @param years Integer vector of years (2014--2021). Length 1 produces a single-year grid.
#' @param policy_x,policy_y Policy variable names. For \code{type="raw"}, use raw names
#'   (e.g., \code{"abortion"}). For \code{type="rf"}, you may pass base names (e.g., \code{"abortion"})
#'   or suffixed names (e.g., \code{"abortion_rf"}); base names will be converted to \code{*_rf}.
#' @param type One of \code{"rf"} or \code{"raw"}.
#' @param panels A panel specification list, e.g. \code{panels_partisan()},
#'   \code{panels_rep_decompose()}, \code{panels_dem_decompose()}, \code{panels_ind_decompose()}.
#' @param scale One of \code{"across_years"} (default) or \code{"within_year"}.
#' @param breaks Numeric breaks used for RF binning (ignored for raw). Default is \code{seq(0,1,0.05)}.
#'
#' @return A \code{GGally} \code{ggmatrix} object.
#' @export
#'
#' @examples
#' # Example (requires data shipped in the package):
#' # p <- plot_policy_heatgrid(years = 2019:2021, policy_x = "abortion", policy_y = "healthcare",
#' #                           type = "rf", panels = panels_partisan())
plot_policy_heatgrid <- function(years,
                                 policy_x,
                                 policy_y,
                                 type = c("rf", "raw"),
                                 panels = panels_partisan(),
                                 scale = c("across_years", "within_year"),
                                 breaks = seq(0, 1, by = 0.05)) {
  type  <- match.arg(type,  c("rf", "raw"))
  scale <- match.arg(scale, c("across_years", "within_year"))

  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package 'GGally' is required for grid output. Please install it (install.packages('GGally')).",
         call. = FALSE)
  }

  years <- as.integer(years)
  if (any(is.na(years))) stop("`years` must be coercible to integer.", call. = FALSE)

  # Load/merge per-year data
  df_year_list <- lapply(years, function(y) .prepare_year_df(y, type = type))

  # Resolve policy column names for requested type
  policy_vec <- .resolve_policy_names(type, policy_x, policy_y)

  # Compute scaling specs across panels x years
  scale_specs <- .compute_scale_specs(
    df_year_list = df_year_list,
    panels       = panels,
    type         = type,
    policy_vec   = policy_vec,
    scale        = scale,
    breaks       = breaks
  )

  # Draw all panels (row-major ordering for ggmatrix)
  n_panels <- length(panels)
  n_years  <- length(years)
  p_list <- vector("list", n_panels * n_years)
  k <- 1

  for (r in seq_len(n_panels)) {
    for (c in seq_len(n_years)) {
      df_rc <- .apply_panel_filter(df_year_list[[c]], panels[[r]])
      p_list[[k]] <- .draw_panel(
        df = df_rc,
        type = type,
        policy_vec = policy_vec,
        scale_spec = scale_specs[[r]][[c]],
        breaks = breaks
      )
      k <- k + 1
    }
  }

  GGally::ggmatrix(
    plots = p_list,
    nrow  = n_panels,
    ncol  = n_years,
    yAxisLabels = vapply(panels, function(p) p$label, character(1)),
    xAxisLabels = years,
    showStrips  = NULL
  ) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 16, face = "bold"),
      strip.text.y = ggplot2::element_text(size = 8,  face = "bold")
    )
}
