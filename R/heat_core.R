# R/heat_core.R
# Internal (non-exported) helpers for data prep, scaling, and drawing single panels.

# ---- utilities ----

.is_null_or_empty <- function(x) is.null(x) || length(x) == 0

.resolve_policy_names <- function(type, policy_x, policy_y) {
  type <- match.arg(type, c("raw", "rf"))
  if (type == "raw") return(c(policy_x, policy_y))

  # rf: accept either base names (e.g., "abortion") or already-suffixed (e.g., "abortion_rf")
  px <- if (grepl("_rf$", policy_x)) policy_x else paste0(policy_x, "_rf")
  py <- if (grepl("_rf$", policy_y)) policy_y else paste0(policy_y, "_rf")
  c(px, py)
}

.prepare_year_df <- function(year, type) {
  # Join policy + demographics for the requested year (2014–2021)
  year <- as.integer(year)
  type <- match.arg(type, c("raw", "rf"))

  if (type == "raw") {
    pol <- get_policy_raw(year = year, cols = NULL)
  } else {
    pol <- get_policy_rf(year = year, cols = NULL)
  }
  demo <- get_demographics(year = year, cols = NULL)

  # Safe base join (avoid requiring dplyr for core join)
  merge(pol, demo, by = c("case_id", "year"), all = FALSE)
}

.apply_panel_filter <- function(df, panel) {
  # panel is a list(label=..., filter=fn)
  if (is.null(panel$filter)) return(df)
  panel$filter(df)
}

.check_required_cols <- function(df, cols, context = "") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop("Missing column(s) in ", context, ": ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

# ---- scaling ----

.compute_scale_spec_rf <- function(df, policy_vec, breaks = seq(0, 1, by = 0.05)) {
  # returns list(max_freq=..., total=...)
  .check_required_cols(df, policy_vec, context = "RF scaling")
  x <- df[[policy_vec[1]]]
  y <- df[[policy_vec[2]]]

  x_cut <- cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE)
  y_cut <- cut(y, breaks = breaks, include.lowest = TRUE, right = TRUE)

  ft <- as.data.frame(table(x_cut, y_cut))
  list(
    max_freq = max(ft$Freq, na.rm = TRUE),
    total    = sum(ft$Freq, na.rm = TRUE)
  )
}

.compute_scale_spec_raw <- function(df, policy_vec) {
  # raw points: treat as categorical (factors)
  .check_required_cols(df, policy_vec, context = "Raw scaling")
  x <- factor(df[[policy_vec[1]]])
  y <- factor(df[[policy_vec[2]]])

  ft <- as.data.frame(table(x, y))
  list(
    max_freq = max(ft$Freq, na.rm = TRUE),
    total    = sum(ft$Freq, na.rm = TRUE)
  )
}

.compute_scale_specs <- function(df_year_list, panels, type,
                                 policy_vec, scale = c("across_years", "within_year"),
                                 breaks = seq(0, 1, by = 0.05)) {
  type  <- match.arg(type,  c("raw", "rf"))
  scale <- match.arg(scale, c("across_years", "within_year"))

  # Returns a matrix-like list: scale_specs[[row_panel]][[col_year]]
  # If scale == across_years, each row_panel shares one spec across all years (computed from "scale_ref_panel")
  n_panels <- length(panels)
  n_years  <- length(df_year_list)

  specs <- vector("list", n_panels)
  for (r in seq_len(n_panels)) {
    specs[[r]] <- vector("list", n_years)
  }

  compute_one <- function(df) {
    if (type == "rf") .compute_scale_spec_rf(df, policy_vec, breaks = breaks)
    else             .compute_scale_spec_raw(df, policy_vec)
  }

  if (scale == "within_year") {
    for (c in seq_len(n_years)) {
      for (r in seq_len(n_panels)) {
        df_rc <- .apply_panel_filter(df_year_list[[c]], panels[[r]])
        specs[[r]][[c]] <- compute_one(df_rc)
      }
    }
    return(specs)
  }

  # across_years: compute a single spec per row-panel across all years by pooling
  for (r in seq_len(n_panels)) {
    pooled <- df_year_list[[1]][0, , drop = FALSE]
    for (c in seq_len(n_years)) {
      pooled <- rbind(pooled, .apply_panel_filter(df_year_list[[c]], panels[[r]]))
    }
    row_spec <- compute_one(pooled)
    for (c in seq_len(n_years)) specs[[r]][[c]] <- row_spec
  }
  specs
}

# ---- panel drawing ----

.draw_panel_rf <- function(df, policy_vec, scale_spec,
                           breaks = seq(0, 1, by = 0.05),
                           low = "#c6dbef", mid = "#F4B811", high = "#CC2929") {
  .check_required_cols(df, policy_vec, context = "RF panel drawing")

  x <- df[[policy_vec[1]]]
  y <- df[[policy_vec[2]]]
  x_cut <- cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE)
  y_cut <- cut(y, breaks = breaks, include.lowest = TRUE, right = TRUE)

  ft <- as.data.frame(table(x_cut, y_cut))
  names(ft) <- c("col1", "col2", "Freq")
  ft$col1 <- as.character(ft$col1)
  ft$col2 <- as.character(ft$col2)

  ggplot2::ggplot(ft, ggplot2::aes(x = col1, y = col2, fill = Freq)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      midpoint = 0.5 * scale_spec$max_freq,
      limits   = c(0, scale_spec$max_freq),
      low = low, mid = mid, high = high
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::coord_equal()
}

.draw_panel_raw <- function(df, policy_vec, scale_spec,
                            low = "#c6dbef", mid = "#F4B811", high = "#CC2929",
                            total_size = 12) {
  .check_required_cols(df, policy_vec, context = "Raw panel drawing")

  x <- factor(df[[policy_vec[1]]])
  y <- factor(df[[policy_vec[2]]])
  ft <- as.data.frame(table(x, y))
  names(ft) <- c("col1", "col2", "Freq")

  ft$prop <- ft$Freq / scale_spec$total
  ft$prop_scaled <- sqrt(ft$prop) * total_size

  ggplot2::ggplot(ft, ggplot2::aes(x = col1, y = col2, color = Freq, size = prop_scaled)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_gradient2(
      midpoint = 0.5 * scale_spec$max_freq,
      low = low, mid = mid, high = high
    ) +
    ggplot2::scale_size_continuous(range = c(0, max(ft$prop_scaled, na.rm = TRUE))) +
    ggplot2::theme(
      legend.position = "none",
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "lightgrey"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::coord_equal()
}

.draw_panel <- function(df, type, policy_vec, scale_spec, breaks = seq(0, 1, by = 0.05)) {
  type <- match.arg(type, c("raw", "rf"))
  if (type == "rf") .draw_panel_rf(df, policy_vec, scale_spec, breaks = breaks)
  else              .draw_panel_raw(df, policy_vec, scale_spec)
}
