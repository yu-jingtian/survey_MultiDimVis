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

.check_required_cols <- function(df, cols, context = "") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop("Missing column(s) in ", context, ": ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

.usable_xy <- function(df, policy_vec) {
  x <- df[[policy_vec[1]]]
  y <- df[[policy_vec[2]]]
  !is.na(x) & !is.na(y)
}

.blank_panel <- function() {
  ggplot2::ggplot() +
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

# ---- data prep ----

.prepare_year_df <- function(year, type) {
  year <- as.integer(year)
  type <- match.arg(type, c("raw", "rf"))

  if (type == "raw") {
    pol <- get_policy_raw(year = year, cols = NULL)
  } else {
    pol <- get_policy_rf(year = year, cols = NULL)
  }
  demo <- get_demographics(year = year, cols = NULL)

  # base join (keeps package lightweight; no need for dplyr here)
  merge(pol, demo, by = c("case_id", "year"), all = FALSE)
}

.apply_panel_filter <- function(df, panel) {
  if (is.null(panel$filter)) return(df)
  panel$filter(df)
}

# ---- scaling ----
# scale specs always computed from the REFERENCE dataset only (population or a chosen panel),
# then re-used for all subgroup panels.

.compute_scale_spec_rf <- function(df, policy_vec, breaks = seq(0, 1, by = 0.05)) {
  .check_required_cols(df, policy_vec, context = "RF scaling")

  x <- df[[policy_vec[1]]]
  y <- df[[policy_vec[2]]]

  # structural missing for the reference
  if (all(is.na(x)) || all(is.na(y))) {
    return(list(max_freq = NA_integer_, total = 0L, empty = TRUE))
  }

  ok <- !is.na(x) & !is.na(y)
  if (!any(ok)) {
    return(list(max_freq = NA_integer_, total = 0L, empty = TRUE))
  }

  x_cut <- cut(x[ok], breaks = breaks, include.lowest = TRUE, right = TRUE)
  y_cut <- cut(y[ok], breaks = breaks, include.lowest = TRUE, right = TRUE)

  ft <- as.data.frame(table(x_cut, y_cut))

  list(
    max_freq = max(ft$Freq, na.rm = TRUE),
    total    = sum(ft$Freq, na.rm = TRUE),
    empty    = FALSE
  )
}

.compute_scale_spec_raw <- function(df, policy_vec) {
  .check_required_cols(df, policy_vec, context = "Raw scaling")

  x <- df[[policy_vec[1]]]
  y <- df[[policy_vec[2]]]

  if (all(is.na(x)) || all(is.na(y))) {
    return(list(max_freq = NA_integer_, total = 0L, empty = TRUE))
  }

  ok <- !is.na(x) & !is.na(y)
  if (!any(ok)) {
    return(list(max_freq = NA_integer_, total = 0L, empty = TRUE))
  }

  fx <- factor(x[ok])
  fy <- factor(y[ok])

  ft <- as.data.frame(table(fx, fy))

  list(
    max_freq = max(ft$Freq, na.rm = TRUE),
    total    = sum(ft$Freq, na.rm = TRUE),
    empty    = FALSE
  )
}

.compute_scale_specs <- function(df_year_list, panels, type,
                                 policy_vec,
                                 scale = c("across_years", "within_year"),
                                 scale_ref = c("population", "panel"),
                                 scale_ref_panel = 1L,
                                 breaks = seq(0, 1, by = 0.05)) {
  type  <- match.arg(type,  c("raw", "rf"))
  scale <- match.arg(scale, c("across_years", "within_year"))
  scale_ref <- match.arg(scale_ref, c("population", "panel"))

  n_panels <- length(panels)
  n_years  <- length(df_year_list)

  specs <- vector("list", n_panels)
  for (r in seq_len(n_panels)) specs[[r]] <- vector("list", n_years)

  compute_one <- function(df) {
    if (type == "rf") .compute_scale_spec_rf(df, policy_vec, breaks = breaks)
    else             .compute_scale_spec_raw(df, policy_vec)
  }

  get_ref_df <- function(df_year) {
    if (scale_ref == "population") return(df_year)
    idx <- as.integer(scale_ref_panel)
    if (is.na(idx) || idx < 1 || idx > length(panels)) {
      stop("Invalid scale_ref_panel index.", call. = FALSE)
    }
    .apply_panel_filter(df_year, panels[[idx]])
  }

  if (scale == "within_year") {
    # One reference-derived scale per YEAR, reused for all subgroup panels in that year
    for (c in seq_len(n_years)) {
      ref_df   <- get_ref_df(df_year_list[[c]])
      ref_spec <- compute_one(ref_df)
      for (r in seq_len(n_panels)) specs[[r]][[c]] <- ref_spec
    }
    return(specs)
  }

  # across_years (FIXED):
  # Compute the reference scale per year, then take the max across years.
  # Do NOT pool rows across years (that inflates counts and washes out colors).
  ref_specs_by_year <- vector("list", n_years)
  for (c in seq_len(n_years)) {
    ref_df <- get_ref_df(df_year_list[[c]])
    .check_required_cols(ref_df, policy_vec, context = "scale reference (across_years)")
    ref_specs_by_year[[c]] <- compute_one(ref_df)
  }

  # Keep only non-empty years
  nonempty <- vapply(ref_specs_by_year, function(s) !is.null(s$empty) && !isTRUE(s$empty), logical(1))
  if (!any(nonempty)) {
    global_spec <- list(max_freq = NA_integer_, total = 0L, empty = TRUE)
  } else {
    max_freqs <- vapply(ref_specs_by_year[nonempty], function(s) s$max_freq, numeric(1))
    totals    <- vapply(ref_specs_by_year[nonempty], function(s) s$total,    numeric(1))

    global_spec <- list(
      max_freq = as.integer(max(max_freqs, na.rm = TRUE)),
      # For RF tiles, total isn't used; for raw points, this keeps sizing stable.
      # Using max(total) is a safe cross-year reference (avoids pooled inflation).
      total    = as.integer(max(totals, na.rm = TRUE)),
      empty    = FALSE
    )
  }

  # Reuse the same global spec for every panel-row and year-column
  for (c in seq_len(n_years)) {
    for (r in seq_len(n_panels)) specs[[r]][[c]] <- global_spec
  }

  specs
}


# ---- panel drawing ----

.draw_panel_rf <- function(df, policy_vec, scale_spec,
                           breaks = seq(0, 1, by = 0.05),
                           low = "#c6dbef", mid = "#F4B811", high = "#CC2929") {
  .check_required_cols(df, policy_vec, context = "RF panel drawing")

  # subgroup may be empty even if year is valid
  ok <- .usable_xy(df, policy_vec)
  if (!any(ok)) return(.blank_panel())

  if (!is.null(scale_spec$empty) && isTRUE(scale_spec$empty)) return(.blank_panel())
  if (is.null(scale_spec$max_freq) || is.na(scale_spec$max_freq) || scale_spec$max_freq <= 0) return(.blank_panel())

  x_cut <- cut(df[[policy_vec[1]]][ok], breaks = breaks, include.lowest = TRUE, right = TRUE)
  y_cut <- cut(df[[policy_vec[2]]][ok], breaks = breaks, include.lowest = TRUE, right = TRUE)

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

  ok <- .usable_xy(df, policy_vec)
  if (!any(ok)) return(.blank_panel())

  if (!is.null(scale_spec$empty) && isTRUE(scale_spec$empty)) return(.blank_panel())
  if (is.null(scale_spec$total) || scale_spec$total <= 0) return(.blank_panel())
  if (is.null(scale_spec$max_freq) || is.na(scale_spec$max_freq) || scale_spec$max_freq <= 0) return(.blank_panel())

  fx <- factor(df[[policy_vec[1]]][ok])
  fy <- factor(df[[policy_vec[2]]][ok])

  ft <- as.data.frame(table(fx, fy))
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
