library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggrepel)
library(cowplot)

# Load data globally
ghcn <- read_csv("data/GHCN_USC00441593.csv", show_col_types = FALSE)

# Helper: summary stats for a variable
get_daily_summary_stats <- function(var, exclude_year) {
  ghcn |>
    filter(year != exclude_year) |>
    select(month, day, !!sym(var)) |>
    rename(value = !!sym(var)) |>
    group_by(month, day) |>
    summarise(
      max = max(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      x5 = quantile(value, 0.05, na.rm = TRUE),
      x20 = quantile(value, 0.2, na.rm = TRUE),
      x40 = quantile(value, 0.4, na.rm = TRUE),
      x60 = quantile(value, 0.6, na.rm = TRUE),
      x80 = quantile(value, 0.8, na.rm = TRUE),
      x95 = quantile(value, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
}

# Helper: record status for a variable
get_record_status <- function(this_year, daily_stats, var) {
  this_year |>
    select(month, day, !!sym(var)) |>
    rename(this_year = !!sym(var)) |>
    inner_join(daily_stats, by = c("month", "day")) |>
    mutate(record_status = case_when(
      var == "TMAX" & this_year > max ~ "record_high_tmax",
      var == "TMAX" & this_year < min ~ "record_low_tmax",
      var == "TMIN" & this_year > max ~ "record_high_tmin",
      var == "TMIN" & this_year < min ~ "record_low_tmin",
      TRUE ~ "none"
    )) |>
    filter(record_status != "none")
}

# Helper: legend percentile stats (across all years)
get_legend_stats <- function() {
  ghcn |>
    group_by(year) |>
    summarise(
      avg = mean((TMAX + TMIN)/2, na.rm = TRUE)
    ) |>
    ungroup() |>
    summarise(
      min = min(avg, na.rm = TRUE),
      x5 = quantile(avg, 0.05, na.rm = TRUE),
      x20 = quantile(avg, 0.20, na.rm = TRUE),
      x40 = quantile(avg, 0.40, na.rm = TRUE),
      x60 = quantile(avg, 0.60, na.rm = TRUE),
      x80 = quantile(avg, 0.80, na.rm = TRUE),
      x95 = quantile(avg, 0.95, na.rm = TRUE),
      max = max(avg, na.rm = TRUE)
    )
}

# Plotting function for a single variable (TMAX or TMIN)
plot_temp_panel <- function(target_year, var = "TMAX", show_x_axis = TRUE) {
  is.leap.year <- lubridate::leap_year(target_year)
  this_year <- ghcn |> filter(year == target_year)
  daily_stats <- get_daily_summary_stats(var, target_year) |>
    mutate(date = as.Date(paste(target_year, month, day, sep = "-")),
           day_of_year = yday(date))
  if(!is.leap.year){
    daily_stats <- daily_stats |> filter(!(month == "02" & day == "29"))
  }
  record_status <- get_record_status(this_year, daily_stats, var)
  color_map <- c(
    "record_high_tmax" = "#d1495b",
    "record_low_tmax"  = "#6baed6",
    "record_high_tmin" = "#fd8d3c",
    "record_low_tmin"  = "#3182bd"
  )
  shape_map <- c(
    "record_high_tmax" = 24,  # Filled triangle up
    "record_low_tmax"  = 25,  # Filled triangle down
    "record_high_tmin" = 24,  # Filled triangle up
    "record_low_tmin"  = 25   # Filled triangle down
  )
  plot_title <- if (var == "TMAX") "Daily High Temperature" else "Daily Low Temperature"
  plot_subtitle <- paste0(
    "Line = ", ifelse(var == "TMAX", "daily high", "daily low"),
    "s for ", target_year, ". ",
    "Colored symbols: red = record high max, light blue = record low max, ",
    "orange = record high min, blue = record low min."
  )

  # --- Legend: fixed position below midpoint, always the same size ---
  x_range <- range(daily_stats$date, na.rm = TRUE)
  y_range <- range(c(daily_stats$min, daily_stats$max), na.rm = TRUE)

  legend_width_days <- 50
  legend_x_center <- x_range[1] + 0.5 * as.numeric(diff(x_range))
  legend_x <- seq(legend_x_center - legend_width_days/2, legend_x_center + legend_width_days/2, by = 1)

  legend_height <- 0.30 * diff(y_range)  # Legend is 30% of y-range tall
  legend_top <- y_range[1] + 0.5 * diff(y_range)  # Top at 50% up the y-range

  # Use percentiles from the whole dataset (for consistency)
  legend_stats <- daily_stats %>%
    summarise(
      min = min(min, na.rm = TRUE),
      x5 = min(x5, na.rm = TRUE),
      x20 = min(x20, na.rm = TRUE),
      x40 = min(x40, na.rm = TRUE),
      x60 = max(x60, na.rm = TRUE),
      x80 = max(x80, na.rm = TRUE),
      x95 = max(x95, na.rm = TRUE),
      max = max(max, na.rm = TRUE)
    )

  legend_vals <- as.numeric(legend_stats)
  legend_scaled <- (legend_vals - min(legend_vals)) / (max(legend_vals) - min(legend_vals))
  legend_y <- legend_top - legend_scaled * legend_height
  names(legend_y) <- names(legend_stats)

  legend_df <- data.frame(
    date = legend_x,
    min = legend_y["min"],
    x5 = legend_y["x5"],
    x20 = legend_y["x20"],
    x40 = legend_y["x40"],
    x60 = legend_y["x60"],
    x80 = legend_y["x80"],
    x95 = legend_y["x95"],
    max = legend_y["max"]
  )
  legend_line_df <- data.frame(
    date = legend_x,
    temp = legend_y["x40"]
  )
  legend_labels <- tibble(
    date = c(legend_x[1], legend_x[length(legend_x)]),
    value = c(legend_y["min"], legend_y["max"]),
    label = c("min", "max")
  ) %>%
    bind_rows(
      tibble(
        date = legend_x[length(legend_x)],
        value = c(legend_y["x5"], legend_y["x20"], legend_y["x40"], legend_y["x60"], legend_y["x80"], legend_y["x95"]),
        label = c("5th percentile", "20th", "40th", "60th", "80th", "95th")
      )
    )

  legend_record_points <- tibble(
    date = c(legend_x_center, legend_x_center),
    value = c(legend_y["max"], legend_y["min"]),
    record_status = c(
      if (var == "TMAX") "record_high_tmax" else "record_high_tmin",
      if (var == "TMAX") "record_low_tmax" else "record_low_tmin"
    )
  )

  # Build plot
  p <- daily_stats |>
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = min, ymax = max), fill = "#bdc9e1") +
    geom_ribbon(aes(ymin = x5, ymax = x95), fill = "#74a9cf") +
    geom_ribbon(aes(ymin = x20, ymax = x80), fill = "#2b8cbe") +
    geom_ribbon(aes(ymin = x40, ymax = x60), fill = "#045a8d") +
    geom_hline(yintercept = seq(-10, 100, 10), color = "white", lwd = 0.1) +
    geom_line(data = this_year, aes(y = !!sym(var)), lwd = 1) +
    geom_point(
      data = record_status,
      aes(y = this_year, color = record_status, fill = record_status, shape = record_status),
      size = 3
    ) +
    scale_color_manual(
      values = color_map,
      breaks = names(color_map)
    ) +
    scale_fill_manual(
      values = color_map,
      breaks = names(color_map)
    ) +
    scale_shape_manual(
      values = shape_map,
      breaks = names(shape_map)
    ) +
    scale_y_continuous(
      breaks = seq(-10, 100, 10),
      labels = scales::unit_format(suffix = "°"),
      expand = expansion(0.01),
      name = NULL,
      sec.axis = dup_axis()
    ) +
    scale_x_date(
      expand = expansion(0),
      breaks = unique(daily_stats$date[daily_stats$day == "15"]),
      labels = scales::label_date(format = "%b"),
      minor_breaks = unique(daily_stats$date[daily_stats$day == "01"]),
      name = NULL
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    # --- Draw legend centered at midyear, below midpoint ---
    geom_ribbon(data = legend_df, aes(x = date, ymin = min, ymax = max), fill = "#bdc9e1", inherit.aes = FALSE) +
    geom_ribbon(data = legend_df, aes(x = date, ymin = x5, ymax = x95), fill = "#74a9cf", inherit.aes = FALSE) +
    geom_ribbon(data = legend_df, aes(x = date, ymin = x20, ymax = x80), fill = "#2b8cbe", inherit.aes = FALSE) +
    geom_ribbon(data = legend_df, aes(x = date, ymin = x40, ymax = x60), fill = "#045a8d", inherit.aes = FALSE) +
    geom_line(data = legend_line_df, aes(x = date, y = temp), color = "black", lwd = 1, inherit.aes = FALSE) +
    geom_point(
      data = legend_record_points,
      aes(x = date, y = value, shape = record_status, fill = record_status),
      color = "black", size = 3, inherit.aes = FALSE
    ) +
    geom_text(
      data = legend_labels,
      aes(x = max(legend_df$date) + 2, y = value, label = label),
      hjust = 0, size = 3, fontface = "plain", inherit.aes = FALSE
    ) +
    annotate("text", x = min(legend_df$date), y = legend_y["max"], label = "Legend", hjust = 0, vjust = 1, fontface = "bold", size = 4) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_line(linetype = "dotted", linewidth = 0.2, colour = "gray"),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "linen", colour = "linen"),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 14, color = ifelse(var == "TMAX", "#d1495b", "#3182bd")),
      axis.ticks = element_blank(),
      axis.text.x = if (show_x_axis) element_text() else element_blank(),
      legend.position = "none"
    )
  return(p)
}



# Combined plot function for a year
generate_combined_temp_plot <- function(target_year, output_dir = "graphs/") {
  p_max <- plot_temp_panel(target_year, "TMAX", show_x_axis = FALSE)
  p_min <- plot_temp_panel(target_year, "TMIN", show_x_axis = TRUE)
  title <- ggdraw() +
    draw_label(
      paste("Temperature Records at McCormick Observatory -", target_year),
      fontface = "bold",
      size = 18
    )
  combined <- plot_grid(
    p_max, p_min,
    ncol = 1,
    align = "v",
    rel_heights = c(1, 1)
  )
  final_plot <- plot_grid(
    title,
    combined,
    ncol = 1,
    rel_heights = c(0.12, 1)
  )
  if(!dir.exists(output_dir)) dir.create(output_dir)
  output_file <- paste0(output_dir, "CombinedTemp_LMO_", target_year, ".png")
  ggsave(output_file, final_plot, width = 10, height = 10)
  return(final_plot)
}

# Example usage:
generate_combined_temp_plot(2023)
generate_combined_temp_plot(1997)
