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
    "The line shows daily ", ifelse(var == "TMAX", "highs", "lows"),
    " for ", target_year, ". The ribbons cover the historical range. The last date shown is ",
    format(max(this_year$date, na.rm = TRUE), "%b %d, %Y."))

  # --- Legend: fixed position, made-up data, visually clear ---
  x_range <- range(daily_stats$date, na.rm = TRUE)
  y_range <- range(c(daily_stats$min, daily_stats$max), na.rm = TRUE)
  legend_width_days <- 41
  legend_x_center <- x_range[1] + 0.5 * as.numeric(diff(x_range))
  legend_x <- seq(legend_x_center - (legend_width_days-1)/2, legend_x_center + (legend_width_days-1)/2, by = 1)

  legend_height <- 0.25 * diff(y_range)   # Taller legend box
  legend_top <- y_range[1] + 0.35 * diff(y_range)   # Lower on plot
  legend_bottom <- legend_top - legend_height

  # Create plausible, visually separated percentile bands (adjust as needed)
  legend_df <- data.frame(
    date = legend_x,
    min = legend_bottom + 0.00*legend_height + c(-2, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -2),
    x5 = legend_bottom + 0.10*legend_height + c(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1),
    x20 = legend_bottom + 0.25*legend_height,
    x40 = legend_bottom + 0.40*legend_height,
    x60 = legend_bottom + 0.60*legend_height,
    x80 = legend_bottom + 0.75*legend_height,
    x95 = legend_bottom + 0.90*legend_height + c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    max = legend_top + c(2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2)
  )

  # Fake black line: trough, wiggle, then peak
  legend_line <- legend_bottom + legend_height * (
    0.25 + 0.20 * cos(seq(-pi, pi, length.out = length(legend_x))) +
      0.10 * sin(seq(0, 4*pi, length.out = length(legend_x)))
  )
  legend_line_df <- data.frame(date = legend_x, temp = legend_line)

  # Record points at edges of the ribbon
  legend_record_points <- tibble(
    date = c(legend_x[which.max(legend_df$max)], legend_x[which.min(legend_df$min)]),
    value = c(max(legend_df$max), min(legend_df$min)),
    record_status = c(
      if (var == "TMAX") "record_high_tmax" else "record_high_tmin",
      if (var == "TMAX") "record_low_tmax" else "record_low_tmin"
    ),
    label = c(
      if (var == "TMAX") "all-time record daily high set this year" else "all-time record daily low set this year",
      if (var == "TMAX") "all-time record lowest daily high set this year" else "all-time record lowest daily low set this year"
    )
  )

  # Percentile labels
  legend_labels <- data.frame(
    date = max(legend_x) + 2,
    value = c(
      legend_bottom, 
      legend_bottom + 0.13*legend_height, 
      legend_bottom + 0.25*legend_height,
      legend_bottom + 0.40*legend_height,
      legend_bottom + 0.60*legend_height,
      legend_bottom + 0.75*legend_height,
      legend_bottom + 0.90*legend_height,
      legend_top
    ),
    label = c("min", "5th percentile", "20th", "40th", "60th", "80th", "95th percentile", "max")
  )

  # Build plot
  p <- daily_stats |>
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = min, ymax = max), fill = "#bdc9e1") +
    geom_ribbon(aes(ymin = x5, ymax = x95), fill = "#74a9cf") +
    geom_ribbon(aes(ymin = x20, ymax = x80), fill = "#2b8cbe") +
    geom_ribbon(aes(ymin = x40, ymax = x60), fill = "#045a8d") +
    geom_hline(yintercept = seq(-10, 100, 10), color = "white", lwd = 0.1) +
    geom_line(data = this_year, aes(y = !!sym(var)), lwd = 1.2) +
    geom_point(
      data = record_status,
      aes(y = this_year, color = record_status, fill = record_status, shape = record_status),
      size = 3
    ) +
    scale_color_manual(values = color_map, breaks = names(color_map)) +
    scale_fill_manual(values = color_map, breaks = names(color_map)) +
    scale_shape_manual(values = shape_map, breaks = names(shape_map)) +
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
      aes(x = date, y = value, label = label),
      hjust = 0, size = 4, fontface = "plain", inherit.aes = FALSE
    ) +
    geom_text(
      data = legend_record_points,
      aes(x = date, y = value, label = label),
      hjust = 0, vjust = c(-1, 2), size = 4, fontface = "plain", inherit.aes = FALSE
    ) +
    annotate("text", x = min(legend_x), y = legend_top, label = "Legend", hjust = 0, vjust = 1, fontface = "bold", size = 5) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_line(linetype = "dotted", linewidth = 0.2, colour = "gray"),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "linen", colour = "linen"),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 16),
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
