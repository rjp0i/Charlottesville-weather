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

# Plotting function for a single variable (TMAX or TMIN)
plot_temp_panel <- function(target_year, var = "TMAX", show_x_axis = TRUE) {
  is.leap.year <- leap_year(target_year)
  this_year <- ghcn |> filter(year == target_year)
  daily_stats <- get_daily_summary_stats(var, target_year) |>
    mutate(date = as.Date(paste(target_year, month, day, sep = "-")),
           day_of_year = yday(date))
  if(!is.leap.year){
    daily_stats <- daily_stats |> filter(!(month == "02" & day == "29"))
  }
  record_status <- get_record_status(this_year, daily_stats, var)
  # Color and shape mappings
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
    "record_low_tmin"  = 21   # Filled circle
  )
  # Title and subtitle
  plot_title <- if (var == "TMAX") {
    "Daily High Temperature"
  } else {
    "Daily Low Temperature"
  }
  plot_subtitle <- paste0(
    "Line = ", ifelse(var == "TMAX", "daily high", "daily low"),
    "s for ", target_year, ". ",
    "Colored symbols: red = record high max, light blue = record low max, ",
    "orange = record high min, blue = record low min."
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
      name = "Record Types",
      values = color_map,
      breaks = names(color_map)
    ) +
    scale_fill_manual(
      name = "Record Types",
      values = color_map,
      breaks = names(color_map)
    ) +
    scale_shape_manual(
      name = "Record Types",
      values = shape_map,
      breaks = names(shape_map)
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 4, fill = color_map)),
      shape = guide_legend(override.aes = list(size = 4, fill = color_map)),
      fill = "none"
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
      legend.position = c(0.5, 0.85),
      legend.justification = c(0.5, 1),
      legend.box.background = element_rect(fill = "white", colour = "gray50"),
      legend.box.margin = margin(6, 6, 6, 6)
    )
  return(p)
}

# Combined plot function for a year
generate_combined_temp_plot <- function(target_year, output_dir = "graphs/") {
  # Top: TMAX, Bottom: TMIN
  p_max <- plot_temp_panel(target_year, "TMAX", show_x_axis = FALSE)
  p_min <- plot_temp_panel(target_year, "TMIN", show_x_axis = TRUE)
  # Title
  title <- ggdraw() + 
    draw_label(
      paste("Temperature Records at McCormick Observatory -", target_year),
      fontface = "bold",
      size = 18
    )
  # Stack plots (no subplot labels)
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
  # Save
  if(!dir.exists(output_dir)) dir.create(output_dir)
  output_file <- paste0(output_dir, "CombinedTemp_USC00441593_", target_year, ".png")
  ggsave(output_file, final_plot, width = 10, height = 8)
  return(final_plot)
}

# Example usage:
generate_combined_temp_plot(2023)
generate_combined_temp_plot(1997)

# Batch for all years:
# all_years <- sort(unique(ghcn$year))
# purrr::walk(all_years, generate_combined_temp_plot)
