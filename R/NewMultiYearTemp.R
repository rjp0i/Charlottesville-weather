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
plot_temp_panel <- function(target_year, var = "TMAX", show_x_axis = TRUE, y_shift = 60) {
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

  # --- Legend block construction ---
  legend_days <- 165:201
  legend_df <- daily_stats %>%
    filter(day_of_year %in% legend_days) %>%
    mutate(
      max = max - y_shift,
      min = min - y_shift,
      x5 = x5 - y_shift,
      x20 = x20 - y_shift,
      x40 = x40 - y_shift,
      x60 = x60 - y_shift,
      x80 = x80 - y_shift,
      x95 = x95 - y_shift
    )

  # Hand-crafted, plausible fake data for the solid black line
  
  # --- Legend block construction ---

legend_days <- 165:201
origin_date <- as.Date(paste0(target_year, "-01-01"))

# 1. The ribbon data for the legend (shifted below the main data)
legend.df <- daily_stats %>%
  filter(day_of_year %in% legend_days) %>%
  mutate(
    max = max - y_shift,
    min = min - y_shift,
    x5 = x5 - y_shift,
    x20 = x20 - y_shift,
    x40 = x40 - y_shift,
    x60 = x60 - y_shift,
    x80 = x80 - y_shift,
    x95 = x95 - y_shift,
    date = origin_date + day_of_year - 1
  )

# 2. Hand-crafted, plausible fake data for the solid black line
legend.line.df <- tibble(
  day_of_year = legend_days,
  temp = case_when(
    day_of_year == 165 ~ legend.df$x40[legend.df$day_of_year == 165],
    day_of_year == 168 ~ legend.df$x40[legend.df$day_of_year == 165] + 3,
    day_of_year == 172 ~ legend.df$x40[legend.df$day_of_year == 165] - 4,
    day_of_year == 177 ~ legend.df$min[legend.df$day_of_year == 177] - 1,
    day_of_year == 180 ~ legend.df$x20[legend.df$day_of_year == 180] - 1,
    day_of_year == 182 ~ legend.df$x60[legend.df$day_of_year == 182] + 1,
    day_of_year == 185 ~ legend.df$x60[legend.df$day_of_year == 185] - 6,
    day_of_year == 189 ~ legend.df$max[legend.df$day_of_year == 189] + 1,
    day_of_year == 194 ~ legend.df$x60[legend.df$day_of_year == 194],
    day_of_year == 198 ~ legend.df$x40[legend.df$day_of_year == 198],
    day_of_year == 201 ~ legend.df$x60[legend.df$day_of_year == 201],
    TRUE ~ NA_real_
  )
) %>%
  filter(!is.na(temp)) %>%
  mutate(date = origin_date + day_of_year - 1)

# 3. Labels for percentiles (with date column)
legend.labels <- legend.df %>%
  pivot_longer(cols = c(max, min, starts_with("x")),
               names_to = "levels") %>%
  mutate(label = case_when(
    levels == "max" ~ "max",
    levels == "min" ~ "min",
    levels == "x95" ~ "95th percentile of past years",
    TRUE ~ paste0(str_sub(levels, 2, -1), "th")
  )) %>%
  mutate(filter_day = ifelse(
    levels %in% c("max", "x80", "x40", "x5"),
    min(day_of_year),
    max(day_of_year)
  )) %>%
  filter(day_of_year == filter_day) %>%
  mutate(date = origin_date + day_of_year - 1)

# 4. Points for record high/low (with date column)
legend_record_points <- tibble(
  day_of_year = c(177, 189),
  temp = c(
    legend.line.df$temp[legend.line.df$day_of_year == 177],
    legend.line.df$temp[legend.line.df$day_of_year == 189]
  ),
  record_status = c(
    if (var == "TMAX") "record_low_tmax" else "record_low_tmin",
    if (var == "TMAX") "record_high_tmax" else "record_high_tmin"
  ),
  label = c(
    if (var == "TMAX") "all-time record lowest daily high set this year" else "all-time record lowest daily low set this year",
    if (var == "TMAX") "all-time record daily high set this year" else "all-time record daily low set this year"
  )
) %>%
  mutate(date = origin_date + day_of_year - 1)

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
    # --- Draw legend block ---
    geom_ribbon(data = legend_df, aes(x = date, ymin = min, ymax = max), fill = "#bdc9e1", inherit.aes = FALSE) +
    geom_ribbon(data = legend_df, aes(x = date, ymin = x5, ymax = x95), fill = "#74a9cf", inherit.aes = FALSE) +
    geom_ribbon(data = legend_df, aes(x = date, ymin = x20, ymax = x80), fill = "#2b8cbe", inherit.aes = FALSE) +
    geom_ribbon(data = legend_df, aes(x = date, ymin = x40, ymax = x60), fill = "#045a8d", inherit.aes = FALSE) +
    geom_line(data = legend.line.df, aes(x = date, y = temp), color = "black", lwd = 1, inherit.aes = FALSE) +
    geom_point(
      data = legend_record_points,
      aes(x = date, y = temp, shape = record_status, fill = record_status),
      color = "black", size = 3, inherit.aes = FALSE
    ) +
    # For record high (red, at upper right)
    geom_text(
     data = filter(legend_record_points, record_status == "record_high_tmax"),
     aes(x = date + 5, y = temp + 5, label = label),
     hjust = 0, size = 4, fontface = "plain"
   ) +
   # For record low (blue, at lower right)
   geom_text(
     data = filter(legend_record_points, record_status == "record_low_tmax"),
     aes(x = date + 5, y = temp - 5, label = label),
     hjust = 0, size = 4, fontface = "plain"
   )
  # Old code, trying to place both record points at one time
    #geom_text(
    #  data = legend_record_points,
    #  aes(x = date, y = temp, label = label),
    #  hjust = 0, vjust = c(-1, 2), size = 4, fontface = "plain", inherit.aes = FALSE
    #) +
    ggrepel::geom_text_repel(
      data = filter(legend.labels, filter_day == max(filter_day)),
      aes(x = date, y = value, label = label),
      min.segment.length = 0, size = 4, direction = "y", hjust = 0, nudge_x = 5, inherit.aes = FALSE) +
    ggrepel::geom_text_repel(
      data = filter(legend.labels, filter_day == min(filter_day)),
      aes(x = date, y = value, label = label),
      min.segment.length = 0, size = 4, direction = "y", hjust = 1, nudge_x = -5, inherit.aes = FALSE) +
    #annotate("text", x = min(legend_df$date), y = max(legend_df$max), label = "Legend", hjust = 0, vjust = 1, fontface = "bold", size = 5) +
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
