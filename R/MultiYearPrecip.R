# Multi-Year Precipitation Analysis for McCormick Observatory
# Last Updated: 2025-05-06 (Revised for global data handling)

library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)  # For walk()

# --- Load Data Globally ---
ghcn <- read_csv("data/GHCN_USC00441593.csv",
                show_col_types = FALSE) |>
  arrange(date) |>
  group_by(year) |>
  mutate(
    PRCP_clean = coalesce(PRCP, 0),
    cum_precip = cumsum(PRCP_clean)
  ) |>
  ungroup()

# --- Define Complete Years ---
complete_years <- ghcn |> 
  group_by(year) |> 
  filter(n() >= 365) |> 
  pull(year) |> 
  unique()

# --- Plot Generation Function ---
generate_precip_plot <- function(target_year, 
                                 output_dir = "graphs/") {
  # --- Data Validation ---
  if(!target_year %in% complete_years) {
    warning(paste("Skipping", target_year, 
                  "- insufficient daily observations"))
    return(NULL)
  }
  
  # --- Data Filtering ---
  this.year <- ghcn |> filter(year == target_year)
  last.date <- max(this.year$date)
  
  past.years <- ghcn |>
    group_by(year) |>
    filter(n() > 364, year != target_year) |>
    ungroup()
  
  # --- Summary Stats ---
  daily.summary.stats <- past.years |>
    select(day_of_year, cum_precip) |>
    group_by(day_of_year) |>
    summarise(
      max = max(cum_precip, na.rm = TRUE),
      min = min(cum_precip, na.rm = TRUE),
      x5 = quantile(cum_precip, 0.05, na.rm = TRUE),
      x20 = quantile(cum_precip, 0.2, na.rm = TRUE),
      x40 = quantile(cum_precip, 0.4, na.rm = TRUE),
      x50 = quantile(cum_precip, 0.5, na.rm = TRUE),
      x60 = quantile(cum_precip, 0.6, na.rm = TRUE),
      x80 = quantile(cum_precip, 0.8, na.rm = TRUE),
      x95 = quantile(cum_precip, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- Plot Components ---
  month.breaks <- ghcn |>
    filter(year == 2024) |>  # Reference year for month positions
    group_by(month) |>
    slice_min(order_by = day_of_year, n = 1) |>
    ungroup() |>
    select(month, day_of_year) |>
    mutate(month_name = month.abb[1:n()])
  
  pctile.labels <- daily.summary.stats |> 
    filter(day_of_year == 365) |> 
    select(-x50) |>
    pivot_longer(cols = -day_of_year, names_to = "pctile", values_to = "precip") |> 
    mutate(pctile = ifelse(str_sub(pctile, 1, 1) == "x", 
                           paste0(str_sub(pctile, 2, -1), "th"), pctile))
  
  # --- Plot Construction ---
  cum.precip.graph <- daily.summary.stats |>
    filter(day_of_year < 366) |>
    ggplot(aes(x = day_of_year)) +
    geom_vline(xintercept = c(month.breaks$day_of_year, 365),
               linetype = "dotted", linewidth = 0.2) +
    geom_ribbon(aes(ymin = min, ymax = max), fill = "#bdc9e1") +
    geom_ribbon(aes(ymin = x5, ymax = x95), fill = "#74a9cf") +
    geom_ribbon(aes(ymin = x20, ymax = x80), fill = "#2b8cbe") +
    geom_ribbon(aes(ymin = x40, ymax = x60), fill = "#045a8d") +
    geom_hline(yintercept = seq(0, 50, 5), color = "white", linewidth = 0.1) +
    geom_line(data = this.year, aes(y = cum_precip), 
              color = "#d1495b", linewidth = 1.5) +
    ggrepel::geom_label_repel(
      data = filter(this.year, day_of_year == max(day_of_year)),
      aes(y = cum_precip, label = round(cum_precip, 1)),
      point.padding = 5, direction = "y", alpha = 0.5
    ) +
    geom_segment(data = pctile.labels, 
                 aes(x = 365, xend = 367, y = precip, yend = precip)) +
    geom_text(data = pctile.labels, aes(367.5, precip, label = pctile),
              hjust = 0, family = "serif", size = 3) +
    scale_y_continuous(
      breaks = seq(-10, 100, 10),
      labels = scales::unit_format(suffix = "in."),
      expand = expansion(0.01),
      name = NULL
    ) +
    scale_x_continuous(
      expand = expansion(c(0, 0.04)),
      breaks = month.breaks$day_of_year + 15,
      labels = month.breaks$month_name,
      name = NULL
    ) +
    labs(
      title = paste("Annual Precipitation at McCormick Observatory -", target_year),
      subtitle = paste("The line shows precipitation for", target_year, 
                       "compared to 1893-present historical ranges"),
      caption = paste(
        "Records begin January 1, 1893. Graph updated:", format(Sys.Date(), "%B %d, %Y.\n"),
        "Includes February 29th accumulations during leap years."
      )
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "linen", colour = "linen"),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 16),
      axis.ticks = element_blank()
    )
  
  # --- Save Output ---
  if(!dir.exists(output_dir)) dir.create(output_dir)
  output_file <- paste0(output_dir, "Precip_", target_year, ".png")
  ggsave(output_file, plot = cum.precip.graph, width = 8, height = 4)
  
  return(cum.precip.graph)
}

# === Usage Examples ===
# Generate for specific year
generate_precip_plot(2024)

# Generate for current year
c(2025) |> walk(generate_precip_plot)

# Generate for all complete years
walk(complete_years, generate_precip_plot)
