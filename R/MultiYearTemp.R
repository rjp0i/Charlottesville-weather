library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggrepel)

# Load data once globally
ghcn <- read_csv("data/GHCN_USC00441593.csv", show_col_types = FALSE)

generate_temp_plot <- function(target_year, output_dir = "graphs/") {
  # Filter for the selected year
  this.year <- ghcn |> filter(year == target_year)
  if(nrow(this.year) == 0) {
    warning(paste("No data for year", target_year))
    return(NULL)
  }
  
  last.date <- max(this.year$date)
  is.leap.year <- leap_year(target_year)
  leap.year.caption <- if (is.leap.year) {
    "Records for Leap Day are shown."
  } else {
    "Records for February 29th are not shown."
  }
  
  # Prepare daily summary stats for all other years
  daily.summary.stats <- ghcn |>
    filter(year != target_year) |>
    select(month, day, PRCP, TMAX, TMIN) |>
    pivot_longer(cols = -c(month, day)) |>
    group_by(month, day, name) |>
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
    ) |>
    mutate(
      date = as.Date(paste(target_year, month, day, sep = "-")),
      day_of_year = yday(date)
    )
  
  # Remove Feb 29 if not a leap year
  if(!is.leap.year){
    daily.summary.stats <- daily.summary.stats |> filter(!(month == "02" & day == "29"))
  }
  
  # Month breaks for the x-axis
  month.breaks <- ghcn |>
    filter(year == 2019) |>  # Use any full year for month positions
    group_by(month) |>
    slice_min(order_by = day_of_year, n = 1) |>
    ungroup() |>
    select(month, day_of_year) |>
    mutate(month_name = month.abb[1:n()])
  
  # Identify record status for this year
  record.status.this.year <- this.year |>
    select(month, day, PRCP, TMAX, TMIN) |>
    pivot_longer(cols = -c(month, day), values_to = "this_year") |>
    inner_join(daily.summary.stats |> select(-starts_with("x")), by = c("month", "day", "name")) |>
    mutate(record_status = case_when(
      this_year > max ~ "max",
      this_year < min ~ "min",
      TRUE ~ "none"
    )) |>
    filter(record_status != "none")
  
  # Build the plot for TMAX
  max.graph <- daily.summary.stats |>
    filter(name == "TMAX") |>
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = min, ymax = max), fill = "#bdc9e1") +
    geom_ribbon(aes(ymin = x5, ymax = x95), fill = "#74a9cf") +
    geom_ribbon(aes(ymin = x20, ymax = x80), fill = "#2b8cbe") +
    geom_ribbon(aes(ymin = x40, ymax = x60), fill = "#045a8d") +
    geom_hline(yintercept = seq(-10, 100, 10), color = "white", lwd = 0.1) +
    geom_line(data = this.year, aes(y = TMAX), lwd = 1) +
    geom_point(
      data = filter(record.status.this.year, name == "TMAX", record_status == "max"),
      aes(y = this_year), color = "red"
    ) +
    geom_point(
      data = filter(record.status.this.year, name == "TMAX", record_status == "min"),
      aes(y = this_year), color = "blue"
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
      breaks = unique(daily.summary.stats$date[daily.summary.stats$day == "15"]),
      labels = scales::label_date(format = "%b"),
      minor_breaks = unique(daily.summary.stats$date[daily.summary.stats$day == "01"]),
      name = NULL
    ) +
    labs(
      title = "Daily High Temperature at Charlottesville's McCormick Observatory",
      subtitle = paste(
        "The line shows daily highs for", target_year, ".",
        "The ribbons cover the historical range. The last date shown is",
        format(last.date, "%b %d, %Y.")
      ),
      caption = paste(
        "Records begin on January 1, 1893.",
        "This graph was last updated on", format(Sys.Date(), "%B %d, %Y."),
        leap.year.caption
      )
    ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_line(linetype = "dotted", linewidth = 0.2, colour = "gray"),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "linen", colour = "linen"),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 16),
      axis.ticks = element_blank()
    )
  
  # Save plot
  if(!dir.exists(output_dir)) dir.create(output_dir)
  output_file <- paste0(output_dir, "DailyHighTemp_USC00441593_", target_year, ".png")
  ggsave(output_file, plot = max.graph, width = 8, height = 4)
  
  return(max.graph)
}

# Example usage:
generate_temp_plot(2023)  # Plot for 2023
generate_temp_plot(1997)  # Plot for 1997
