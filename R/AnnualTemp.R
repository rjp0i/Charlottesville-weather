library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Set your missing day threshold (e.g., max 5 missing days)
max_missing_days <- 20

# Load and process data
ghcn <- read_csv("data/GHCN_USC00441593.csv") |>
  mutate(
    date = ymd(date),
    year = year(date),
    daily_avg = (TMAX + TMIN)/2
  )

# Calculate annual statistics
annual_stats <- ghcn |>
  group_by(year) |>
  summarise(
    missing_days = (365 + leap_year(first(year))) - sum(!is.na(daily_avg)),
    annual_avg = mean(daily_avg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(missing_days <= max_missing_days)

# Create and save plot
temp_plot <- ggplot(annual_stats, aes(x = year, y = annual_avg, color = missing_days)) +
  geom_point(size = 3) +
  geom_line(alpha = 0.5) +
  scale_color_viridis_c(
    name = "Missing Days",
    direction = -1,
    limits = c(0, max_missing_days)
  ) +
  labs(
    title = "Annual Average Temperature Trends (1893-Present)",
    subtitle = paste("Years with >", max_missing_days, "missing days excluded"),
    x = "Year",
    y = "Temperature (°F)",
    caption = "McCormick Observatory Weather Station Data\nNOAA GHCN Daily Archive"
  ) +
  theme_minimal() +
  theme(plot.title.position = "plot")

# Save plot
ggsave("graphs/AnnualTemperatureTrends.png", temp_plot, width = 10, height = 6)

# To display plot in RStudio
print(temp_plot)
