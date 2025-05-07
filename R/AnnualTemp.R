library(dplyr)
library(lubridate)
library(ggplot2)

# Set your missing day threshold (e.g., max 5% missing days = ~18 days)
max_missing_days <- 5

ghcn <- read_csv("data/GHCN_USC00441593.csv") |>
  mutate(
    date = ymd(date),
    year = year(date),
    daily_avg = (TMAX + TMIN)/2  # Conventional daily average
  )
#ghcn |>
#  mutate(
#    completeness = cut(missing_days,
#                       breaks = c(-1, 0, 5, 18, 365),
#                       labels = c("Complete", "1-5 days", "6-18 days", "Incomplete"))
#  ) |>
#  ggplot(aes(x = year, y = annual_avg, color = completeness)) +
$  geom_point()

annual_stats <- ghcn |>
  group_by(year) |>
  summarise(
    missing_days = 365 + leap_year(year[1]) - sum(!is.na(daily_avg)),
    annual_avg = mean(daily_avg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(missing_days <= max_missing_days)  # Exclude years with too many missing days

ggplot(annual_stats, aes(x = year, y = annual_avg, color = missing_days)) +
  geom_point(size = 3) +
  geom_line(alpha = 0.5) +
  scale_color_viridis_c(
    name = "Missing Days",
    direction = -1,
    limits = c(0, max_missing_days)
  ) +
  labs(
    title = "Annual Average Temperature Trends",
    subtitle = paste("Years with >", max_missing_days, "missing days excluded"),
    x = "Year",
    y = "Annual Average Temperature (°F)",
    caption = "Daily average calculated as (TMAX + TMIN)/2\nData source: NOAA GHCN"
  ) +
  theme_minimal()
