legend.df <- daily.summary.stats %>%
  filter(day_of_year %in% 165:201,
         name == "TMAX") %>%
  mutate(max = max - 60,
         min = min - 60,
         x5 = x5 - 60,
         x20 = x20 - 60,
         x40 = x40 - 60,
         x60 = x60 - 60,
         x80 = x80 - 60,
         x95 = x95 - 60)

legend.line.df <- tibble(
  day_of_year = 165:201,
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
  filter(!is.na(temp))

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
  filter(day_of_year == filter_day)

##  Add legend
max.graph2 <- max.graph +
  # ribbon between the lowest and 5th percentiles
  geom_ribbon(data = legend.df,
              aes(ymin = min, ymax = max),
              fill = "#bdc9e1") +
  # ribbon between the 5th and 20th percentiles
  geom_ribbon(data = legend.df,
              aes(ymin = x5, ymax = x95),
              fill = "#74a9cf") +
  # ribbon between the 20th and 40th percentiles
  geom_ribbon(data = legend.df,
              aes(ymin = x20, ymax = x80),
              fill = "#2b8cbe") +
  # ribbon between the 40th and 60th percentiles
  geom_ribbon(data = legend.df,
              aes(ymin = x40, ymax = x60),
              fill = "#045a8d") +
  geom_line(data = legend.line.df, aes(y = temp), lwd = 0.9) +
  geom_point(aes(x = 177, y = legend.line.df$temp[legend.line.df$day_of_year == 177]),
             color = "blue") +
  geom_point(aes(x = 189, y = legend.line.df$temp[legend.line.df$day_of_year == 189]),
             color = "red") +
  geom_text(aes(x = 180, y = legend.line.df$temp[legend.line.df$day_of_year == 177] - 2,
                label = "record low this year"),
            hjust = 0, size = 3) +
  geom_text(aes(x = 192, y = legend.line.df$temp[legend.line.df$day_of_year == 189] + 2,
                label = "record high this year"),
            hjust = 0, size = 3) +
  ggrepel::geom_text_repel(data = filter(legend.labels,
                                         filter_day == max(filter_day)),
                           aes(y = value, label = label),
                           min.segment.length = 0, size = 3,
                           direction = "y", hjust = 0, nudge_x = 5) +
  ggrepel::geom_text_repel(data = filter(legend.labels,
                                         filter_day == min(filter_day)),
                           aes(y = value, label = label),
                           min.segment.length = 0, size = 3,
                           direction = "y", hjust = 1, nudge_x = -5)

