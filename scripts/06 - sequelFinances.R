# ---- Load packages ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, sysfonts, showtext, googlesheets4, janitor, ggthemes, plotly)

# ---- Marvel exploration ----
marvelList <- historyData |> 
  filter(studio == "Marvel") |> 
  group_by(studio, series) |> 
  summarise(
    count = n()
  ) |> 
  pull(series)

singleSeries <- historyData |> 
  filter(studio == "Marvel") |> 
  group_by(studio, series) |> 
  summarise(
    count = n()
  ) |> 
  filter(count == 1) |> 
  filter(series != "X-men") |> 
  pull(series)

# Call out originals
marvelSlope <- historyData |>
  select(series, title, year, type, original, release_date, 6:9) |> 
  filter(series %in% marvelList) |> 
  mutate(
    hasSequel = ifelse(series %in% singleSeries, "Solo", "Franchise")
  )

# Plot - Underperforming originals
ggplot(marvelSlope, aes(x = reorder(title, release_date), y = (profit / 1000000000), fill = hasSequel)) +
  geom_col(position = "dodge", aes(alpha = hasSequel)) +
  theme_wsj() +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_alpha_manual(
    values = c("Solo" = 1, "Franchise" = 0.6)  # Adjust alpha levels for Solo and Franchise
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Solo" = "#F57A97",
               "Franchise" = "#bfbfbf")
  ) +
  labs(
    title = "Profit",
  ) +
  theme(
    text = element_text(colour = "#FFFAFA", size = 14, family = font),
    plot.title = element_text(family = title_font, size = 35, margin = margin(0, 0, 5, -250)),
    plot.subtitle = element_text(size = 15, margin = margin(10,0,30,-250), hjust = 0),
    legend.position = "NONE",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(hjust = 1, size = 12),
    plot.background = element_rect(fill = "#335248"),
    panel.background = element_rect(fill = "#335248"),
    panel.grid.major = element_line(colour = "#dbe3e1", linewidth = rel(0.7)),
    axis.line.x = element_line(colour = "#dbe3e1", linewidth = rel(2.8)),
    axis.ticks = element_line(colour = "#dbe3e1", linewidth = rel(1.5))
    )













 ggplot(marvelSlope, aes(x = reorder(title, year), y = profit / 1000000000, fill = series)) + 
  geom_col(position = "dodge") +
  theme(legend.position = "none") +
  theme_wsj() +
  theme(
    legend.position = "false"
  ) + 
  facet_wrap(~ series, scales = "free") +
  labs(
    title = "Proportion"
  )






# ---- Data preparation ----
# Create series list with more than one entry
seriesList <- historyData |>
  filter(studio != "Walt Disney Pictures") |>
  group_by(studio, series) |>
  summarise(
    count = n()
  ) |>
  filter(count > 1) |>
  pull(series)

# Filter data based on series list
franchiseData <- historyData |>
  filter(series %in% seriesList) |>
  select(1:11, year) |>
  mutate(
    profit = (profit / 1000000000)
  )

# Prepare data for slope chart
slopeData <- franchiseData |>
  arrange(series) |>
  select(series, title, year, 6:9) |>
  group_by(series) |>
  mutate(
    entry = row_number()
  ) |> 
  mutate(
    first_movie_profit = first(profit),
    success_comparison = case_when(
      profit < first_movie_profit ~ "Less successful",
      profit > first_movie_profit ~ "More successful",
      TRUE ~ "Same success"
    )
  ) |> 
  ungroup() |> 
  select(-first_movie_profit)




# All marvel
ggplot(filter(franchiseData, studio == "Marvel"), aes(x = year, y = profit, colour = studio)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ series, nrow = 4) +
  labs(
    title = "Profits"
  ) +
  theme_wsj()

# Slope chart
ggplot(data = filter(slopeData, series != "Star Wars"), aes(x = entry, y = profit, group = series)) +
         geom_line(aes(color = success_comparison, alpha = 1), size = 2) +
         geom_point(aes(color = success_comparison, alpha = 1), size = 4) +
  scale_x_continuous(position = "top") +
  theme_minimal() +
  theme(
    panel.border = element_blank()
  ) +
  facet_wrap(~ series)
# 
# # ---- Other charts ----
# # Profit
# ggplot(filter(franchiseData, studio == "Marvel"), aes(x = year, y = profit, colour = studio)) + 
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ series, nrow = 4) +
#   labs(
#     title = "Profits"
#   ) +
#   theme_wsj()
# 
# # Years to study
# years <- franchiseData |>
#   filter(str_detect(title, "Guardians")) |> 
#   pull(year)
# 
# unique(years)
# 
# 
# # ---- Other series metrics ----
# # Rating
# ggplot(franchiseData, aes(x = year, y = rating, colour = studio)) + 
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ series, nrow = 4) +
#   labs(
#     title = "Rating"
#   ) +
#   theme_wsj()
# 
# # Budget
# ggplot(franchiseData, aes(x = year, y = budget, colour = studio)) + 
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ series, nrow = 4) +
#   labs(
#     title = "Budget"
#   ) +
#   theme_wsj()
# 
# 
# 
# nonSequelAvg <- marvelSlope |> 
#   group_by(series) |> 
#   mutate(
#     entries = n()
#   ) |> 
#   filter(entries == 1) |> 
#   ungroup() |> 
#   summarise(
#     nonAvg = mean(profit)
#   ) |> 
#   pull(nonAvg)
# 
# allMovieAvg <- marvelSlope |> 
#   group_by(series) |> 
#   mutate(
#     entry = row_number()
#   ) |> 
#   filter(entry == 1) |> 
#   ungroup() |> 
#   summarise(
#     avg = mean(profit)
#   ) |> 
#   pull(avg)
# 
# 
# 
# # Summarize the total profit for each series
# series_profit <- marvelSlope %>%
#   group_by(series) %>%
#   summarize(total_profit = sum(profit))
# 
# # Plot with ggplot2
# ggplot(marvelSlope, aes(x = reorder(title, year), y = returns_prop, fill = original)) + 
#   geom_col(position = "dodge") +
#   coord_flip() +
#   theme(legend.position = "none") +
#   theme_wsj() +
#   theme(
#     legend.position = "false"
#   ) + 
#   facet_wrap(~ series)
# 
# 
# bigFilms <- marvelSlope |> 
#   group_by(series) |> 
#   summarize(sum_profit = sum(profit),
#             count = n()
#             ) |> 
#   filter(series == "Avengers") |> 
#   summarise(
#     total_profit = (sum(sum_profit) / 1000000)
#   ) |> 
#   pull()
# 
# smallerList <- marvelSlope |> 
#   group_by(series) |> 
#   summarize(sum_profit = sum(profit)) |> 
#   filter(sum_profit <= 1126057284) |> 
#   pull(series)
# 
# smallFilms <- marvelSlope |> 
#   group_by(series) |> 
#   summarize(sum_profit = sum(profit),
#             count = n()
#   ) |> 
#   filter(series %in% smallerList) |> 
#   summarise(
#     total_profit = (sum(sum_profit) / 1000000)
#   ) |> 
#   pull()
# 
# smallFilms - bigFilms
