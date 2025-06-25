if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sysfonts, showtext, googlesheets4, janitor, ggthemes, extrafont, svglite)

# ---- 01: All types ----
nonOriginalsOnly <- historyData |> 
  group_by(year, type) |> 
  summarise(count = n()) |> 
  arrange(year) |> 
  group_by(type) |> 
  mutate(
    cumulative_count = cumsum(count),
  ) |> 
  ungroup()

# Reorder `type` based on cumulative_count in the max year
points <- nonOriginalsOnly |> 
  group_by(type) |> 
  filter(year == max(year)) |> 
  summarise(cumulative_count = max(cumulative_count),
            year = year) |> 
  arrange(desc(cumulative_count)) |> 
  mutate(type = factor(type, levels = type)) # Set factor levels by descending order of max cumulative count

# Apply the ordering to the original dataset
nonOriginalsOnly <- nonOriginalsOnly |> 
  mutate(type = factor(type, levels = levels(points$type)))

zeroYears <- nonOriginalsOnly |> 
  group_by(type) |> 
  summarise(
    earliest_year = min(year),
    .groups = "drop"
  ) |> 
  mutate(
    year = earliest_year -1,
    count = 0,
    cumulative_count = 0
  ) |> 
  select(-earliest_year) |> 
  bind_rows(nonOriginalsOnly) |> 
  arrange(type, year)

nonOriginalsOnly <- zeroYears

# Line labels
lineLabels <- nonOriginalsOnly |> 
  group_by(type) |> 
  filter(year == max(year))


# Plots
# Plot code
Chart01_allMovies <- ggplot(nonOriginalsOnly, aes(x = year, y = cumulative_count, colour = type)) +
  geom_line(linewidth = 3, show.legend = FALSE) +
  geom_point(data = points, 
             aes(x = year, y = cumulative_count, fill = type), 
             size = 4.5) +
  geom_text(data = lineLabels, 
            aes(x = year + 3.2, y = cumulative_count + 1.5, label = type, colour = type),
            family = font,
            size = 9.5,
            show.legend = FALSE) + 
  scale_colour_manual(values = c("#065143", "#23CE6B", "#E84855", "#FF6F59", "purple")) +
  scale_y_continuous(
    limits = c(0, 500),
    expand = c(0,0)) +
  theme_economist() +
  labs(
    title = "Good artists copy, great artists\nsequel",
    subtitle = "Total non-original movies released by Disney",
    caption = "Sources: Wikipedia, Box Office Mojo"
  ) +
  theme(
    text = element_text(colour = "#000000", size = 20, family = font),
    plot.title = element_text(size = 55, margin = margin(0, 0, 5, 0), lineheight = 1.2, family = title_font),
    plot.subtitle = element_text(size = 30, margin = margin(25,0,50,0), hjust = 0),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(hjust = 1, size = 25),
    axis.text.x = element_text(size = 25),
    plot.background = element_rect(fill = "#EFEFEF"),
    panel.grid.major = element_line(colour = "#000000", linewidth = rel(0.7)),
    axis.line.x = element_line(colour = "#000000", linewidth = rel(2.8)),
    axis.ticks = element_blank(),
    plot.caption = element_text(size = 22, hjust = 1, margin = margin(20, 0, 0, 0))
  )

width_px <- 1000
height_px <- 1000
dpi <- 72

ggsave(Chart01_allMovies, filename = "finalBatch/01_cumulativeReleases/allMoviesChart1.svg", device = svglite::svglite,
       width = width_px / dpi, height = height_px / dpi)


# ---- 01: Original IP vs not ----
nonOriginalsOnly2 <- historyData |> 
  mutate(
    category = case_when(
      based_on_other_media == 1 ~ "Movies based on other media",
      type == "Original" & based_on_other_media == 0 ~ "Original IP",
      TRUE ~ "Movie based on movies"
    )
  ) |> 
  group_by(year, category) |> 
  summarise(count = n()) |> 
  arrange(year) |> 
  group_by(category) |> 
  mutate(
    cumulative_count = cumsum(count),
  ) |> 
  ungroup()

# Reorder `type` based on cumulative_count in the max year
points <- nonOriginalsOnly2 |> 
  group_by(category) |> 
  filter(year == max(year)) |> 
  summarise(cumulative_count = max(cumulative_count),
            year = year) |> 
  arrange(desc(cumulative_count)) |> 
  mutate(category = factor(category, levels = category)) # Set factor levels by descending order of max cumulative count

# Apply the ordering to the original dataset
nonOriginalsOnly2 <- nonOriginalsOnly2 |> 
  mutate(category = factor(category, levels = levels(points$category)))

zeroYears <- nonOriginalsOnly2 |> 
  group_by(category) |> 
  summarise(
    earliest_year = min(year),
    .groups = "drop"
  ) |> 
  mutate(
    year = earliest_year -1,
    count = 0,
    cumulative_count = 0
  ) |> 
  select(-earliest_year) |> 
  bind_rows(nonOriginalsOnly2) |> 
  arrange(category, year)

nonOriginalsOnly2 <- zeroYears

# Line labels
lineLabels <- nonOriginalsOnly2 |> 
  group_by(category) |> 
  filter(year == max(year))


# Plots
# Plot code
Chart01_allMovies2 <- ggplot(nonOriginalsOnly2, aes(x = year, y = cumulative_count, colour = category)) +
  geom_line(linewidth = 3, show.legend = FALSE) +
  geom_point(data = points, 
             aes(x = year, y = cumulative_count, fill = category), 
             size = 4.5) +
  geom_text(data = lineLabels, 
            aes(x = year + 3.2, y = cumulative_count + 1.5, label = category, colour = category),
            family = font,
            size = 9.5,
            show.legend = FALSE) + 
  scale_colour_manual(values = c("#065143", "#23CE6B", "#E84855", "#FF6F59", "purple")) +
  scale_y_continuous(
    limits = c(0, 300),
    expand = c(0,0)) +
  theme_economist() +
  labs(
    title = "Good artists copy, great artists\nsequel",
    subtitle = "Total non-original movies released by Disney",
    caption = "Sources: Wikipedia, Box Office Mojo"
  ) +
  theme(
    text = element_text(colour = "#000000", size = 20, family = font),
    plot.title = element_text(size = 55, margin = margin(0, 0, 5, 0), lineheight = 1.2, family = title_font),
    plot.subtitle = element_text(size = 30, margin = margin(25,0,50,0), hjust = 0),
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(hjust = 1, size = 25),
    axis.text.x = element_text(size = 25),
    plot.background = element_rect(fill = "#EFEFEF"),
    panel.grid.major = element_line(colour = "#000000", linewidth = rel(0.7)),
    axis.line.x = element_line(colour = "#000000", linewidth = rel(2.8)),
    axis.ticks = element_blank(),
    plot.caption = element_text(size = 22, hjust = 1, margin = margin(20, 0, 0, 0))
  )

Chart01_allMovies2

width_px <- 1000
height_px <- 1000
dpi <- 72

ggsave(Chart01_allMovies2, filename = "finalBatch/01_cumulativeReleases/allMoviesChart2b.svg", device = svglite::svglite,
       width = width_px / dpi, height = height_px / dpi)
