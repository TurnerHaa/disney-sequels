# ---- Load packages ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, sysfonts, showtext, googlesheets4, janitor, waffle, ggthemes, gridExtra, cowplot)

# ---- Data wrangling ----

# All studios
historyProportion <- historyData |> 
  group_by(original) |> 
  summarize(
    n = n(),
  ) |> 
  mutate(
    proportion = n / sum(n),
    tiles = round(proportion * 100)
    )

# Disney
disneyProportion <- disneyData |> 
  group_by(original) |> 
  summarize(
    n = n(),
  ) |> 
  mutate(
    proportion = n / sum(n),
    tiles = round(proportion * 100)
  )

# Marvel
marvelProportion <- marvelData |> 
  group_by(original) |> 
  summarize(
    n = n(),
  ) |> 
  mutate(
    proportion = n / sum(n),
    tiles = round(proportion * 100)
  )

# Lucas
lucasProportion <- lucasData |> 
  group_by(original) |> 
  summarize(
    n = n(),
  ) |> 
  mutate(
    proportion = n / sum(n),
    tiles = round(proportion * 100)
  )
# Pixar
pixarProportion <- pixarData |> 
  group_by(original) |> 
  summarize(
    n = n(),
  ) |> 
  mutate(
    proportion = n / sum(n),
    tiles = round(proportion * 100)
  )

# ---- Visualization ----
# ---- Waffle chart for marvelProportion ----
marvelWaffle <- ggplot(marvelProportion, aes(fill = original, values = tiles)) +
  geom_waffle(n_rows = 10, size = 1, colour = "#000000") +
  scale_fill_manual(values = c("#FF6F59", "#8c9e98")) +
  labs(
    title = "Marvel"
  ) +
  theme_economist() +
  coord_equal() +
  theme(
    text = element_text(colour = "#000000", size = 20, family = font),
    plot.title = element_text(family = title_font, size = 32, margin = margin(0, 0, 5, 0)),    plot.subtitle = element_text(size = 15, margin = margin(10,0,30,-20), hjust = 0),
    legend.position = "NONE",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "#EFEFEF"),
    panel.grid.major = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# ---- Waffle chart for lucasProportion ----
lucasWaffle <- ggplot(lucasProportion, aes(fill = original, values = tiles)) +
  geom_waffle(n_rows = 10, size = 1, colour = "#000000") +
  scale_fill_manual(values = c("#FF6F59", "#8c9e98")) +
  labs(
    title = "Lucas"
  ) +
  theme_economist() +
  coord_equal() +
  theme(
    text = element_text(colour = "#000000", size = 20, family = font),
    plot.title = element_text(family = title_font, size = 32, margin = margin(0, 0, 5, 0)),    plot.subtitle = element_text(size = 15, margin = margin(10,0,30,-20), hjust = 0),
    legend.position = "NONE",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "#EFEFEF"),
    panel.grid.major = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# ---- Waffle chart for pixarProportion ----
pixarWaffle <- ggplot(pixarProportion, aes(fill = original, values = tiles)) +
  geom_waffle(n_rows = 10, size = 1, colour = "#000000") +
  scale_fill_manual(values = c("#FF6F59", "#8c9e98")) +
  labs(
    title = "Pixar"
  ) +
  theme_economist() +
  coord_equal() +
  theme(
    text = element_text(colour = "#000000", size = 20, family = font),
    plot.title = element_text(family = title_font, size = 32, margin = margin(0, 0, 5, 0)),    plot.subtitle = element_text(size = 15, margin = margin(10,0,30,-20), hjust = 0),
    legend.position = "NONE",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "#EFEFEF"),
    panel.grid.major = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# ---- Waffle chart for disneyProportion ----
disneyWaffle <- ggplot(disneyProportion, aes(fill = original, values = tiles, alpha = original)) +
  geom_waffle(n_rows = 10, size = 1, colour = "#000000") +
  scale_fill_manual(values = c("#FF6F59", "#8c9e98")) +
  scale_alpha_manual(values = c(1, 0.2)) +
  labs(
    title = "Disney"
  ) +
  theme_economist() +
  coord_equal() +
  theme(
    text = element_text(colour = "#000000", size = 20, family = font),
    plot.title = element_text(family = title_font, size = 32, margin = margin(0, 0, 5, 0)),
    plot.subtitle = element_text(size = 15, margin = margin(10,0,30,-20), hjust = 0),
    legend.position = "NONE",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "#EFEFEF"),
    panel.grid.major = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

disneyWaffle

# ---- Plotting the Waffle charts ----
g <- grid.arrange(marvelWaffle, lucasWaffle, pixarWaffle, disneyWaffle, nrow = 2)

Chart03_studioWaffles <- cowplot::ggdraw(g) +
  theme(plot.background = element_rect(fill = "#EFEFEF"))

Chart03_studioWaffles


Chart03_studioWaffles <-  ggdraw(add_sub(Chart03_studioWaffles, "Sources: Wikipedia, Box Office Mojo", hjust = 0, colour = "#000000", fontfamily = font))

Chart03_studioWaffles

width_px <- 1000
height_px <- 1000
dpi <- 72

ggsave("finalBatch/03_studioWaffles/03studioWaffles.svg_ALT", plot = Chart03_studioWaffles, device = svglite, width = width_px / dpi, height = height_px / dpi)
 
  # historyData |> 
#   group_by(studio, original) |> 
# summarize(
#   n = n(),
# ) |> 
#   mutate(
#     proportion = n / sum(n),
#     tiles = round(proportion * 100)
#   )