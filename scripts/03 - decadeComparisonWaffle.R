# ---- Install packages ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, sysfonts, showtext, googlesheets4, janitor, waffle, ggthemes,
               gridExtra)

# ---- 01 - All types by decade ----
# Data
waffleBars <- historyData |> 
  count(decade, type, series) |> 
  mutate(category = case_when(
    type == "Original" ~ "Original",
    TRUE ~ "Rest"
  ))


#Plot 1 - All types
Chart02_waffleBar <- ggplot(
  data = waffleBars, 
  aes(fill = type, values = n)
) +
  geom_waffle(
    color = "#000000", 
    size = .25, 
    n_rows = 5, 
    flip = TRUE
  ) +
  facet_wrap(
    ~decade, 
    nrow = 1, 
    strip.position = "bottom"
  ) +
  scale_x_discrete() + 
  scale_y_continuous(
    limits = c(0, 30),
    labels = function(x) x * 5, # make this multiplier the same as n_rows
    expand = c(0,0)
    ) +
  scale_fill_manual(values = c("#8c9e98", "#E84855", "#23CE6B", "#065143", "#FF6F59")) +
  labs(
    x = "Year", y = "Count",
    title = "Disney movie releases",
    subtitle = "By type of movie",
    caption = "Sources: Wikipedia, Box Office Mojo"
  ) +
  theme_economist() +
  theme(
    panel.grid = element_blank(),
    text = element_text(colour = "#000000", size = 20, family = font),
    plot.title = element_text(size = 55, margin = margin(0, 0, 5, 0), lineheight = 1.2, family = title_font),
    plot.subtitle = element_text(size = 30, margin = margin(25,0,50,0), hjust = 0),
    plot.background = element_rect(fill = "#EFEFEF"),
    panel.background = element_rect(fill = "#EFEFEF"),
    axis.text.y = element_text(hjust = 1, size = 25),
    axis.title = element_blank(),
    legend.text = element_text(size = 20),
    plot.caption = element_text(size = 22, hjust = 1, margin = margin(20, 0, 0, 0)),
    strip.text = element_text(
      size = 25,
      margin = margin(5, 0, 4, 0)),
    legend.position = "bottom"
  )
  
  # coord_equal() +


Chart02_waffleBar

# ggsave("chartsFinal/02_decadeWaffle/02decadeWaffle.png", plot = Chart02_waffleBar, width = 1000, height = 1000, units = "px", dpi = 72)

width_px <- 1000
height_px <- 1000
dpi <- 72


ggsave("finalBatch/02_decadeWaffle/02decadeWaffle.svg_ALT", plot = Chart02_waffleBar, device = svglite, width = width_px / dpi, height = height_px / dpi)

# By studio

waffleBars2 <- historyData |> 
  group_by(decade) |> 
  count(type, studio) |> 
  mutate(category = case_when(
    type == "Original" ~ "Original",
    TRUE ~ "Rest"
  )) |> 
  filter(
    category == "Rest",
    studio != "Walt Disney Pictures"
  )
  
  
  
  count(decade, type, series, studio) |> 
  mutate(category = case_when(
    type == "Original" ~ "Original",
    TRUE ~ "Rest"
  ))



# ---- 02 - Originals vs not ----
# Data
historyData |> 
  count(decade, original) |> 
  mutate(
    original = factor(original, levels = c("N", "Y"))) -> originalsWaffle

# Plot
barOriginals <- ggplot(
  data = originalsWaffle, 
  aes(fill = original, values = n)
) +
  geom_waffle(
    color = "white", 
    size = .25, 
    n_rows = 5, 
    flip = TRUE
  ) +
  facet_wrap(
    ~decade, 
    nrow = 1, 
    strip.position = "bottom"
  ) +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 5, # make this multiplier the same as n_rows
    expand = c(0,0)
  ) +
  ggthemes::scale_fill_tableau(
    name = NULL,
    limits = rev(levels(originalsWaffle$original)) # Reverse the order of categories
  ) +
  coord_equal() +
  labs(
    x = "Year", y = "Count",
    title = "Releases by decade",
    subtitle = "Color by original"
  ) +
  theme_economist(

  ) +
  theme(
    panel.grid = element_blank(), 
    axis.ticks.y = element_line()
  ) +
  guides(
    fill = guide_legend(reverse = TRUE)
  )

barOriginals

# ---- 03 - Based on other media vs not ----
# Data
historyData |> 
  count(decade, based_on_other_media) -> otherWaffle

# Plot
barOtherMedia <- ggplot(
  data = otherWaffle, 
  aes(fill = based_on_other_media, values = n)
) +
  geom_waffle(
    color = "white", 
    size = .25, 
    n_rows = 5, 
    flip = TRUE
  ) +
  facet_wrap(
    ~decade, 
    nrow = 1, 
    strip.position = "bottom"
  ) +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 5, # make this multiplier the same as n_rows
    expand = c(0,0)
  ) +
  ggthemes::scale_fill_tableau(
    name = NULL,
    limits = rev(levels(originalsWaffle$original)) # Reverse the order of categories
  ) +
  coord_equal() +
  labs(
    x = "Year", y = "Count",
    title = "Releases by decade",
    subtitle = "Color by original"
  ) +
  theme_clean(
    base_family = font
  ) +
  theme(
    panel.grid = element_blank(), 
    axis.ticks.y = element_line()
  ) +
  guides(
    fill = guide_legend(reverse = TRUE)
  )

barOtherMedia
