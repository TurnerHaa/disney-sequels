if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sysfonts, showtext, googlesheets4, janitor, ggthemes, scales, svglite, Cairo)

# font_import(prompt = FALSE)
loadfonts(device = "win")



# ---- Proportion: Sequels ----
# Area
areaSum <- historyData |> 
  group_by(decade, original) |> 
  summarize(
    count = n()
  ) |> 
  mutate(
    decTot = sum(count),
    prop = round(((count / decTot) * 100), digits = 0),
    txt = paste0(prop,"%")) |> 
  select(-decTot)

# Plot
Chart04_proportionChange <- ggplot(areaSum, aes(x = decade, y = count, fill = original)) +
  geom_bar(position="fill", stat="identity") +
    scale_y_reverse(labels = label_percent(accuracy = 1),
                  expand = c(0,0)) +
  scale_fill_manual(values = c("#F4A259", "#8c9e98")) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1945, 2025),
                     breaks = seq(1945, 2025, by = 10)) +
  geom_text(aes(label = txt), color = "#000000", size = 9,
            position = position_fill(vjust = 0.5),
            show.legend = FALSE) +
  theme_economist() +
  labs(
    title = "Disney's movie releases",
    subtitle = "Original movies and movie based on movies, %\n ",
    caption = "Sources: Wikipedia, Box Office Mojo"
  ) +
  theme(
    aspect.ratio = 1,
    text = element_text(colour = "#000000", size = 20, family = font),
    plot.title = element_text(size = 55, margin = margin(0, 0, 5, 0), lineheight = 1.2, family = title_font),
    plot.subtitle = element_text(size = 30, margin = margin(25,0,50,0), hjust = 0),
    plot.background = element_rect(fill = "#EFEFEF"),
    panel.background = element_rect(fill = "#EFEFEF"),
    legend.position = "NONE",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 22),
    plot.caption = element_text(size = 22, hjust = 1, margin = margin(20, 0, 0, 0))
  )

Chart04_proportionChange

###########
# Save the plot as SVG
width_px <- 1000
height_px <- 1000
dpi <- 72

ggsave("finalBatch/045_proportionChanges/04proportionChange_ALT.svg", plot = Chart04_proportionChange, device = svglite, width = width_px / dpi, height = height_px / dpi)



# ---- Proportion: All ----
# Data
areaSum2 <- historyData |> 
  mutate(
    type2 = case_when(
      type == "Original" & based_on_other_media == 0 ~ "Original",
      type == "Original" & based_on_other_media == 1 ~ "Based on other media",
      type != "Original" & based_on_other_media == 1 ~ "Non-original",
      TRUE ~ "Non-original"
    )
  ) |> 
  group_by(decade, type2) |> 
  summarise(
    count = n()
  ) |> 
  filter(decade != 1930) |> 
  mutate(type2 = factor(type2, levels = c("Based on other media", "Original", "Non-original")),
         decTot = sum(count),
         prop = round(((count / decTot) * 100), digits = 0),
         txt = paste0(prop,"%")) |> 
  select(-decTot)

# Plot
Chart05_proportionChanges <- ggplot(areaSum2, aes(x = decade, y = count, fill = type2)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = c("#23CE6B", "#8c9e98", "#FF6F59")) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1945, 2025),
                     breaks = seq(1945, 2025, by = 10)) +
  geom_text(aes(label = txt), color = "#36454F", size = 9, 
            position = position_fill(vjust = 0.5), 
            show.legend = FALSE) +
  theme_economist() +
  labs(
    title = "Disney's movie releases",
    subtitle = "Original movies, movies based on movies and\nmovies based on other media, %",
    caption = "Sources: Wikipedia, Box Office Mojo"
  ) +
  theme(
    aspect.ratio = 1,
    text = element_text(colour = "#000000", size = 20, family = font),
    plot.title = element_text(size = 55, margin = margin(0, 0, 5, 0), lineheight = 1.2, family = title_font),
    plot.subtitle = element_text(size = 30, margin = margin(25,0,50,0), hjust = 0),
    plot.background = element_rect(fill = "#EFEFEF"),
    panel.background = element_rect(fill = "#EFEFEF"),
    legend.position = "NONE",
    legend.text = element_text(size = 10),
    legend.title = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 22),
    plot.caption = element_text(size = 22, hjust = 1, margin = margin(20, 0, 0, 0))
  )

Chart05_proportionChanges

# Save the plot as SVG
width_px <- 1000
height_px <- 1000
dpi <- 72

ggsave("finalBatch/045_proportionChanges/05proportionChange_ALT.svg", plot = Chart05_proportionChanges, device = svglite, width = width_px / dpi, height = height_px / dpi)
