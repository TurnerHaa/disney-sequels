# ---- Load packages ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, googlesheets4, janitor, ggthemes, extrafont, svglite)
# ---- Import from Google sheet ----
sheet_id <- "1L5-KtL4fBAD_Lyjn-aS4T3dIA19LFziF-7XUjx89jEk"

disneyAll <- read_sheet(sheet_id,
                           range = "A1:L629",
                           sheet = "updatedData")

# ---- Font setup ----
font_import()

title_font <-  "Cambria"
font <- "Poppins"

# ---- Clean primary dataset ----
historyData <- disneyAll |> 
  clean_names() |>    # Clean column names     
  arrange(release_date) |>    # Sort by release_date, earliest first
  mutate(
    row_number = row_number(),  # Create a sequential row number column
    X = ((row_number - 1) %% 8 + 1), # Add X value for plotting on grid
    Y = ceiling(row_number / 8), # Add Y value for plotting on grid
    based_on_other_media = as.factor(based_on_other_media),  # Convert to factor
    year = as.numeric(format(release_date, "%Y")), # Create column with just year as number
    decade = floor(year / 10) * 10, # Calculate decade based on year
    original = case_when( # Create new column on whether a film is an original or otherwise
      type == "Original" ~ "Y", 
      TRUE ~ "N"
    )
  ) |> 
  filter(year != 2025) |> # Remove movies not out yet as of October 2024
  filter(!(studio == "Pixar" & title == "Inside Out 2")) # Remove duplicate of Inside Out 2
  

# Invert Y so that earliest films are at the top
max_Y <- max(historyData$Y)

historyData <- historyData |> 
  mutate(Y = max_Y - Y)  


# ---- Quality check primary data ----
# Checking for duplicate movies
duplicates <- historyData |> 
  group_by(title) |> 
  summarize(
    n = n()
  ) |> 
  filter(n > 1) |> 
  arrange(desc(n))

rm(duplicates)

# Remove duplicates form Disney-Pixar
historyData <- historyData %>%
  group_by(title) %>%
  filter(!(studio == "Walt Disney Pictures" & "Pixar" %in% studio)) %>%
  ungroup()


# ---- Clean Lucas Film ----
lucasData <- historyData |>
  filter(studio == "Lucasfilm") |> 
  clean_names() |> 
  mutate(
    row_number = row_number(),
    X = ((row_number - 1) %% 8 + 1),
    Y = ceiling(row_number / 8) * 2,
    original = case_when(
      type == "Original" ~ "Y",
      TRUE ~ "N")
  )

# Invert Y so that earliest films are at the top
max_Y <- max(lucasData$Y)

lucasData <- lucasData |> 
  mutate(Y = max_Y - Y)  

# ---- Clean Marvel ----
marvelData <- historyData |>
  filter(studio == "Marvel") |> 
  clean_names() |> 
  mutate(
    row_number = row_number(),
    X = ((row_number - 1) %% 8 + 1),
    Y = ceiling(row_number / 8) *2,
    original = case_when(
      type == "Original" ~ "Y",
      TRUE ~ "N")
  )

# Invert Y so that earliest films are at the top
max_Y <- max(marvelData$Y)

marvelData <- marvelData |> 
  mutate(Y = max_Y - Y)  

# ---- Clear Pixar ---- 
pixarData <- historyData |>
  filter(studio == "Pixar") |> 
  clean_names() |> 
  mutate(
    row_number = row_number(),
    X = ((row_number - 1) %% 8 + 1),
    Y = ceiling(row_number / 8) * 2,
    original = case_when(
      type == "Original" ~ "Y",
      TRUE ~ "N")
  )

# Invert Y so that earliest films are at the top
max_Y <- max(pixarData$Y)

pixarData <- pixarData |> 
  mutate(Y = max_Y - Y) 

# ---- Clean Disney ---- 
disneyData <- historyData |>
  filter(studio == "Walt Disney Pictures") |> 
  clean_names() |> 
  mutate(
    row_number = row_number(),
    X = ((row_number - 1) %% 8 + 1),
    Y = ceiling(row_number / 8) * 2,
    original = case_when(
      type == "Original" ~ "Y",
      TRUE ~ "N")
  )
