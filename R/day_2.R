d <- read.delim("data/day_2/puzzle.txt", header = FALSE)

# Part 1 ----------
# Steps
# 1. Separate data by delimiters to long format, clean data
# 2. Get the max observed value for each game-color
# 3. Filter each game per designated color values
#     - 12 red
#     - 13 green
#     - 14 blue
# 4. Sum the filtered ids, i.e., the game numbers

library(tidyverse)

max_values <- d |> 
  # Step 1 - separate data
  separate_wider_delim(cols = V1, 
                       delim = ": ",
                       names = c("day", "results")) |> 
  separate_longer_delim(results, 
                       delim = regex("; |, ")) |> 
  # Step 1 - clean data
  transmute(game = str_remove(day, "Game ") |> 
              as.numeric(),
            value = str_extract(results, "\\d*") |> 
              as.numeric(),
            color = str_extract(results, "\\w*$")) |> 
  # Step 2 - get max values
  group_by(game, color) |> 
  summarise(max = max(value)) |> 
  ungroup() |> 
  # Step 2 - make wider so one game per row
  pivot_wider(names_from = color, values_from = max) 
  
# Step 3
max_values |> 
  filter(red < 13,
         green < 14,
         blue < 15) |> 
  # Step 4
  summarise(total = sum(game))


# Part 2 ----------

# Answer is the sum of the product of the max values
max_values |> 
  mutate(power = blue * green * red) |> 
  summarise(total_power = sum(power))
