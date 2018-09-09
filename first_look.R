rm(list = ls())

library(readr)
library(tidyverse)
library(highcharter)
library(lubridate)
library(gridExtra)
library(RColorBrewer)
library(splitstackshape) # for delimited values

files <-  list.files(pattern="*.csv")

full_data <-  lapply(files, read_csv) %>% 
  bind_rows() %>%
  select(-bgg_url, -image_url)


# select the row where num_votes is highest (ie assuming this is the latest data)
unique_games <- full_data %>% 
  group_by(game_id) %>% 
  mutate(rank_most_votes = order(order(num_votes, decreasing=TRUE))) %>%
  filter(rank_most_votes == 1) %>%
  select(-rank_most_votes)

# find games per year per rating
unique_games_by_year <- full_data %>%
  group_by(year = ymd(year, truncated = 2L), average_rating = as.integer(avg_rating)) %>%
  filter(year > ymd('1950', truncated = 2L)) %>%
  summarise(total=n())


# when most popular games were released
# hchart(unique_games_by_year, "line", hcaes(x = year, y = total, group = average_rating))


# what are the most common ratings
# hchart(unique_games$avg_rating)

# scatterplot of average rating and geek rating - clearly geek rating is stricter
# ggplot(unique_games, aes(x=avg_rating, y=geek_rating)) +
#   geom_point() +
#   theme_minimal() 

str_split_fixed(unique_games$mechanic, ", ", 15) -> matrix_of_mechanics

unique_games$mechanic <- as.character(unique_games$mechanic)
unique_games_wide <- cSplit(unique_games,splitCols = "mechanic", direction = "long")


# data.frame(unique_games %>% select(-mechanic), 
#            matrix_of_mechanics %>% na.omit()) -> unique_games_mechanic_wide
# unique_games_mechanic_wide %>% 
#   gather("remove_me", "game_mechanic", 18:32) %>%
#   select(-remove_me) -> unique_games_mechanic_wide
# unique_games_mechanic_wide$game_mechanic[unique_games_mechanic_wide$game_mechanic==""]  <- NA 
# unique_games_mechanic_wide %>% 
#   na.omit() %>% arrange(game_id) %>%
#   spread(game_mechanic, new_col)
