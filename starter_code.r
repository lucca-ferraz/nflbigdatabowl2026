library(tidyverse)
library(gganimate)
library(sportyR)
# install.packages("ggnewscale")
library(ggnewscale)
sup <- read_csv("supplementary_data.csv")

# input <- read_csv("train/input_2023_w01.csv") |>
#   bind_rows(read_csv("train/input_2023_w02.csv")) |>
#   bind_rows(read_csv("train/input_2023_w03.csv")) |>
#   bind_rows(read_csv("train/input_2023_w04.csv")) |>
#   bind_rows(read_csv("train/input_2023_w05.csv")) |>
#   bind_rows(read_csv("train/input_2023_w06.csv")) |>
#   bind_rows(read_csv("train/input_2023_w07.csv")) |>
#   bind_rows(read_csv("train/input_2023_w08.csv")) |>
#   bind_rows(read_csv("train/input_2023_w09.csv")) |> 
#   bind_rows(read_csv("train/input_2023_w10.csv")) |> 
#   bind_rows(read_csv("train/input_2023_w11.csv")) |> 
#   bind_rows(read_csv("train/input_2023_w12.csv")) |> 
#   bind_rows(read_csv("train/input_2023_w13.csv")) |> 
#   bind_rows(read_csv("train/input_2023_w14.csv")) |> 
#   bind_rows(read_csv("train/input_2023_w15.csv")) |> 
#   bind_rows(read_csv("train/input_2023_w16.csv")) |> 
#   bind_rows(read_csv("train/input_2023_w17.csv")) |> 
#   bind_rows(read_csv("train/input_2023_w18.csv")) 
# 
# arrow::write_parquet(input, "input.parquet")
# 
# output <- read_csv("train/output_2023_w01.csv") |>
#   bind_rows(read_csv("train/output_2023_w02.csv")) |>
#   bind_rows(read_csv("train/output_2023_w03.csv")) |>
#   bind_rows(read_csv("train/output_2023_w04.csv")) |>
#   bind_rows(read_csv("train/output_2023_w05.csv")) |>
#   bind_rows(read_csv("train/output_2023_w06.csv")) |>
#   bind_rows(read_csv("train/output_2023_w07.csv")) |>
#   bind_rows(read_csv("train/output_2023_w08.csv")) |>
#   bind_rows(read_csv("train/output_2023_w09.csv")) |> 
#   bind_rows(read_csv("train/output_2023_w10.csv")) |> 
#   bind_rows(read_csv("train/output_2023_w11.csv")) |> 
#   bind_rows(read_csv("train/output_2023_w12.csv")) |> 
#   bind_rows(read_csv("train/output_2023_w13.csv")) |> 
#   bind_rows(read_csv("train/output_2023_w14.csv")) |> 
#   bind_rows(read_csv("train/output_2023_w15.csv")) |> 
#   bind_rows(read_csv("train/output_2023_w16.csv")) |> 
#   bind_rows(read_csv("train/output_2023_w17.csv")) |> 
#   bind_rows(read_csv("train/output_2023_w18.csv"))
# 
# arrow::write_parquet(output, "output.parquet")

input <- arrow::read_parquet("input.parquet")
output <- arrow::read_parquet("output.parquet")

sup <- sup |> 
  mutate(yds_from_tgt_endzone =
           ifelse((possession_team != yardline_side) |
                    # Plays at the 50 yard marker have NA for yardlineSide
                    (yardline_number == 50), yardline_number,
                  100 - yardline_number),
         adj_x_first_down = 110 - (yds_from_tgt_endzone - yards_to_go))

# standardize tracking data coordinates
input <- input |> 
  mutate(game_play_id = paste(game_id, play_id)) |>
  mutate(
    x = ifelse(play_direction == "left", 120 - x, x),
    y = ifelse(play_direction == "left", 160 / 3 - y, y),
    ball_land_x = ifelse(play_direction == "left", 120 - ball_land_x, ball_land_x),
    ball_land_y = ifelse(play_direction == "left", 160 / 3 - ball_land_y, ball_land_y),
    dir = ifelse(play_direction == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(play_direction == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )
play_directions <- input |> 
  select(game_id, play_id, play_direction) |> 
  distinct()
output <- output |> 
  inner_join(play_directions) |> 
  mutate(game_play_id = paste(game_id, play_id)) |> 
  mutate(
    x = ifelse(play_direction == "left", 120 - x, x),
    y = ifelse(play_direction == "left", 160 / 3 - y, y)
  )
input_roles <- input |> 
  group_by(game_play_id, nfl_id) |> 
  summarise(player_role = first(player_role),
            ball_land_x = first(ball_land_x),
            ball_land_y = first(ball_land_y))

animate_input <- function(gameplayid){
  input_frames <- input |> 
    filter(game_play_id == gameplayid)
  input_frames |> 
    ggplot(aes(x, y, fill = player_role)) +
    geom_point(size = 5, shape = 21, color = "black") +
    geom_point(aes(ball_land_x, ball_land_y), size = 5, shape = 21, fill = "brown",
               color = "black") +
    transition_time(input_frames$frame_id)
}
animate_input("2023090700 1300")

animate_output <- function(gameplayid){
  output_frames <- output |>
    filter(game_play_id == gameplayid) |> 
    inner_join(input_roles)
  output_frames |> 
    ggplot(aes(x, y, fill = player_role)) + 
    geom_point(size = 5, shape = 21, color = "black") +
    geom_point(aes(ball_land_x, ball_land_y), size = 5, shape = 21, fill = "brown",
               color = "black") +
    transition_time(output_frames$frame_id)
}
animate_output("2023090700 1422")

animate_play <- function(gameplayid){
  input_frames <- input |> 
    filter(game_play_id == gameplayid) |> 
    select(-ball_land_x, -ball_land_y)
  output_frames <- output |> 
    filter(game_play_id == gameplayid) |> 
    inner_join(input_roles)
  all_frames <- bind_rows(input_frames, output_frames)
  all_frames <- all_frames |> 
    group_by(nfl_id) |> 
    mutate(frameid = row_number())
  x_lim1 <- all_frames |> pull(x) |> min() - 5
  x_lim2 <- all_frames |> pull(x) |> max() + 5
  
  field_params <- list(field_apron = "springgreen3",
                       field_border = "springgreen3",
                       offensive_endzone = "springgreen3",
                       defensive_endzone = "springgreen3",
                       offensive_half = "springgreen3",
                       defensive_half = "springgreen3")
  nfl_field <- geom_football(league = "nfl",
                             display_range = "in_bounds_only",
                             x_trans = 60,
                             y_trans = 26.6667,
                             xlims = c(x_lim1, x_lim2),
                             color_updates = field_params)
  nfl_field + 
    geom_point(data = all_frames,
               aes(x, y, fill = player_role),
               size = 5,
               shape = 21,
               color = "black") +
    geom_point(data = all_frames,
               aes(ball_land_x, ball_land_y),
               size = 5,
               shape = 21,
               fill = "brown", color = "black") +
    transition_time(all_frames$frameid)
  # all_frames |> 
  #   ggplot(aes(x, y, fill = player_role)) + 
  #   geom_point(size = 5, shape = 21, color = "black") +
  #   geom_point(aes(ball_land_x, ball_land_y), size = 5, shape = 21, fill = "brown",
  #              color = "black") +
  #   transition_time(all_frames$frameid)
}
animate_play("2023091002 2942")
