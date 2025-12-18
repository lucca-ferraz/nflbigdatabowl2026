players <- input |> 
  select(game_play_id, nfl_id, player_name, player_side) |> 
  distinct()

all_output <- output |> 
  inner_join(sup) |> 
  inner_join(players)

all_input <- input |> 
  inner_join(sup)

qb_start <- all_input |> 
  filter(player_role == "Passer") |> 
  group_by(game_id, play_id) |> 
  summarise(qb_x = last(x), qb_y = last(y), qb_s = last(s)) |> 
  ungroup()

receiver_start <- all_input |> 
  filter(player_to_predict) |> 
  group_by(game_id, play_id) |> 
  filter(player_side == "Offense") |> 
  summarise(receiver_start_x = last(x), receiver_start_y = last(y), 
            receiver_start_s = last(s), receiver_start_a = last(a),
            receiver_start_dir = last(dir), receiver_start_o = last(o), 
            ball_land_x = first(ball_land_x),
            ball_land_y = first(ball_land_y)) |> 
  ungroup()

receiver_arrivals <- all_output |>  # for each play, there is one offensive player in the output
  group_by(game_id, play_id) |> 
  filter(player_side == "Offense") |> 
  summarise(receiver_arrival_x = last(x), receiver_arrival_y = last(y),
            frames = last(frame_id)) |> 
  ungroup()

# for each play, there could be more than one defensive player in the output
defender_arrivals <- all_output |> 
  group_by(game_id, play_id, nfl_id) |> 
  filter(player_side == "Defense") |> 
  summarise(defender_arrival_x = last(x), defender_arrival_y = last(y)) |> 
  ungroup()

defender_start <- all_input |> 
  filter(player_to_predict) |> 
  filter(player_side == "Defense") |> 
  group_by(game_id, play_id, nfl_id) |> 
  summarise(defender_start_x = last(x), defender_start_y = last(y), 
            defender_start_s = last(s), defender_start_a = last(a),
            defender_start_dir = last(dir), defender_start_o = last(o),
            defender_position = first(player_position),
            play_action = first(play_action)) |> 
  ungroup()

# find closest defender to the receiver at the time ball lands/is caught
closest_defender <- defender_arrivals |> 
  inner_join(receiver_arrivals) |> 
  mutate(end_dist_from_receiver = 
           sqrt((defender_arrival_x - receiver_arrival_x)^2 + 
                  (defender_arrival_y - receiver_arrival_y)^2)) |> 
  group_by(game_id, play_id) |> 
  arrange(end_dist_from_receiver) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  mutate(game_play_nfl_id = paste(game_id, play_id, nfl_id)) |> 
  pull(game_play_nfl_id)

catch_point_data <- defender_start |> 
  inner_join(receiver_start) |> 
  inner_join(receiver_arrivals) |> 
  inner_join(qb_start) |> 
  mutate(qb_x_change = receiver_arrival_x - qb_x, 
         qb_y_change_absval = abs(receiver_arrival_y - qb_y),
         qb_dist_from_receiver = 
           sqrt((qb_x - receiver_arrival_x)^2 + (qb_y - receiver_arrival_y)^2)) |> 
  select(-qb_x, -qb_y) |> 
  inner_join(sup |> select(game_id, play_id, week, route_of_targeted_receiver, 
                           team_coverage_type, adj_x_first_down)) |> 
  mutate(receiver_x_from_first_down = receiver_arrival_x - adj_x_first_down + 10) |> 
  select(-adj_x_first_down) |> 
  mutate(start_x_change_from_receiver = receiver_start_x - defender_start_x,
         start_y_change_from_receiver = receiver_start_y - defender_start_y,
         start_dist_from_receiver = 
           sqrt((defender_start_x - receiver_start_x)^2 + (defender_start_y - receiver_start_y)^2)) |> 
  inner_join(defender_arrivals) |> 
  #filter(defender_position != "DT" & defender_position != "NT") |> 
  filter(defender_position != "FB") |> 
  filter(!is.na(route_of_targeted_receiver)) |> 
  #filter(team_coverage_type != "PREVENT") |> 
  filter(!is.na(team_coverage_type)) |> 
  mutate(game_play_nfl_id = paste(game_id, play_id, nfl_id)) |> 
  filter(game_play_nfl_id %in% closest_defender) |> 
  select(-game_play_nfl_id)

# install.packages("fastDummies")
library(fastDummies)
catch_point_dummy_data <- catch_point_data |> 
  dummy_cols() |> 
  select(-route_of_targeted_receiver, -team_coverage_type, -defender_position) 

field_grid <- expand.grid("grid_x" = 0:120, "grid_y" = 0:54)

# devtools::install_github("tpospisi/RFCDE/r")
# devtools::install_github("tpospisi/cdetools/r")
library(RFCDE)
library(cdetools)
set.seed(123)

model_eval <- tibble()
evaluate_model <- function(weeks){
  next_week <- weeks + 1
  train <- catch_point_dummy_data |> 
    filter(week <= weeks)
  test <- catch_point_dummy_data |> 
    filter(week == next_week)
  x_train <- train |> 
    select(defender_start_x:start_dist_from_receiver, defender_position_CB:team_coverage_type_PREVENT) |> 
    as.matrix()
  z_train <- train |> 
    select(defender_arrival_x, defender_arrival_y) |> 
    as.matrix()
  
  rfcde_model <- RFCDE(x_train, z_train, n_trees = 500, n_basis = 15)
  #variable_importance(rfcde_model)
  
  x_test <- test |> 
    select(defender_start_x:start_dist_from_receiver, defender_position_CB:team_coverage_type_PREVENT) |> 
    as.matrix()
  z_test <- test |> 
    select(defender_arrival_x, defender_arrival_y) |> 
    mutate(defender_arrival_x = round(defender_arrival_x),
           defender_arrival_y = round(defender_arrival_y)) |> 
    # Provide necessary caps and floors based on the grid:
    mutate(defender_arrival_x = pmax(pmin(defender_arrival_x, 120), 0),
           defender_arrival_y = pmax(pmin(defender_arrival_y, 54), 0)) |> 
    as.matrix()
  cde_preds <- predict(rfcde_model, x_test, "CDE", field_grid)
  loss <- cde_loss(cde_preds, field_grid, z_test) #-0.005417882
  loss_loss <- loss$loss
  loss_se <- loss$se
  print(paste("weeks trained on:", weeks, "  CDE loss:", loss_loss))
  return(tibble(train_weeks = weeks, cde_loss = loss_loss, cde_se = loss_se))
}
evaluate_model(1)
for (i in seq(1, 17)){
  res <- evaluate_model(i)
  model_eval <- bind_rows(model_eval, res)
}


full_model_x_data <- catch_point_dummy_data |> 
  select(defender_start_x:start_dist_from_receiver, defender_position_CB:team_coverage_type_PREVENT) |> 
  as.matrix()

baseline_rfcde_x <- catch_point_

full_model_z_data <- catch_point_dummy_data |> 
  select(defender_arrival_x, defender_arrival_y) |> 
  as.matrix()

full_cde_model <- RFCDE(full_model_x_data, full_model_z_data, n_trees = 500, n_basis = 15)

full_model_z_test_data <- catch_point_dummy_data |> 
  select(defender_arrival_x, defender_arrival_y) |> 
  mutate(defender_arrival_x = round(defender_arrival_x),
         defender_arrival_y = round(defender_arrival_y)) |> 
  # Provide necessary caps and floors based on the grid:
  mutate(defender_arrival_x = pmax(pmin(defender_arrival_x, 120), 0),
         defender_arrival_y = pmax(pmin(defender_arrival_y, 54), 0)) |> 
  as.matrix()

full_cde_preds <- predict(full_cde_model, full_model_x_data, "CDE", field_grid) #takes about an hour
full_cde_loss <- cde_loss(full_cde_preds, field_grid, full_model_z_test_data)

full_cde_loss

plot_cde_play <- function(play_index, cde_preds, field_grid, test_data) {
  
  df <- field_grid |>
    mutate(density = cde_preds[play_index, ])
  
  start_x <- test_data$defender_start_x[play_index]
  start_y <- test_data$defender_start_y[play_index]
  
  # actual observed location for this play
  actual_x <- test_data$defender_arrival_x[play_index]
  actual_y <- test_data$defender_arrival_y[play_index]
  
  receiver_x <- test_data$receiver_arrival_x[play_index]
  receiver_y <- test_data$receiver_arrival_y[play_index]
  
  ball_land_x <- test_data$ball_land_x[play_index]
  ball_land_y <- test_data$ball_land_y[play_index]
  
  ggplot(df, aes(x = grid_x, y = grid_y, fill = density)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma") +
    geom_point(aes(x = actual_x, y = actual_y),
               color = "white", size = 3) +
    geom_point(aes(x = ball_land_x, y = ball_land_y),
               color = "brown", size = 3) +
    geom_point(aes(x = receiver_x, y = receiver_y),
               color = "black", size = 3) +
    geom_point(aes(x = start_x, y = start_y),
               color = "red", size = 3) +
    coord_fixed() +
    theme_minimal() +
    labs(
      title = paste("RFCDE Predicted Density — Play", play_index),
      subtitle = "White point = actual defender arrival location",
      x = "X coordinate",
      y = "Y coordinate",
      fill = "Density"
    )
}
plot_cde_play(2, full_cde_preds, field_grid, catch_point_data)

animate_play_with_density <- function(gameplayid, cde_preds, field_grid, test_data) {
  input_frames <- input |> 
    filter(game_play_id == gameplayid)
  
  output_frames <- output |> 
    filter(game_play_id == gameplayid) |> 
    inner_join(input_roles)
  
  all_frames <- bind_rows(input_frames, output_frames) |> 
    group_by(nfl_id) |> 
    mutate(frameid = row_number())
  
  test_data <- test_data |> mutate(game_play_id = paste(game_id, play_id))
  play_index <- which(test_data$game_play_id == gameplayid)

  round_test_data_resp <- test_data |> 
    as_tibble() |> 
    mutate(defender_arrival_x = round(defender_arrival_x),
           defender_arrival_y = round(defender_arrival_y))
  
  # --- 2. Compute density grid for this play ---
  df_density <- field_grid |>
    mutate(density = cde_preds[play_index, ])
  
  df_density <- df_density |> 
    left_join(
      mutate(round_test_data_resp[play_index,], obs_grid = 1),
      by = c("grid_x" = "defender_arrival_x",
             "grid_y" = "defender_arrival_y")
    ) |> 
    mutate(obs_grid = ifelse(is.na(obs_grid), 0,
                             obs_grid))
  
  df_density <- df_density |> 
    # Remove rows that are negligible given the
    # grid size - unless it is the actual point
    filter((density > 1e-3) | (obs_grid == 1)) |> 
    mutate(density = pmax(density, 1e-3),
           # Compute normalized versions to sum to one:
           pred_prob = density / sum(density))

  # --- 3. Compute axis limits ---
  x_lim1 <- min(c(all_frames$x, df_density$grid_x)) - 5
  x_lim2 <- max(c(all_frames$x, df_density$grid_x)) + 5
  
  # --- 4. Field params + base field layer ---
  field_params <- list(
    field_apron = "springgreen3",
    field_border = "springgreen3",
    offensive_endzone = "springgreen3",
    defensive_endzone = "springgreen3",
    offensive_half = "springgreen3",
    defensive_half = "springgreen3"
  )
  
  base_field <- geom_football(
    league = "nfl",
    display_range = "in_bounds_only",
    x_trans = 60,
    y_trans = 26.6667,
    xlims = c(x_lim1, x_lim2),
    color_updates = field_params
  )
  
  
  # --- 5. Build the full animation ---
  base_field + 
    #ggplot() +
    # --- density heatmap (static backdrop) ---
    geom_tile(
      data = df_density,
      aes(x = grid_x, y = grid_y, fill = pred_prob),
      alpha = 0.7
    ) +
    scale_fill_viridis_c(option = "plasma") +
    
    # --- football field ---
    # #base_field +
    new_scale_fill() +
    
    # --- players ---
    geom_point(
      data = all_frames,
      aes(x, y, fill = player_role),
      size = 5,
      shape = 21,
      color = "black"
    ) +
    
    # --- ball landing point (static or from frames) ---
    geom_point(
      data = all_frames,
      aes(ball_land_x, ball_land_y),
      size = 5,
      shape = 21,
      fill = "brown",
      color = "black"
    ) +
    
    # --- animation over time ---
    transition_time(frameid) +
    
    coord_cartesian(
      xlim = c(x_lim1, x_lim2),
      ylim = c(0, 53.3),
      expand = FALSE
    ) +
    theme_minimal() +
    labs(
      title = paste("Play", gameplayid, "with RFCDE Defender Density"),
      subtitle = "Heatmap = predicted defender arrival distribution",
      fill = "Density"
    )
}
animate_play_with_density("2023091002 2942", full_cde_preds, field_grid, catch_point_data)

plot_cde_play <- function(gameplayid, cde_preds, field_grid, test_data) {
  test_data <- test_data |> mutate(game_play_id = paste(game_id, play_id))
  play_index <- which(test_data$game_play_id == gameplayid)
  
  round_test_data_resp <- test_data |> 
    as_tibble() |> 
    mutate(defender_arrival_x = round(defender_arrival_x),
           defender_arrival_y = round(defender_arrival_y))
  
  df_density <- field_grid |>
    mutate(density = cde_preds[play_index, ])
  
  df_density <- df_density |> 
    left_join(
      mutate(round_test_data_resp[play_index,], obs_grid = 1),
      by = c("grid_x" = "defender_arrival_x",
             "grid_y" = "defender_arrival_y")
    ) |> 
    mutate(obs_grid = ifelse(is.na(obs_grid), 0,
                             obs_grid))
  
  df_density <- df_density |> 
    # Remove rows that are negligible given the
    # grid size - unless it is the actual point
    filter((density > 1e-3) | (obs_grid == 1)) |> 
    mutate(density = pmax(density, 1e-3),
           # Compute normalized versions to sum to one:
           pred_prob = density / sum(density))
  
  # --- 3. Compute axis limits ---
  x_lim1 <- min(df_density$grid_x) - 5
  x_lim2 <- max(df_density$grid_x) + 5
  
  field_params <- list(
    field_apron = "springgreen3",
    field_border = "springgreen3",
    offensive_endzone = "springgreen3",
    defensive_endzone = "springgreen3",
    offensive_half = "springgreen3",
    defensive_half = "springgreen3"
  )
  
  base_field <- geom_football(
    league = "nfl",
    display_range = "in_bounds_only",
    x_trans = 60,
    y_trans = 26.6667,
    xlims = c(x_lim1, x_lim2),
    color_updates = field_params
  )
  
  # start_x <- test_data$defender_start_x[play_index]
  # start_y <- test_data$defender_start_y[play_index]
  
  # actual observed location for this play
  actual_x <- test_data$defender_arrival_x[play_index]
  actual_y <- test_data$defender_arrival_y[play_index]
  
  receiver_x <- test_data$receiver_arrival_x[play_index]
  receiver_y <- test_data$receiver_arrival_y[play_index]
  
  ball_land_x <- test_data$ball_land_x[play_index]
  ball_land_y <- test_data$ball_land_y[play_index]
  
  # ggplot(df_density, aes(x = grid_x, y = grid_y, fill = density)) +
  #   geom_tile() +
  #   scale_fill_viridis_c(option = "plasma") +
  #   geom_point(aes(x = actual_x, y = actual_y),
  #              color = "white", size = 3) +
  #   geom_point(aes(x = ball_land_x, y = ball_land_y),
  #              color = "brown", size = 3) +
  #   geom_point(aes(x = receiver_x, y = receiver_y),
  #              color = "black", size = 3) +
  #   geom_point(aes(x = start_x, y = start_y),
  #              color = "red", size = 3) +
  #   coord_fixed() +
  #   theme_minimal() +
  #   labs(
  #     title = paste("RFCDE Predicted Density — Play", play_index),
  #     subtitle = "White point = actual defender arrival location",
  #     x = "X coordinate",
  #     y = "Y coordinate",
  #     fill = "Density"
  #   )
  
  base_field + 
    #ggplot() +
    # --- density heatmap (static backdrop) ---
    geom_tile(
      data = df_density,
      aes(x = grid_x, y = grid_y, fill = pred_prob),
      alpha = 0.7
    ) +
    scale_fill_viridis_c(option = "plasma") +
    
    # --- football field ---
    # #base_field +
    # new_scale_fill() +
    # 
    # # --- players ---
    # geom_point(
    #   data = all_frames,
    #   aes(x, y, fill = player_role),
    #   size = 5,
    #   shape = 21,
    #   color = "black"
    # ) +
    
    # --- ball landing point (static or from frames) ---
    geom_point(
      aes(ball_land_x, ball_land_y),
      size = 5,
      shape = 21,
      fill = "brown",
      color = "black"
    ) +
    geom_point(
      aes(receiver_x, receiver_y),
      size = 5,
      shape = 21,
      fill = "purple",
      color = "black"
    ) +
    geom_point(
      aes(actual_x, actual_y),
      size = 5,
      shape = 21,
      fill = "red",
      color = "black"
    ) +
    
    # --- animation over time ---
    
    coord_cartesian(
      xlim = c(x_lim1, x_lim2),
      ylim = c(0, 53.3),
      expand = FALSE
    ) +
    theme_minimal() +
    labs(
      title = paste("Play", gameplayid, "with RFCDE Defender Density"),
      subtitle = "Heatmap = predicted defender arrival distribution",
      fill = "Density"
    )
}
plot_cde_play("2023091002 2942", full_cde_preds, field_grid, catch_point_data)

as_tibble(variable_importance(full_cde_model)) |> mutate(column = full_cde_model$x_names) |> View()
