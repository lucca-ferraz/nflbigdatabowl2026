def_epa_per_play <- sup |> 
  group_by(defensive_team) |> 
  summarise(plays = n(), epa_allowed = sum(expected_points_added), 
            epa_per_play = epa_allowed / plays,
            completions = sum(pass_result == "C"),
            comp_pct = completions / plays) |> 
  ungroup()

slime_vs_epa <- catch_prob_data |> 
  inner_join(sup |> select(game_id, play_id, defensive_team)) |> 
  inner_join(defender_ghost_coef |> mutate(level = as.numeric(level)) |> 
               select(level, estimate), 
             by = join_by(defender_nfl_id == level)) |> 
  group_by(defensive_team) |> 
  summarise(plays = n(), resid_cde = sum(cde_resid), slime_score = sum(estimate),
            delta_cde = resid_cde / plays, avg_slime_score = slime_score / plays) |> 
  ungroup() |> 
  inner_join(def_epa_per_play |> select(-plays))

summary(lm(comp_pct ~ slime_score, data = slime_vs_epa))

mid_x <- 0
mid_y <- mean(slime_vs_epa$comp_pct, na.rm = TRUE)
slime_vs_epa |> 
  ggplot(aes(avg_slime_score, comp_pct)) +
  geom_rect(
    xmin = -Inf, xmax = mid_x,
    ymin = -Inf, ymax = mid_y,
    fill = "darkgreen", alpha = 0.01
  ) +
  geom_rect(
    xmin = mid_x, xmax = Inf,
    ymin = mid_y, ymax = Inf,
    fill = "red3", alpha = 0.01
  ) + 
  geom_point(alpha = 0) +
  geom_nfl_logos(aes(team_abbr = defensive_team, width = 0.075, height = 0.1)) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_clean() +
  labs(title = "Lower Average SLIME, Lower Completion Percentage Allowed",
       subtitle = "All pass plays, 2023",
       y = "Completion Percentage Allowed",
       x = "Average SLIME Score") +
  theme(
    plot.title = ggtext::element_markdown(size = 15, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 10, face = "italic"),
    axis.title = ggtext::element_markdown(size = 13, face = "bold"),
    axis.text = ggtext::element_markdown(size = 12),
    text = element_text(family = "PT Sans")
  )


higgins_delpit <- catch_prob_data |> mutate(game_play_id = paste(game_id, play_id))
higgins_delpit_index <- which(catch_prob_data$game_id == 2023091002 & catch_prob_data$play_id == 2942)

round_resp <- catch_prob_data |> 
  as_tibble() |> 
  mutate(defender_arrival_x = round(defender_arrival_x),
         defender_arrival_y = round(defender_arrival_y))

higgins_delpit_data <- round_resp[higgins_delpit_index, ]

higgins_delpit_density <- field_grid |>
  mutate(density = full_cde_preds[higgins_delpit_index, ])

higgins_delpit_density <- higgins_delpit_density |> 
  left_join(
    mutate(round_resp[higgins_delpit_index,], obs_grid = 1),
    by = c("grid_x" = "defender_arrival_x",
           "grid_y" = "defender_arrival_y")
  ) |> 
  mutate(obs_grid = ifelse(is.na(obs_grid), 0,
                           obs_grid))

higgins_delpit_density <- higgins_delpit_density |> 
  # Remove rows that are negligible given the
  # grid size - unless it is the actual point
  filter((density > 1e-3) | (obs_grid == 1)) |> 
  mutate(density = pmax(density, 1e-3),
         # Compute normalized versions to sum to one:
         pred_prob = density / sum(density))

# --- 3. Compute axis limits ---
x_lim1 <- min(higgins_delpit_density$grid_x) - 5
x_lim2 <- max(higgins_delpit_density$grid_x) + 5

field_params <- list(
  field_apron = "springgreen3",
  field_border = "springgreen3",
  offensive_endzone = "springgreen3",
  defensive_endzone = "springgreen3",
  offensive_half = "springgreen3",
  defensive_half = "springgreen3"
)

delpit_heatmap <- geom_football(
  league = "nfl",
  display_range = "in_bounds_only",
  x_trans = 60,
  y_trans = 26.6667,
  xlims = c(min(higgins_delpit_density$grid_x) - 5, max(higgins_delpit_density$grid_x) + 5),
  color_updates = field_params
) +
  geom_tile(
    data = higgins_delpit_density,
    aes(x = grid_x, y = grid_y, fill = pred_prob),
    alpha = 0.7
  ) +
  scale_fill_viridis_c(option = "plasma") +
  # coord_cartesian(
  #   xlim = c(x_lim1, x_lim2),
  #   ylim = c(0, 53.3),
  #   expand = FALSE
  # ) +
  theme_minimal() +
  labs(
    title = paste("Grant Delpit Ghost Defender Distribution"),
    subtitle = "CDE of potential locations at time of ball arrival",
    fill = "Density",
    x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  theme(
    plot.title = ggtext::element_markdown(size = 15, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 10, face = "italic"),
    axis.title = ggtext::element_markdown(size = 13, face = "bold"),
    text = element_text(family = "PT Sans")
  )

readr_players <- nflreadr::load_players()

animate_delpit <- function(gameplayid, cde_preds, field_grid, test_data) {
  input_frames <- input |> 
    filter(game_play_id == gameplayid) |> 
    select(-ball_land_x, -ball_land_y)
  
  player_positions <- input_frames |> 
    select(nfl_id, player_position) |> 
    distinct()
  
  output_frames <- output |> 
    filter(game_play_id == gameplayid) |> 
    inner_join(input_roles) |> 
    inner_join(player_positions)
  
  all_frames <- bind_rows(input_frames, output_frames) |> 
    group_by(nfl_id) |> 
    mutate(frameid = row_number()) |> 
    mutate(team = ifelse(player_role == "Defensive Coverage", "CLE", "CIN"))
  
  first_output_frame <- all_frames |> 
    inner_join(output_frames) |> 
    pull(frameid) |> min()
  
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
  
  df_density_anim <- df_density |>
    crossing(
      frameid = unique(all_frames$frameid)
    ) |> 
    mutate(first_frame = first_output_frame) |> 
    mutate(
      alpha_density = ifelse(frameid >= first_output_frame, 0.7, 0)
    ) |> 
    filter(alpha_density > 0)
  
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
      data = df_density_anim,
      aes(x = grid_x, y = grid_y, fill = pred_prob, alpha = alpha_density),
    ) +
    scale_fill_viridis_c(option = "plasma", guide = "colorbar") +
    labs(fill = "Density") +
    # guides(
    #   fill = guide_legend("Density")
    # ) +
    scale_alpha_identity(guide = "none") +
    
    # --- football field ---
    # #base_field +
    new_scale_fill() +
    
    # --- players ---
    geom_point(
      data = all_frames,
      aes(x, y, fill = team),
      size = 6,
      shape = 21,
      color = "black",
    ) +
    
    # Add player positions as text
    geom_text(
      data = all_frames,
      aes(x, y, label = player_position, color = team),
      size = 3,          # adjust to your preference
      fontface = "bold",
      vjust = 0.5,
      hjust = 0.5
    ) +
    scale_fill_manual(
      values = c("CLE" = "#ff3c00", "CIN" = "white"),
      guide = FALSE
    ) + 
    scale_color_manual(
      values = c("CLE" = "#311D00", "CIN" = "#fb4f14"),
      guide = FALSE
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
    #shadow_mark(past = TRUE, future = FALSE) +
    
    # coord_cartesian(
    #   xlim = c(x_lim1, x_lim2),
    #   ylim = c(0, 53.3),
    #   expand = FALSE
    # ) +
    theme_minimal() +
    labs(
      title = paste("Conditional Density Estimate for Grant Delpit"),
      subtitle = "Heatmap = Distribution of potential locations at time of ball arrival",
      fill = "Density", x = "", y = ""
    ) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
    theme(
      plot.title = ggtext::element_markdown(size = 15, face = "bold"),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "italic"),
      axis.title = ggtext::element_markdown(size = 13, face = "bold"),
      text = element_text(family = "PT Sans")
    )
}
animate_delpit("2023091002 2942", full_cde_preds, field_grid, catch_point_data)

delpit_anim <- animate_delpit(
  "2023091002 2942",
  full_cde_preds,
  field_grid,
  catch_point_data
)
anim_save(
  "delpit_ghost_density2.gif",
  delpit_anim,
  width = 1080,
  height = 1080,
  res = 200,
  fps = 10
)

all_catch_probs_over_cde <- function(gameplayid, cde_preds, density_thresh = 1e-3) {
  data <- catch_prob_data |> mutate(game_play_id = paste(game_id, play_id))
  play_index <- which(data$game_play_id == gameplayid)
  
  # --- 1. CDE grid for this play ---
  df_grid <- field_grid |>
    mutate(density = full_cde_preds[play_index, ]) |>
    filter(density > density_thresh) |>
    mutate(prob = density / sum(density))
  
  # --- 2. Base play row ---
  play_row <- catch_prob_data[play_index, ]
  
  # --- 3. Create counterfactual rows over grid ---
  cf_data <- df_grid |>
    mutate(
      defender_arrival_x = grid_x,
      defender_arrival_y = grid_y
    ) |>
    bind_cols(
      play_row[rep(1, nrow(df_grid)), ] |>
        select(-defender_arrival_x, -defender_arrival_y)
    )
  
  # --- 4. Recompute ALL defender-dependent features ---
  # (these MUST match how you built them originally)
  cf_data <- cf_data |>
    mutate(
      end_dist_from_receiver = 
        sqrt((defender_arrival_x - receiver_arrival_x)^2 + (defender_arrival_y - receiver_arrival_y)^2),
      end_x_from_receiver = defender_arrival_x - receiver_arrival_x,
      end_y_from_receiver = defender_arrival_y - receiver_arrival_y,
      
      def_dist_from_ball = 
        sqrt((defender_arrival_x - ball_land_x)^2 + (defender_arrival_y - ball_land_y)^2),
      defender_x_from_ball = defender_arrival_x - ball_land_x,
      defender_y_from_ball = defender_arrival_y - ball_land_y
    )
  
  # --- 5. Build design matrix (EXACT SAME FORMULA) ---
  X_cf <- model.matrix(
    completion ~ defender_start_s + defender_start_a + defender_start_o
    + receiver_start_s + receiver_start_a 
    + receiver_start_dir + receiver_start_o + frames + qb_s
    + qb_x_change + qb_y_change_absval + qb_dist_from_receiver 
    + receiver_x_from_first_down
    + start_x_change_from_receiver + start_y_change_from_receiver
    + start_dist_from_receiver + end_dist_from_receiver 
    + end_x_from_receiver + end_y_from_receiver + receiver_dist_from_ball
    + receiver_x_from_ball + receiver_y_from_ball + def_dist_from_ball
    + defender_x_from_ball + defender_y_from_ball,
    data = cf_data
  )
  
  # --- 6. Predict catch probability ---
  p_catch <- predict(full_xg_model, X_cf)
  
  cf_data <- cf_data |> 
    mutate(catch_prob = p_catch) |> 
    select(grid_x, grid_y, prob, catch_prob)
  return(cf_data)
}
delpit_cde_catch_prob <- all_catch_probs_over_cde("2023091002 2942", full_cde_preds)

plot_cde_catch_prob <- function(gameplayid, cde_preds, density_thresh = 1e-3){
  data <- all_catch_probs_over_cde("2023091002 2942", full_cde_preds, density_thresh)
  
  x_lim1 <- min(data$grid_x) - 5
  x_lim2 <- max(data$grid_x) + 5
  
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
  
  receiver_x <- catch_prob_data |> 
    mutate(game_play_id = paste(game_id, play_id)) |> 
    filter(game_play_id == gameplayid) |> 
    pull(receiver_arrival_x)
  
  receiver_y <- catch_prob_data |> 
    mutate(game_play_id = paste(game_id, play_id)) |> 
    filter(game_play_id == gameplayid) |> 
    pull(receiver_arrival_y)
  
  # --- 5. Build the full animation ---
  base_field + 
    #ggplot() +
    # --- density heatmap (static backdrop) ---
    geom_tile(
      data = data,
      aes(x = grid_x, y = grid_y, fill = catch_prob),
      alpha = 1
    ) +
    # scale_fill_viridis_c(
    #   option = "cividis",
    #   limits = c(0, 1),
    #   name = "Catch\nProbability"
    # ) + 
    scale_fill_gradient2(
      low = "#D7191C",     # blue (low catch prob)
      mid = "white",
      high = "#2C7BB6",    # red (high catch prob)
      midpoint = 0.5,
      limits = c(0, 1),
      name = "Catch\nProbability",
    ) + 
    labs(title = "Catch Probability at Ghost Defender Locations",
         subtitle = "Grant Delpit, 3rd and 5, Browns vs Bengals, Week 1, 2023",
         fill = "Catch Probability") +
    theme(
      plot.title = ggtext::element_markdown(size = 15, face = "bold"),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "italic"),
      text = element_text(family = "PT Sans")
    )
    # geom_point(
    #   data = data,
    #   aes(
    #     x = grid_x,
    #     y = grid_y,
    #     size = prob,
    #     color = catch_prob
    #   ),
    #   # shape = 22,      # square
    #   alpha = 0.7
    # ) +
    # scale_size(guide = "none") +
    # scale_color_viridis_c(option = "plasma")
    # geom_point(aes(receiver_x, receiver_y),
    #            size = 5, shape = 21, fill = "white", color = "black",
    #            alpha = 0.5)
}
delpit_catch_prob <- plot_cde_catch_prob("2023091002 2942", full_cde_preds)

library(patchwork)
delpit_heatmap + delpit_catch_prob
delpit_heatmap + plot_spacer() + delpit_catch_prob +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(theme = theme(plot.margin = margin(0, 10, 0, 10)))

delpit_heatmap + plot_spacer() + delpit_catch_prob +
  plot_layout(ncol = 3, widths = c(1, 0.05, 1)) +  # small spacer in the middle
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))

library(gridExtra)
grid.arrange(delpit_heatmap, delpit_catch_prob, ncol = 2)
