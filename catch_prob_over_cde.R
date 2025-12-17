integrate_catch_prob_over_cde <- function(play_index, cde_preds, density_thresh = 1e-3) {
  
  # --- 1. CDE grid for this play ---
  df_grid <- field_grid |>
    mutate(density = cde_preds[play_index, ]) |>
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
  
  # --- 7. Integrate over CDE ---
  expected_catch_prob <- sum(p_catch * cf_data$prob)
  
  return(expected_catch_prob)
}
integrate_catch_prob_over_cde(2, full_cde_preds)

avg_catch_prob <- map_dbl(seq_len(nrow(catch_prob_data)), 
                          ~ integrate_catch_prob_over_cde(.x, full_cde_preds))
catch_prob_data <- catch_prob_data |> 
  mutate(cde_catch_prob = avg_catch_prob,
         cde_resid = prob_complete - cde_catch_prob)
catch_prob_data |> 
  ggplot(aes(prob_complete, cde_catch_prob)) +
  geom_point()

library(lme4)
library(broom.mixed)
ghost_mixed_effects <- lmer(cde_resid ~ (1 | defender_nfl_id) + (1 | receiver_nfl_id),
                                  data = catch_prob_data)
summary(ghost_mixed_effects)
ghost_coef <- tidy(ghost_mixed_effects, effects = "ran_vals")
defender_ghost_coef <- ghost_coef |> 
  filter(group == "defender_nfl_id")
receiver_ghost_coef <- ghost_coef |> 
  filter(group == "receiver_nfl_id")

player_names <- players |> 
  select(nfl_id, player_name) |> 
  distinct()

all_coef <- ghost_coef |> 
  rename(ghost_estimate = estimate, ghost_se = std.error) |> 
  inner_join(tidy_coef) |> 
  mutate(total_estimate = ghost_estimate + estimate)

ghost_defender_totals <- catch_prob_data |> 
  group_by(defender_nfl_id) |> 
  summarise(plays = n(), cpoe = sum(cde_resid)) |> 
  ungroup()

receiver_ghost_totals <- catch_prob_data |> 
  group_by(receiver_nfl_id) |> 
  summarise(plays = n(), cpoe = sum(cde_resid)) |> 
  ungroup()

library(gt)
library(nflplotR)
library(nflfastR)
library(gtExtras)
readr_players <- nflreadr::load_players()
slimy_def <- defender_ghost_coef |> 
  mutate(level = as.numeric(level)) |> 
  inner_join(ghost_defender_totals, by = join_by(level == defender_nfl_id)) |> 
  inner_join(player_names, by = join_by(level == nfl_id)) |> 
  inner_join(readr_players |> select(player = display_name, nfl_id, gsis_id, position) |> 
               mutate(nfl_id = as.numeric(nfl_id)),
             by = join_by(level == nfl_id)) |> 
  select(player_name, gsis_id, plays, estimate) |> 
  arrange(estimate) |> 
  slice_head(n = 5) |> 
  gt() |> 
  gtExtras::gt_theme_538() |> 
  #gt_nfl_logos("team", height = 40) |> 
  gt_nfl_headshots("gsis_id", height = 40) |> 
  data_color(columns = estimate, palette = "RdYlGn", domain = c(-0.05, 0.033),
             reverse = TRUE) |> 
  fmt_number(columns = estimate, decimals = 3) |> 
  cols_label(gsis_id = "", estimate = "SLIME", player_name = "Player") |> 
  tab_header(title = md("**Slimiest Defenders, 2023**")) |> 
  tab_style(
    style = cell_text(font = "PT Sans", weight = "bold", size = px(20)),   
    locations = cells_body(columns = estimate) 
  ) |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = player_name)
  ) |> 
  cols_align(align = "center", columns = estimate) |> 
  cols_align(align = "center", columns = plays) |>
  tab_options(heading.title.font.size = px(24)) |> 
  tab_style(
    style = cell_text(weight = "bold"),   # Apply bold style to column headers
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(20)),
    locations = cells_body(columns = player_name)
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(28)), 
    locations = cells_title()
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(20)), 
    locations = cells_column_labels()
  )

not_slimy_def <- defender_ghost_coef |> 
  mutate(level = as.numeric(level)) |> 
  inner_join(ghost_defender_totals, by = join_by(level == defender_nfl_id)) |> 
  inner_join(player_names, by = join_by(level == nfl_id)) |> 
  inner_join(readr_players |> select(player = display_name, nfl_id, gsis_id, position) |> 
               mutate(nfl_id = as.numeric(nfl_id)),
             by = join_by(level == nfl_id)) |> 
  #filter(!(position %in% c("LB", "MLB", "OLB"))) |> 
  #filter(position == "CB") |> 
  select(player_name, gsis_id, plays, estimate) |> 
  arrange(estimate) |> 
  slice_tail(n = 5) |> 
  gt() |> 
  gtExtras::gt_theme_538() |> 
  #gt_nfl_logos("team", height = 40) |> 
  gt_nfl_headshots("gsis_id", height = 40) |> 
  data_color(columns = estimate, palette = "RdYlGn", domain = c(-0.05, 0.04),
             reverse = TRUE) |> 
  fmt_number(columns = estimate, decimals = 3) |> 
  cols_label(gsis_id = "", estimate = "SLIME", player_name = "Player") |> 
  tab_header(title = md("**Least Slimy Defenders, 2023**")) |> 
  tab_style(
    style = cell_text(font = "PT Sans", weight = "bold", size = px(20)),   
    locations = cells_body(columns = estimate) 
  ) |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = player_name)
  ) |> 
  cols_align(align = "center", columns = estimate) |> 
  tab_options(heading.title.font.size = px(24)) |> 
  tab_style(
    style = cell_text(weight = "bold"),   # Apply bold style to column headers
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(20)),
    locations = cells_body(columns = player_name)
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(28)), 
    locations = cells_title()
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(20)), 
    locations = cells_column_labels()
  )

gt_two_column_layout(tables = list(slimy_def, not_slimy_def), vwidth = 800,
                     output = "save", filename = "slimy_def.png")

slimy_off <- receiver_ghost_coef |> 
  mutate(level = as.numeric(level)) |> 
  inner_join(receiver_ghost_totals, by = join_by(level == receiver_nfl_id)) |> 
  inner_join(player_names, by = join_by(level == nfl_id)) |> 
  inner_join(readr_players |> select(player = display_name, nfl_id, gsis_id, position) |> 
               mutate(nfl_id = as.numeric(nfl_id)),
             by = join_by(level == nfl_id)) |> 
  filter(position == "WR") |> 
  select(player_name, gsis_id, plays, estimate) |> 
  arrange(estimate) |> 
  slice_head(n = 5) |> 
  gt() |> 
  gtExtras::gt_theme_538() |> 
  #gt_nfl_logos("team", height = 40) |> 
  gt_nfl_headshots("gsis_id", height = 40) |> 
  data_color(columns = estimate, palette = "RdYlGn", domain = c(-0.09, 0.055),
             reverse = FALSE) |> 
  fmt_number(columns = estimate, decimals = 3) |> 
  cols_label(gsis_id = "", estimate = "SLIME", player_name = "Player") |> 
  tab_header(title = md("**Slimiest Defenders, 2023**")) |> 
  tab_style(
    style = cell_text(font = "PT Sans", weight = "bold", size = px(20)),   
    locations = cells_body(columns = estimate) 
  ) |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = player_name)
  ) |> 
  cols_align(align = "center", columns = estimate) |> 
  cols_align(align = "center", columns = plays) |>
  tab_options(heading.title.font.size = px(24)) |> 
  tab_style(
    style = cell_text(weight = "bold"),   # Apply bold style to column headers
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(20)),
    locations = cells_body(columns = player_name)
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(28)), 
    locations = cells_title()
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(20)), 
    locations = cells_column_labels()
  )

not_slimy_off <- receiver_ghost_coef |> 
  mutate(level = as.numeric(level)) |> 
  inner_join(receiver_ghost_totals, by = join_by(level == receiver_nfl_id)) |> 
  inner_join(player_names, by = join_by(level == nfl_id)) |> 
  inner_join(readr_players |> select(player = display_name, nfl_id, gsis_id, position) |> 
               mutate(nfl_id = as.numeric(nfl_id)),
             by = join_by(level == nfl_id)) |> 
  #filter(position == "WR") |> 
  select(player_name, gsis_id, plays, estimate) |> 
  arrange(estimate) |> 
  slice_tail(n = 5) |> 
  gt() |> 
  gtExtras::gt_theme_538() |> 
  #gt_nfl_logos("team", height = 40) |> 
  gt_nfl_headshots("gsis_id", height = 40) |> 
  data_color(columns = estimate, palette = "RdYlGn", domain = c(-0.09, 0.055),
             reverse = FALSE) |> 
  fmt_number(columns = estimate, decimals = 3) |> 
  cols_label(gsis_id = "", estimate = "SLIME", player_name = "Player") |> 
  tab_header(title = md("**Least Slimy Defenders, 2023**")) |> 
  tab_style(
    style = cell_text(font = "PT Sans", weight = "bold", size = px(20)),   
    locations = cells_body(columns = estimate) 
  ) |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = player_name)
  ) |> 
  cols_align(align = "center", columns = estimate) |> 
  tab_options(heading.title.font.size = px(24)) |> 
  tab_style(
    style = cell_text(weight = "bold"),   # Apply bold style to column headers
    locations = cells_column_labels()
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(20)),
    locations = cells_body(columns = player_name)
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(28)), 
    locations = cells_title()
  ) |> 
  tab_style(
    style = cell_text(font = "PT Sans", size = px(20)), 
    locations = cells_column_labels()
  )