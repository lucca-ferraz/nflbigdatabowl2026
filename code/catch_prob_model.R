library(xgboost)
library(caret)
library(fastDummies)

catch_prob_data <- defender_start |> 
  inner_join(receiver_start) |> 
  inner_join(receiver_arrivals) |> 
  inner_join(qb_start) |> 
  mutate(qb_x_change = receiver_arrival_x - qb_x, 
         qb_y_change_absval = abs(receiver_arrival_y - qb_y),
         qb_dist_from_receiver = 
           sqrt((qb_x - receiver_arrival_x)^2 + (qb_y - receiver_arrival_y)^2)) |> 
  select(-qb_x, -qb_y) |> 
  inner_join(sup |> select(game_id, play_id, week, route_of_targeted_receiver, 
                           team_coverage_type, adj_x_first_down, pass_result)) |> 
  mutate(receiver_x_from_first_down = receiver_arrival_x - adj_x_first_down + 10) |> 
  select(-adj_x_first_down) |> 
  mutate(start_x_change_from_receiver = receiver_start_x - defender_start_x,
         start_y_change_from_receiver = receiver_start_y - defender_start_y,
         start_dist_from_receiver = 
           sqrt((defender_start_x - receiver_start_x)^2 + (defender_start_y - receiver_start_y)^2)) |> 
  inner_join(defender_arrivals) |> 
  filter(defender_position != "DT" & defender_position != "NT") |> 
  filter(defender_position != "FB") |> 
  filter(!is.na(route_of_targeted_receiver)) |> 
  #filter(team_coverage_type != "PREVENT") |> 
  filter(!is.na(team_coverage_type)) |> 
  mutate(game_play_nfl_id = paste(game_id, play_id, nfl_id)) |> 
  filter(game_play_nfl_id %in% closest_defender) |> 
  select(-game_play_nfl_id) |> 
  mutate(completion = ifelse(pass_result == "C", 1, 0)) |> 
  select(-pass_result) |> 
  mutate(end_dist_from_receiver = 
           sqrt((defender_arrival_x - receiver_arrival_x)^2 + (defender_arrival_y - receiver_arrival_y)^2),
         end_x_from_receiver = defender_arrival_x - receiver_arrival_x,
         end_y_from_receiver = defender_arrival_y - receiver_arrival_y,
         receiver_dist_from_ball = 
           sqrt((receiver_arrival_x - ball_land_x)^2 + (receiver_arrival_y - ball_land_y)^2),
         receiver_x_from_ball = receiver_arrival_x - ball_land_x,
         receiver_y_from_ball = receiver_arrival_y - ball_land_y,
         def_dist_from_ball = 
           sqrt((defender_arrival_x - ball_land_x)^2 + (defender_arrival_y - ball_land_y)^2),
         defender_x_from_ball = defender_arrival_x - ball_land_x,
         defender_y_from_ball = defender_arrival_y - ball_land_y)

full_mat <- model.matrix(completion ~ defender_start_s + defender_start_a + defender_start_o
                         #+ defender_position + route_of_targeted_receiver + team_coverage_type + play_action
                         + receiver_start_s + receiver_start_a 
                         + receiver_start_dir + receiver_start_o + frames + qb_s
                         + qb_x_change + qb_y_change_absval + qb_dist_from_receiver 
                         + receiver_x_from_first_down
                         + start_x_change_from_receiver + start_y_change_from_receiver
                         + start_dist_from_receiver + end_dist_from_receiver 
                         + end_x_from_receiver + end_y_from_receiver + receiver_dist_from_ball
                         + receiver_x_from_ball + receiver_y_from_ball + def_dist_from_ball
                         + defender_x_from_ball + defender_y_from_ball, data = catch_prob_data)

full_xg_tune <- train(x = full_mat,
                      y = catch_prob_data |> pull(completion) |> as.factor(),
                      tuneGrid = xg_grid,
                      trControl = trainControl(method = "cv", number = 5),
                      method = "xgbTree",
                      verbosity = 0)

lowo_cv_catch_prob <- tibble()
for (i in 1:18){
  train <- catch_prob_data |> filter(week != i)
  test <- catch_prob_data |> filter(week == i)
  train_mat <- model.matrix(completion ~ defender_start_s + defender_start_a + defender_start_o
                            #+ defender_position + route_of_targeted_receiver + team_coverage_type + play_action
                            + receiver_start_s + receiver_start_a 
                            + receiver_start_dir + receiver_start_o + frames + qb_s
                            + qb_x_change + qb_y_change_absval + qb_dist_from_receiver 
                            + receiver_x_from_first_down
                            + start_x_change_from_receiver + start_y_change_from_receiver
                            + start_dist_from_receiver + end_dist_from_receiver 
                            + end_x_from_receiver + end_y_from_receiver + receiver_dist_from_ball
                            + receiver_x_from_ball + receiver_y_from_ball + def_dist_from_ball
                            + defender_x_from_ball + defender_y_from_ball, data = train)
  test_mat <- model.matrix(completion ~ defender_start_s + defender_start_a + defender_start_o
                           #+ defender_position + route_of_targeted_receiver + team_coverage_type + play_action
                           + receiver_start_s + receiver_start_a 
                           + receiver_start_dir + receiver_start_o + frames + qb_s
                           + qb_x_change + qb_y_change_absval + qb_dist_from_receiver 
                           + receiver_x_from_first_down
                           + start_x_change_from_receiver + start_y_change_from_receiver
                           + start_dist_from_receiver + end_dist_from_receiver 
                           + end_x_from_receiver + end_y_from_receiver + receiver_dist_from_ball
                           + receiver_x_from_ball + receiver_y_from_ball + def_dist_from_ball
                           + defender_x_from_ball + defender_y_from_ball, data = test)
  
  xg_model <- xgboost(data = train_mat,
                      label = train |> pull(completion) |> as.numeric(),
                      nrounds = full_xg_tune$bestTune$nrounds,
                      params = as.list(select(full_xg_tune$bestTune, -nrounds)),
                      objective = "binary:logistic",
                      verbose = 0)
  
  baseline_model <- lm(completion ~ def_dist_from_ball + receiver_dist_from_ball
                       + end_dist_from_receiver, data = train)
  baseline_preds <- predict(baseline_model, test, type = "response")
  baseline_rmse <- mean((test$completion - baseline_preds)^2)
  library(glmnet)
  x_train <- model.matrix(
    completion ~ defender_start_s + defender_start_a + defender_start_o
    #+ defender_position + route_of_targeted_receiver + team_coverage_type + play_action
    + receiver_start_s + receiver_start_a 
    + receiver_start_dir + receiver_start_o + frames + qb_s
    + qb_x_change + qb_y_change_absval + qb_dist_from_receiver 
    + receiver_x_from_first_down
    + start_x_change_from_receiver + start_y_change_from_receiver
    + start_dist_from_receiver + end_dist_from_receiver 
    + end_x_from_receiver + end_y_from_receiver + receiver_dist_from_ball
    + receiver_x_from_ball + receiver_y_from_ball + def_dist_from_ball
    + defender_x_from_ball + defender_y_from_ball, data = train
  )[, -1]   # drop intercept
  
  y_train <- train$completion
  
  x_test <- model.matrix(
    completion ~ defender_start_s + defender_start_a + defender_start_o
    #+ defender_position + route_of_targeted_receiver + team_coverage_type + play_action
    + receiver_start_s + receiver_start_a 
    + receiver_start_dir + receiver_start_o + frames + qb_s
    + qb_x_change + qb_y_change_absval + qb_dist_from_receiver 
    + receiver_x_from_first_down
    + start_x_change_from_receiver + start_y_change_from_receiver
    + start_dist_from_receiver + end_dist_from_receiver 
    + end_x_from_receiver + end_y_from_receiver + receiver_dist_from_ball
    + receiver_x_from_ball + receiver_y_from_ball + def_dist_from_ball
    + defender_x_from_ball + defender_y_from_ball, data = test
  )[, -1]
  cv_lasso <- cv.glmnet(
    x_train,
    y_train,
    family = "binomial",
    alpha = 1,
    nfolds = 5
  )
  lasso_test_preds <- predict(
    cv_lasso,
    newx = x_test,
    s = "lambda.min",
    type = "response"
  )
  lasso_rmse <- mean((test$completion - lasso_test_preds)^2)
  # 
  # log_model <- lm(completion ~ defender_start_s + defender_start_a + defender_start_o
  #                 + defender_position + play_action + receiver_start_s + receiver_start_a 
  #                 + receiver_start_dir + receiver_start_o + frames + qb_s
  #                 + qb_x_change + qb_y_change_absval + qb_dist_from_receiver 
  #                 + route_of_targeted_receiver + team_coverage_type + receiver_x_from_first_down
  #                 + start_x_change_from_receiver + start_y_change_from_receiver
  #                 + start_dist_from_receiver + end_dist_from_receiver 
  #                 + end_x_from_receiver + end_y_from_receiver + receiver_dist_from_ball
  #                 + receiver_x_from_ball + receiver_y_from_ball + def_dist_from_ball
  #                 + defender_x_from_ball + defender_y_from_ball, data = train)
  # # summary(log_model)
  # log_test_preds <- predict(log_model, test, type = "response")
  # log_rmse <- mean((test$completion - log_test_preds)^2)
  
  library(ranger)
  rf_model <- ranger(completion ~ defender_start_s + defender_start_a + defender_start_o
                     + defender_position + play_action + receiver_start_s + receiver_start_a 
                     + receiver_start_dir + receiver_start_o + frames + qb_s
                     + qb_x_change + qb_y_change_absval + qb_dist_from_receiver 
                     + route_of_targeted_receiver + team_coverage_type + receiver_x_from_first_down
                     + start_x_change_from_receiver + start_y_change_from_receiver
                     + start_dist_from_receiver + end_dist_from_receiver 
                     + end_x_from_receiver + end_y_from_receiver + receiver_dist_from_ball
                     + receiver_x_from_ball + receiver_y_from_ball + def_dist_from_ball
                     + defender_x_from_ball + defender_y_from_ball, data = train)
  rf_test_preds <- predict(rf_model, test, type = "response")$predictions
  rf_rmse <- mean((test$completion - rf_test_preds)^2) #0.1317264
  
  xg_test_preds <- predict(xg_model, test_mat, type = "response")
  xg_rmse <- mean((test$completion - xg_test_preds)^2)
  rmses <- tibble(baseline = baseline_rmse, lasso = lasso_rmse,
                  random_forest = rf_rmse, xgboost = xg_rmse)
  lowo_cv_catch_prob <- bind_rows(lowo_cv_catch_prob, rmses)
}
lowo_cv_catch_prob <- lowo_cv_catch_prob |> 
  mutate(week = row_number())

lowo_long <- lowo_cv_catch_prob |>
  pivot_longer(
    cols = -week,
    names_to = "model",
    values_to = "brier"
  )

# ggplot(lowo_long, aes(x = model, y = brier, fill = model)) +
#   geom_boxplot(alpha = 0.7, outlier.shape = NA) +
#   geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
#   labs(
#     title = "Model Performance Under Leave-One-Week-Out CV",
#     x = "",
#     y = "Brier Score (lower is better)"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")

#devtools::install_github("johannesbjork/LaCroixColoR")
library(LaCroixColoR)
library(wesanderson)
ggplot(lowo_long, aes(x = week, y = brier, color = model)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "XGBoost Performs Best Using LOWO-CV",
    subtitle = "Leave-One-Week-Out Brier Score by Model",
    x = "Held-Out Week",
    y = "Brier Score (lower is better)",
    color = "Model"
  ) +
  ggthemes::theme_clean() +
  theme(
    plot.title = ggtext::element_markdown(size = 15, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 10, face = "italic"),
    axis.title = ggtext::element_markdown(size = 13, face = "bold"),
    axis.text = ggtext::element_markdown(size = 12),
    text = element_text(family = "PT Sans")
  ) +
  scale_color_manual(
    labels = c("Baseline", "Lasso", "Random Forest", "XGBoost"),
    values = wes_palette("Darjeeling1", n = 4, type = "discrete")
  )

full_xg_model <- xgboost(data = full_mat,
                         label = catch_prob_data |> pull(completion) |> as.numeric(),
                         nrounds = full_xg_tune$bestTune$nrounds,
                         params = as.list(select(full_xg_tune$bestTune, -nrounds)),
                         objective = "binary:logistic",
                         verbose = 0)
all_preds <- predict(full_xg_model, full_mat, type = "response")
mean((catch_prob_data$completion - all_preds)^2) #0.09129707

library(vip)
vip(full_xg_model, num_features = 50)

targeted_receiver <- input |> 
  filter(player_role == "Targeted Receiver") |> 
  group_by(game_id, play_id) |> 
  summarise(receiver_nfl_id = first(nfl_id))

catch_prob_data <- catch_prob_data |> 
  mutate(prob_complete = all_preds) |> 
  rename(defender_nfl_id = nfl_id) |> 
  inner_join(targeted_receiver)

catch_prob_data <- catch_prob_data |> 
  mutate(resid_completion = completion - prob_complete)

library(lme4)
library(broom.mixed)
catch_point_mixed_effects <- lmer(resid_completion ~ (1 | defender_nfl_id) + (1 | receiver_nfl_id),
                                  data = catch_prob_data)
summary(catch_point_mixed_effects)
ranef(catch_point_mixed_effects)
tidy_coef <- tidy(catch_point_mixed_effects, effects = "ran_vals") 
defender_coef <- tidy_coef |> 
  filter(group == "defender_nfl_id")
receiver_coef <- tidy_coef |> 
  filter(group == "receiver_nfl_id")
