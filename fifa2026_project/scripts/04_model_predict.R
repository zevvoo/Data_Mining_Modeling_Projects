######################################
## FIFA Ranking Data Mining        ##
## Implemented by Jewoo Yoo         ##
## 2026-06-04                       ##
######################################

args <- commandArgs(FALSE)
file_arg <- args[grepl("^--file=", args)]
base_dir <- if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()
data_dir <- file.path(base_dir, "data")

library(MASS)
library(rpart)
library(nnet)

set.seed(2026)

train <- read.csv(file.path(data_dir, "train_features_1994_2018.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
test <- read.csv(file.path(data_dir, "test_features_2022.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
predict_2026 <- read.csv(file.path(data_dir, "predict_features_2026.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")

feature_cols <- c(
  "pre_wc_rank", "pre_wc_points",
  "rank_change_1year", "points_change_1year",
  "rank_change_4year", "points_change_4year",
  "avg_rank_recent", "sd_rank_recent", "avg_points_recent"
)

formula_text <- paste("finish_rank ~", paste(feature_cols, collapse = " + "))
model_formula <- as.formula(formula_text)

mae <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
spearman <- function(actual, pred) suppressWarnings(cor(actual, pred, method = "spearman", use = "complete.obs"))

evaluate_model <- function(name, pred) {
  data.frame(
    model = name,
    MAE = mae(test$finish_rank, pred),
    RMSE = rmse(test$finish_rank, pred),
    Spearman = spearman(test$finish_rank, pred),
    stringsAsFactors = FALSE
  )
}

lm_model <- lm(model_formula, data = train)
lm_pred_test <- predict(lm_model, newdata = test)

rlm_model <- rlm(model_formula, data = train, maxit = 100)
rlm_pred_test <- predict(rlm_model, newdata = test)

tree_model <- rpart(
  model_formula,
  data = train,
  method = "anova",
  control = rpart.control(cp = 0.01, minsplit = 12, xval = 7)
)
tree_pred_test <- predict(tree_model, newdata = test)

scale_center <- sapply(train[, feature_cols], mean)
scale_scale <- sapply(train[, feature_cols], sd)
scale_scale[scale_scale == 0] <- 1

scale_features <- function(df) {
  x <- sweep(df[, feature_cols], 2, scale_center, "-")
  sweep(x, 2, scale_scale, "/")
}

nn_train_x <- scale_features(train)
nn_test_x <- scale_features(test)
nn_predict_x <- scale_features(predict_2026)

nn_train <- data.frame(finish_rank = train$finish_rank, nn_train_x)
nn_test <- data.frame(nn_test_x)
nn_predict <- data.frame(nn_predict_x)
nn_formula <- as.formula(paste("finish_rank ~", paste(colnames(nn_train_x), collapse = " + ")))

nn_model <- nnet(
  nn_formula,
  data = nn_train,
  size = 4,
  linout = TRUE,
  decay = 0.05,
  maxit = 1000,
  trace = FALSE
)
nn_pred_test <- as.numeric(predict(nn_model, newdata = nn_test))

baseline_pred_test <- test$pre_wc_rank / max(test$pre_wc_rank) * max(test$finish_rank)

metrics <- do.call(rbind, list(
  evaluate_model("FIFA-rank scaled baseline", baseline_pred_test),
  evaluate_model("Linear regression", lm_pred_test),
  evaluate_model("Robust regression", rlm_pred_test),
  evaluate_model("Regression tree", tree_pred_test),
  evaluate_model("Neural network", nn_pred_test)
))

metrics <- metrics[order(metrics$RMSE, -metrics$Spearman, metrics$MAE), ]
best_model <- metrics$model[1]

prediction_dispatch <- list(
  "FIFA-rank scaled baseline" = predict_2026$pre_wc_rank / max(predict_2026$pre_wc_rank) * 48,
  "Linear regression" = predict(lm_model, newdata = predict_2026),
  "Robust regression" = predict(rlm_model, newdata = predict_2026),
  "Regression tree" = predict(tree_model, newdata = predict_2026),
  "Neural network" = as.numeric(predict(nn_model, newdata = nn_predict))
)

predict_2026$predicted_score <- as.numeric(prediction_dispatch[[best_model]])
predict_2026 <- predict_2026[order(predict_2026$predicted_score, predict_2026$pre_wc_rank), ]
predict_2026$predicted_finish_rank <- seq_len(nrow(predict_2026))

final_predictions <- predict_2026[, c(
  "predicted_finish_rank", "country", "predicted_score",
  "pre_wc_rank", "pre_wc_points", "rank_change_1year",
  "points_change_1year", "rank_change_4year", "points_change_4year",
  "host", "avg_rank_recent", "best_rank_recent", "worst_rank_recent"
)]

test_predictions <- data.frame(
  tournament_year = test$tournament_year,
  country = test$country,
  actual_finish_rank = test$finish_rank,
  pre_wc_rank = test$pre_wc_rank,
  baseline_predicted_score = baseline_pred_test,
  lm_predicted_score = lm_pred_test,
  rlm_predicted_score = rlm_pred_test,
  tree_predicted_score = tree_pred_test,
  nn_predicted_score = nn_pred_test,
  stringsAsFactors = FALSE
)
test_predictions$best_model_predicted_score <- test_predictions[[switch(
  best_model,
  "FIFA-rank scaled baseline" = "baseline_predicted_score",
  "Linear regression" = "lm_predicted_score",
  "Robust regression" = "rlm_predicted_score",
  "Regression tree" = "tree_predicted_score",
  "Neural network" = "nn_predicted_score"
)]]
test_predictions <- test_predictions[order(test_predictions$actual_finish_rank), ]

write.csv(metrics, file.path(data_dir, "model_metrics_2022_test.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(test_predictions, file.path(data_dir, "test_predictions_2022.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(final_predictions, file.path(data_dir, "final_prediction_2026_rankings.csv"), row.names = FALSE, fileEncoding = "UTF-8")

summary_lines <- c(
  "FIFA World Cup 2026 modeling summary",
  sprintf("Train rows: %s", nrow(train)),
  sprintf("Test rows: %s", nrow(test)),
  sprintf("Prediction rows: %s", nrow(final_predictions)),
  sprintf("Compared models: %s", paste(metrics$model, collapse = ", ")),
  sprintf("Best model by 2022 RMSE: %s", best_model),
  sprintf("Best model RMSE: %.3f", metrics$RMSE[1]),
  sprintf("Best model MAE: %.3f", metrics$MAE[1]),
  sprintf("Best model Spearman correlation: %.3f", metrics$Spearman[1]),
  "Final 2026 ranking is obtained by sorting the best model's predicted finish score from lowest to highest."
)

writeLines(summary_lines, file.path(data_dir, "modeling_summary.txt"), useBytes = TRUE)
cat(paste(summary_lines, collapse = "\n"), "\n")
cat("\nTop 10 predicted teams:\n")
print(final_predictions[1:10, c("predicted_finish_rank", "country", "predicted_score", "pre_wc_rank")], row.names = FALSE)
View(final_predictions)
