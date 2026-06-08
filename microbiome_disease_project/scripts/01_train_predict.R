######################################
## Microbiome Disease Prediction   ##
## Implemented by Jewoo Yoo         ##
## 2026-06-08                       ##
######################################

library(data.table)
library(nnet)
library(class)

set.seed(2026)

find_project_dir <- function() {
  wd <- normalizePath(getwd())
  candidates <- unique(c(
    wd,
    dirname(wd),
    file.path(wd, "microbiome_disease_project"),
    file.path(dirname(wd), "microbiome_disease_project")
  ))

  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "scripts", "01_train_predict.R"))) {
      return(candidate)
    }
  }

  stop("Project directory not found. Run this script from the repository, project, or scripts folder.")
}

base_dir <- find_project_dir()
source_dir <- file.path(base_dir, "input_data")
data_dir <- file.path(base_dir, "data")
model_dir <- file.path(base_dir, "model")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

train_path <- file.path(source_dir, "Q2_train.tsv")
test_path <- file.path(source_dir, "Q2_test.tsv")

prediction_csv <- file.path(data_dir, "Q2_test_predictions_R.csv")
metrics_csv <- file.path(data_dir, "model_metrics_cv_R.csv")
summary_path <- file.path(data_dir, "modeling_summary_R.txt")
answer_output <- file.path(base_dir, "Q2_AnswerSheet_filled_R.xlsx")
model_output <- file.path(model_dir, "microbiome_disease_model_R.rds")

top_k <- 200
cv_folds <- 5

read_q2_data <- function() {
  train <- fread(train_path, data.table = FALSE)
  test <- fread(test_path, data.table = FALSE)
  feature_cols <- setdiff(names(train), "Disease")

  if (!identical(names(test)[-1], feature_cols)) {
    stop("Train and test feature columns do not match.")
  }
  if (anyNA(train) || anyNA(test)) {
    stop("Missing values detected. Please impute or remove missing values first.")
  }

  list(train = train, test = test, feature_cols = feature_cols)
}

make_stratified_folds <- function(y, k = 5) {
  folds <- vector("list", k)
  for (level in levels(y)) {
    idx <- which(y == level)
    idx <- sample(idx)
    parts <- split(idx, rep(seq_len(k), length.out = length(idx)))
    for (i in seq_len(k)) folds[[i]] <- c(folds[[i]], parts[[i]])
  }
  lapply(folds, sort)
}

select_features <- function(x, y, k = 500) {
  score_one <- function(v) {
    groups <- split(v, y)
    overall <- mean(v)
    between <- sum(vapply(groups, length, integer(1)) * (vapply(groups, mean, numeric(1)) - overall)^2)
    within <- sum(vapply(groups, function(g) sum((g - mean(g))^2), numeric(1)))
    if (within == 0) return(0)
    between / within
  }

  variances <- apply(x, 2, var)
  kept <- which(variances > 1e-12)
  x_kept <- x[, kept, drop = FALSE]
  scores <- apply(x_kept, 2, score_one)
  selected_local <- order(scores, decreasing = TRUE)[seq_len(min(k, length(scores)))]
  colnames(x_kept)[selected_local]
}

fit_preprocessor <- function(x, y, selected_features = NULL) {
  if (is.null(selected_features)) {
    selected_features <- select_features(x, y, top_k)
  }
  x_selected <- x[, selected_features, drop = FALSE]
  center <- colMeans(x_selected)
  scale <- apply(x_selected, 2, sd)
  scale[scale == 0] <- 1
  list(features = selected_features, center = center, scale = scale)
}

transform_features <- function(x, prep) {
  x_selected <- x[, prep$features, drop = FALSE]
  sweep(sweep(x_selected, 2, prep$center, "-"), 2, prep$scale, "/")
}

fit_models <- function(x_train_scaled, y_train) {
  train_df <- data.frame(Disease = y_train, x_train_scaled, check.names = FALSE)

  multinom_model <- multinom(
    Disease ~ .,
    data = train_df,
    maxit = 250,
    MaxNWts = 20000,
    trace = FALSE
  )

  list(multinom = multinom_model, knn_y = y_train, knn_x = x_train_scaled)
}

predict_models <- function(models, x_scaled) {
  p_multinom <- predict(models$multinom, newdata = data.frame(x_scaled, check.names = FALSE))
  p_knn <- knn(train = models$knn_x, test = x_scaled, cl = models$knn_y, k = 5)

  votes <- data.frame(
    multinom = as.character(p_multinom),
    knn = as.character(p_knn),
    stringsAsFactors = FALSE
  )

  apply(votes, 1, function(row) {
    counts <- sort(table(row), decreasing = TRUE)
    names(counts)[1]
  })
}

evaluate_predictions <- function(actual, pred) {
  accuracy <- mean(actual == pred)
  class_acc <- tapply(actual == pred, actual, mean)
  balanced_accuracy <- mean(class_acc)
  c(accuracy = accuracy, balanced_accuracy = balanced_accuracy)
}

run_cross_validation <- function(x, y) {
  folds <- make_stratified_folds(y, cv_folds)
  rows <- list()

  for (i in seq_along(folds)) {
    valid_idx <- folds[[i]]
    train_idx <- setdiff(seq_along(y), valid_idx)

    x_train <- x[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    x_valid <- x[valid_idx, , drop = FALSE]
    y_valid <- y[valid_idx]

    prep <- fit_preprocessor(x_train, y_train)
    x_train_scaled <- transform_features(x_train, prep)
    x_valid_scaled <- transform_features(x_valid, prep)

    models <- fit_models(x_train_scaled, y_train)
    pred <- predict_models(models, x_valid_scaled)
    metrics <- evaluate_predictions(y_valid, pred)

    rows[[i]] <- data.frame(
      fold = i,
      accuracy = metrics["accuracy"],
      balanced_accuracy = metrics["balanced_accuracy"]
    )
    cat(sprintf("Fold %d accuracy: %.4f, balanced accuracy: %.4f\n", i, metrics["accuracy"], metrics["balanced_accuracy"]))
  }

  do.call(rbind, rows)
}

escape_xml <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

write_simple_xlsx <- function(df, path) {
  tmp <- tempfile("xlsx_build_")
  dir.create(tmp)
  dir.create(file.path(tmp, "_rels"), recursive = TRUE)
  dir.create(file.path(tmp, "xl", "_rels"), recursive = TRUE)
  dir.create(file.path(tmp, "xl", "worksheets"), recursive = TRUE)

  writeLines(
    c(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
      '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
      '<Default Extension="xml" ContentType="application/xml"/>',
      '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
      '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
      '</Types>'
    ),
    file.path(tmp, "[Content_Types].xml")
  )

  writeLines(
    c(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
      '</Relationships>'
    ),
    file.path(tmp, "_rels", ".rels")
  )

  writeLines(
    c(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
      '<sheets><sheet name="Sheet1" sheetId="1" r:id="rId1"/></sheets>',
      '</workbook>'
    ),
    file.path(tmp, "xl", "workbook.xml")
  )

  writeLines(
    c(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>',
      '</Relationships>'
    ),
    file.path(tmp, "xl", "_rels", "workbook.xml.rels")
  )

  col_letter <- function(n) {
    result <- ""
    while (n > 0) {
      n <- n - 1
      result <- paste0(intToUtf8(65 + (n %% 26)), result)
      n <- floor(n / 26)
    }
    result
  }

  rows <- c(list(names(df)), lapply(seq_len(nrow(df)), function(i) as.character(unlist(df[i, ], use.names = FALSE))))
  row_xml <- character(length(rows))
  for (r in seq_along(rows)) {
    values <- unlist(rows[[r]], use.names = FALSE)
    cell_xml <- paste0(vapply(seq_along(values), function(c) {
      ref <- paste0(col_letter(c), r)
      paste0('<c r="', ref, '" t="inlineStr"><is><t>', escape_xml(values[c]), '</t></is></c>')
    }, character(1)), collapse = "")
    row_xml[r] <- paste0('<row r="', r, '">', cell_xml, '</row>')
  }

  dimension_ref <- paste0("A1:", col_letter(ncol(df)), length(rows))

  sheet_xml <- c(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
    paste0('<dimension ref="', dimension_ref, '"/>'),
    '<sheetData>',
    row_xml,
    '</sheetData>',
    '</worksheet>'
  )
  writeLines(sheet_xml, file.path(tmp, "xl", "worksheets", "sheet1.xml"))

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(tmp)
  if (file.exists(path)) file.remove(path)
  zip(path, files = list.files(".", recursive = TRUE), flags = "-q")
}

data <- read_q2_data()
train <- data$train
test <- data$test
feature_cols <- data$feature_cols

x <- as.matrix(train[, feature_cols])
y <- factor(train$Disease)
x_test <- as.matrix(test[, feature_cols])

cv_results <- run_cross_validation(x, y)
metrics_summary <- data.frame(
  model = "R ensemble: multinom + kNN",
  accuracy_mean = mean(cv_results$accuracy),
  accuracy_sd = sd(cv_results$accuracy),
  balanced_accuracy_mean = mean(cv_results$balanced_accuracy),
  balanced_accuracy_sd = sd(cv_results$balanced_accuracy)
)
fwrite(metrics_summary, metrics_csv)

prep <- fit_preprocessor(x, y)
x_scaled <- transform_features(x, prep)
x_test_scaled <- transform_features(x_test, prep)
final_models <- fit_models(x_scaled, y)
test_pred <- predict_models(final_models, x_test_scaled)

prediction_df <- data.frame(
  ID = test$ID,
  Prediction = test_pred,
  stringsAsFactors = FALSE
)
fwrite(prediction_df, prediction_csv)
write_simple_xlsx(prediction_df, answer_output)

bundle <- list(
  preprocessor = prep,
  models = final_models,
  feature_cols = feature_cols,
  classes = levels(y)
)
saveRDS(bundle, model_output)

summary_lines <- c(
  "Microbiome disease prediction modeling summary",
  sprintf("Train rows: %s", nrow(train)),
  sprintf("Test rows: %s", nrow(test)),
  sprintf("Features: %s", length(feature_cols)),
  sprintf("Classes: %s", length(levels(y))),
  "Selected model: R ensemble of multinomial logistic regression and kNN",
  sprintf("5-fold CV accuracy: %.4f", metrics_summary$accuracy_mean),
  sprintf("5-fold CV balanced accuracy: %.4f", metrics_summary$balanced_accuracy_mean),
  sprintf("Answer sheet written: %s", answer_output),
  sprintf("Model file written: %s", model_output)
)
writeLines(summary_lines, summary_path)
cat(paste(summary_lines, collapse = "\n"), "\n")
