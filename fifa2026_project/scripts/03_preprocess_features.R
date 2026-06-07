######################################
## FIFA Ranking Data Mining        ##
## Implemented by Jewoo Yoo         ##
## 2026-06-04                       ##
######################################

args <- commandArgs(FALSE)
file_arg <- args[grepl("^--file=", args)]
base_dir <- if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()

ranking_path <- Sys.getenv(
  "FIFA_RANKING_TSV",
  file.path(base_dir, "input_data", "fifa_rankings_men.tsv")
)
data_dir <- file.path(base_dir, "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

rankings <- read.delim(
  ranking_path,
  sep = "\t",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

month_lookup <- c(
  January = 1, February = 2, March = 3, April = 4, May = 5, June = 6,
  July = 7, August = 8, September = 9, October = 10, November = 11,
  December = 12
)

split_label <- strsplit(rankings$date_label, " ")
rankings$day <- as.integer(vapply(split_label, `[`, character(1), 1))
rankings$month <- as.integer(month_lookup[vapply(split_label, `[`, character(1), 2)])
rankings$ranking_date <- as.Date(sprintf("%04d-%02d-%02d", rankings$year, rankings$month, rankings$day))
rankings <- rankings[order(rankings$country, rankings$ranking_date), ]

make_standings <- function(year, countries) {
  data.frame(
    tournament_year = year,
    country = countries,
    finish_rank = seq_along(countries),
    stringsAsFactors = FALSE
  )
}

final_standings <- do.call(rbind, list(
  make_standings(1994, c(
    "Brazil", "Italy", "Sweden", "Bulgaria", "Germany", "Romania",
    "Netherlands", "Spain", "Nigeria", "Argentina", "Belgium",
    "Saudi Arabia", "Mexico", "USA", "Switzerland", "Republic of Ireland",
    "Norway", "Russia", "Colombia", "Korea Republic", "Bolivia",
    "Cameroon", "Morocco", "Greece"
  )),
  make_standings(1998, c(
    "France", "Brazil", "Croatia", "Netherlands", "Italy", "Argentina",
    "Germany", "Denmark", "England", "Yugoslavia", "Romania", "Nigeria",
    "Mexico", "Paraguay", "Norway", "Chile", "Spain", "Morocco",
    "Belgium", "IR Iran", "Colombia", "Jamaica", "Austria",
    "South Africa", "Cameroon", "Tunisia", "Scotland", "Saudi Arabia",
    "Bulgaria", "Korea Republic", "Japan", "USA"
  )),
  make_standings(2002, c(
    "Brazil", "Germany", "Turkey", "Korea Republic", "Spain", "England",
    "Senegal", "USA", "Japan", "Denmark", "Mexico", "Republic of Ireland",
    "Sweden", "Belgium", "Italy", "Paraguay", "South Africa",
    "Argentina", "Costa Rica", "Cameroon", "Portugal", "Russia",
    "Croatia", "Ecuador", "Poland", "Uruguay", "Nigeria", "France",
    "Tunisia", "Slovenia", "China PR", "Saudi Arabia"
  )),
  make_standings(2006, c(
    "Italy", "France", "Germany", "Portugal", "Brazil", "Argentina",
    "England", "Ukraine", "Spain", "Switzerland", "Netherlands",
    "Ecuador", "Ghana", "Sweden", "Mexico", "Australia",
    "Korea Republic", "Paraguay", "Côte d'Ivoire", "Czech Republic",
    "Poland", "Croatia", "Angola", "Tunisia", "IR Iran", "USA",
    "Trinidad and Tobago", "Saudi Arabia", "Japan", "Togo",
    "Costa Rica", "Serbia and Montenegro"
  )),
  make_standings(2010, c(
    "Spain", "Netherlands", "Germany", "Uruguay", "Argentina", "Brazil",
    "Ghana", "Paraguay", "Japan", "Chile", "Portugal", "USA",
    "England", "Mexico", "Korea Republic", "Slovakia", "Côte d'Ivoire",
    "Slovenia", "Switzerland", "South Africa", "Australia",
    "New Zealand", "Serbia", "Denmark", "Greece", "Italy", "Nigeria",
    "Algeria", "France", "Honduras", "Cameroon", "Korea DPR"
  )),
  make_standings(2014, c(
    "Germany", "Argentina", "Netherlands", "Brazil", "Colombia",
    "Belgium", "France", "Costa Rica", "Chile", "Mexico", "Switzerland",
    "Uruguay", "Greece", "Algeria", "USA", "Nigeria", "Ecuador",
    "Portugal", "Croatia", "Bosnia and Herzegovina", "Côte d'Ivoire",
    "Italy", "Spain", "Russia", "Ghana", "England", "Korea Republic",
    "IR Iran", "Japan", "Australia", "Honduras", "Cameroon"
  )),
  make_standings(2018, c(
    "France", "Croatia", "Belgium", "England", "Uruguay", "Brazil",
    "Sweden", "Russia", "Colombia", "Spain", "Denmark", "Mexico",
    "Portugal", "Switzerland", "Japan", "Argentina", "Senegal",
    "IR Iran", "Korea Republic", "Peru", "Nigeria", "Germany",
    "Serbia", "Tunisia", "Poland", "Saudi Arabia", "Morocco",
    "Iceland", "Costa Rica", "Australia", "Egypt", "Panama"
  )),
  make_standings(2022, c(
    "Argentina", "France", "Croatia", "Morocco", "Netherlands",
    "England", "Brazil", "Portugal", "Japan", "Senegal", "Australia",
    "Switzerland", "Spain", "USA", "Poland", "Korea Republic",
    "Germany", "Ecuador", "Cameroon", "Uruguay", "Tunisia", "Mexico",
    "Belgium", "Ghana", "Saudi Arabia", "IR Iran", "Costa Rica",
    "Denmark", "Serbia", "Wales", "Canada", "Qatar"
  ))
))

tournament_dates <- data.frame(
  tournament_year = c(1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022, 2026),
  start_date = as.Date(c(
    "1994-06-17", "1998-06-10", "2002-05-31", "2006-06-09",
    "2010-06-11", "2014-06-12", "2018-06-14", "2022-11-20",
    "2026-06-11"
  ))
)

host_map <- list(
  `1994` = "USA",
  `1998` = "France",
  `2002` = c("Japan", "Korea Republic"),
  `2006` = "Germany",
  `2010` = "South Africa",
  `2014` = "Brazil",
  `2018` = "Russia",
  `2022` = "Qatar",
  `2026` = c("Canada", "Mexico", "USA")
)

latest_snapshot_before <- function(country, cutoff_date) {
  rows <- rankings[rankings$country == country & rankings$ranking_date <= cutoff_date, ]
  if (nrow(rows) == 0) {
    return(data.frame(
      rank = NA_integer_,
      points = NA_real_,
      ranking_date = as.Date(NA)
    ))
  }
  rows[nrow(rows), c("rank", "points", "ranking_date")]
}

window_summary <- function(country, start_date, end_date) {
  rows <- rankings[
    rankings$country == country &
      rankings$ranking_date > start_date &
      rankings$ranking_date <= end_date,
  ]
  if (nrow(rows) == 0) {
    return(data.frame(
      avg_rank_recent = NA_real_,
      best_rank_recent = NA_integer_,
      worst_rank_recent = NA_integer_,
      sd_rank_recent = NA_real_,
      avg_points_recent = NA_real_,
      ranking_count_recent = 0
    ))
  }
  data.frame(
    avg_rank_recent = mean(rows$rank, na.rm = TRUE),
    best_rank_recent = min(rows$rank, na.rm = TRUE),
    worst_rank_recent = max(rows$rank, na.rm = TRUE),
    sd_rank_recent = ifelse(nrow(rows) > 1, sd(rows$rank, na.rm = TRUE), 0),
    avg_points_recent = mean(rows$points, na.rm = TRUE),
    ranking_count_recent = nrow(rows)
  )
}

build_feature_rows <- function(rows, label_available = TRUE) {
  output <- vector("list", nrow(rows))
  for (i in seq_len(nrow(rows))) {
    yr <- rows$tournament_year[i]
    country <- rows$country[i]
    start_date <- tournament_dates$start_date[tournament_dates$tournament_year == yr]
    cutoff_date <- start_date - 1

    current <- latest_snapshot_before(country, cutoff_date)
    one_year <- latest_snapshot_before(country, cutoff_date - 365)
    four_year <- latest_snapshot_before(country, cutoff_date - 1461)
    recent <- window_summary(country, cutoff_date - 365, cutoff_date)

    has_1year <- !is.na(one_year$rank)
    has_4year <- !is.na(four_year$rank)
    host <- as.integer(country %in% host_map[[as.character(yr)]])

    output[[i]] <- data.frame(
      tournament_year = yr,
      country = country,
      finish_rank = if (label_available) rows$finish_rank[i] else NA_integer_,
      pre_wc_rank = current$rank,
      pre_wc_points = current$points,
      ranking_snapshot_date = current$ranking_date,
      rank_change_1year = ifelse(has_1year, one_year$rank - current$rank, 0),
      points_change_1year = ifelse(has_1year, current$points - one_year$points, 0),
      rank_change_4year = ifelse(has_4year, four_year$rank - current$rank, 0),
      points_change_4year = ifelse(has_4year, current$points - four_year$points, 0),
      has_1year_history = as.integer(has_1year),
      has_4year_history = as.integer(has_4year),
      host = host,
      recent,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, output)
}

participants_2026 <- read.csv(
  file.path(data_dir, "worldcup2026_participants.csv"),
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
predict_rows <- data.frame(
  tournament_year = 2026,
  country = participants_2026$country,
  stringsAsFactors = FALSE
)

missing_label_countries <- setdiff(unique(final_standings$country), unique(rankings$country))
if (length(missing_label_countries) > 0) {
  stop(paste("Historical World Cup countries not found in ranking TSV:", paste(missing_label_countries, collapse = ", ")))
}

model_dataset <- build_feature_rows(final_standings, label_available = TRUE)
predict_dataset_2026 <- build_feature_rows(predict_rows, label_available = FALSE)

model_dataset$split <- ifelse(model_dataset$tournament_year <= 2018, "train", "test")
predict_dataset_2026$split <- "predict_2026"

train_dataset <- model_dataset[model_dataset$split == "train", ]
test_dataset <- model_dataset[model_dataset$split == "test", ]

write.csv(final_standings, file.path(data_dir, "worldcup_final_standings_1994_2022.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(model_dataset, file.path(data_dir, "model_dataset_1994_2022.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(train_dataset, file.path(data_dir, "train_features_1994_2018.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(test_dataset, file.path(data_dir, "test_features_2022.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(predict_dataset_2026, file.path(data_dir, "predict_features_2026.csv"), row.names = FALSE, fileEncoding = "UTF-8")

summary_lines <- c(
  "FIFA World Cup 2026 preprocessing summary",
  sprintf("Historical labeled rows: %s", nrow(model_dataset)),
  sprintf("Train rows: %s tournaments %s-%s", nrow(train_dataset), min(train_dataset$tournament_year), max(train_dataset$tournament_year)),
  sprintf("Test rows: %s tournament %s", nrow(test_dataset), unique(test_dataset$tournament_year)),
  sprintf("Prediction rows: %s tournament 2026", nrow(predict_dataset_2026)),
  sprintf("Missing pre-WC rank rows in labeled data: %s", sum(is.na(model_dataset$pre_wc_rank))),
  sprintf("Missing pre-WC rank rows in 2026 data: %s", sum(is.na(predict_dataset_2026$pre_wc_rank))),
  "Target variable: finish_rank (lower is better)",
  "Split policy: train = 1994-2018 World Cups, test = 2022 World Cup, predict = 2026 qualified teams"
)

writeLines(summary_lines, file.path(data_dir, "preprocessing_summary.txt"), useBytes = TRUE)
cat(paste(summary_lines, collapse = "\n"), "\n")
