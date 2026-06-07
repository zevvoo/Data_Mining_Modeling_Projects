######################################
## FIFA Ranking Data Mining        ##
## Implemented by Jewoo Yoo         ##
## 2026-06-04                       ##
######################################

args <- commandArgs(FALSE)
file_arg <- args[grepl("^--file=", args)]
script_dir <- if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg[1]))) else getwd()

ranking_path <- Sys.getenv(
  "FIFA_RANKING_TSV",
  file.path(script_dir, "input_data", "fifa_rankings_men.tsv")
)
out_dir <- file.path(script_dir, "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

rankings <- read.delim(
  ranking_path,
  sep = "\t",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

participants <- data.frame(
  country = c(
    "Canada", "Mexico", "USA",
    "Australia", "Iraq", "IR Iran", "Japan", "Jordan", "Korea Republic",
    "Qatar", "Saudi Arabia", "Uzbekistan",
    "Algeria", "Cabo Verde", "Congo DR", "Côte d'Ivoire", "Egypt",
    "Ghana", "Morocco", "Senegal", "South Africa", "Tunisia",
    "Curaçao", "Haiti", "Panama",
    "Argentina", "Brazil", "Colombia", "Ecuador", "Paraguay", "Uruguay",
    "New Zealand",
    "Austria", "Belgium", "Bosnia and Herzegovina", "Croatia", "Czechia",
    "England", "France", "Germany", "Netherlands", "Norway", "Portugal",
    "Scotland", "Spain", "Sweden", "Switzerland", "Türkiye"
  ),
  confederation = c(
    rep("Host", 3),
    rep("AFC", 9),
    rep("CAF", 10),
    rep("Concacaf", 3),
    rep("CONMEBOL", 6),
    rep("OFC", 1),
    rep("UEFA", 16)
  ),
  host = c(rep(1, 3), rep(0, 45)),
  stringsAsFactors = FALSE
)

missing_countries <- setdiff(participants$country, unique(rankings$country))
if (length(missing_countries) > 0) {
  stop(paste("Countries not found in FIFA ranking TSV:", paste(missing_countries, collapse = ", ")))
}

month_lookup <- c(
  January = 1, February = 2, March = 3, April = 4, May = 5, June = 6,
  July = 7, August = 8, September = 9, October = 10, November = 11,
  December = 12
)

split_label <- strsplit(rankings$date_label, " ")
rankings$day <- as.integer(vapply(split_label, `[`, character(1), 1))
rankings$month <- as.integer(month_lookup[vapply(split_label, `[`, character(1), 2)])
rankings$ranking_date <- as.Date(sprintf("%04d-%02d-%02d", rankings$year, rankings$month, rankings$day))

participant_history <- merge(rankings, participants, by = "country", all.x = FALSE, all.y = FALSE)
participant_history <- participant_history[order(
  participant_history$country,
  participant_history$ranking_date
), ]

latest_date <- max(rankings$ranking_date, na.rm = TRUE)
latest_rankings <- participant_history[participant_history$ranking_date == latest_date, ]
latest_rankings <- latest_rankings[order(latest_rankings$rank), ]

write.csv(
  participants,
  file.path(out_dir, "worldcup2026_participants.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  participant_history,
  file.path(out_dir, "fifa_rankings_2026_qualified_history.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  latest_rankings,
  file.path(out_dir, "fifa_rankings_2026_latest.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

summary_lines <- c(
  "FIFA World Cup 2026 data mining summary",
  sprintf("Ranking source rows: %s", nrow(rankings)),
  sprintf("Unique FIFA ranking countries: %s", length(unique(rankings$country))),
  sprintf("Ranking period: %s to %s", min(rankings$ranking_date), max(rankings$ranking_date)),
  sprintf("Qualified teams: %s", nrow(participants)),
  sprintf("Qualified-team ranking history rows: %s", nrow(participant_history)),
  sprintf("Latest ranking snapshot date: %s", latest_date),
  sprintf("Latest ranking snapshot teams: %s", nrow(latest_rankings))
)

writeLines(summary_lines, file.path(out_dir, "mining_summary.txt"), useBytes = TRUE)
cat(paste(summary_lines, collapse = "\n"), "\n")
