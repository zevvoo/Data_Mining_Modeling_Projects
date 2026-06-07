######################################
## FIFA Ranking Data Mining        ##
## Implemented by Jewoo Yoo         ##
## 2026-06-04                       ##
######################################

setRepositories(ind = 1:7)

library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(data.table)

# ══════════════════════════════════════════════
# 섹션 1. Selenium 시작
# ══════════════════════════════════════════════
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open()
remDr$maxWindowSize()
Sys.sleep(1)

# ══════════════════════════════════════════════
# 섹션 2. FIFA 랭킹 페이지 접속
# ══════════════════════════════════════════════
url <- "https://inside.fifa.com/fifa-world-ranking/men"
remDr$navigate(url)
Sys.sleep(5)

# ══════════════════════════════════════════════
# 섹션 3. 쿠키 팝업 닫기
# ══════════════════════════════════════════════
tryCatch({
  accept_btn <- remDr$findElement("css selector", "#onetrust-accept-btn-handler")
  accept_btn$clickElement()
  cat("쿠키 팝업 닫힘\n")
  Sys.sleep(2)
}, error = function(e) {
  cat("쿠키 팝업 없음, 계속 진행\n")
})

# ══════════════════════════════════════════════
# 헬퍼 함수들
# ══════════════════════════════════════════════

# 헬퍼 1: Filters 패널 열기
# [변경] "이미 열려있음" 확인 로직 제거 → 단순히 버튼 찾아서 클릭
open_filters_panel <- function() {
  btn <- tryCatch(
    remDr$findElement("css selector",
                      "button.live-world-ranking-filters-module_filterButtonDesktop__yhxl9"),
    error = function(e) NULL
  )
  if (is.null(btn)) {
    btn <- tryCatch(
      remDr$findElement("xpath",
                        "//button[contains(translate(normalize-space(.),'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'filter')]"),
      error = function(e) NULL
    )
  }
  if (is.null(btn)) {
    cat("  [WARN] Filters 버튼을 찾을 수 없습니다.\n")
    return(FALSE)
  }
  remDr$executeScript("arguments[0].scrollIntoView({block:'center'});", list(btn))
  Sys.sleep(0.5)
  remDr$executeScript("arguments[0].click();", list(btn))
  Sys.sleep(2.5)
  cat("  Filters 패널 열기 완료\n")
  return(TRUE)
}

# 헬퍼 2: 드롭다운 버튼 찾기
find_dropdown_btn <- function(label, max_try = 5) {
  xpath1 <- paste0("//button[.//span[normalize-space(text())='", label, "']]")
  xpath2 <- paste0("//button[@aria-label='", label, "']")
  xpath3 <- paste0("//button[contains(normalize-space(.), '", label, "')]")
  for (attempt in seq_len(max_try)) {
    for (xp in c(xpath1, xpath2, xpath3)) {
      el <- tryCatch(remDr$findElement("xpath", xp), error = function(e) NULL)
      if (!is.null(el)) return(el)
    }
    Sys.sleep(0.5)
  }
  stop(paste0("드롭다운 버튼 '", label, "'을 찾을 수 없습니다."))
}

# 헬퍼 3: 열린 드롭다운 ul 찾기
get_open_ul <- function(max_wait = 10) {
  for (attempt in 1:max_wait) {
    uls <- remDr$findElements("css selector", "ul.dropdown-module_optionsList__Lb63I")
    for (ul in uls) {
      cls <- tryCatch(ul$getElementAttribute("class")[[1]], error = function(e) "")
      if (!grepl("hidden", cls)) {
        lis <- tryCatch(ul$findChildElements("xpath", ".//li[@role='option']"),
                        error = function(e) list())
        if (length(lis) > 0) return(ul)
      }
    }
    Sys.sleep(0.5)
  }
  return(NULL)
}

# 헬퍼 4: fallback ul 찾기
# [변경] max_wait 기본값 15로 증가 → 느린 연도(1992 등) 대응
get_open_ul_fallback <- function(max_wait = 15) {
  result <- get_open_ul(max_wait)
  if (!is.null(result)) return(result)
  for (attempt in 1:max_wait) {
    uls <- remDr$findElements("xpath",
                              "//ul[@role='listbox' or contains(@class,'optionsList') or contains(@class,'dropdown')]")
    for (ul in uls) {
      display <- tryCatch(ul$getCssValue("display")[[1]],    error = function(e) "none")
      vis     <- tryCatch(ul$getCssValue("visibility")[[1]], error = function(e) "hidden")
      if (display != "none" && vis != "hidden") {
        lis <- tryCatch(ul$findChildElements("xpath", ".//li[@role='option']"),
                        error = function(e) list())
        if (length(lis) > 0) return(ul)
      }
    }
    Sys.sleep(0.5)
  }
  return(NULL)
}

# 헬퍼 5: Year 선택 공통 함수
# [변경] open_ul NULL일 때 최대 3회 재시도
select_year <- function(yr, max_retry = 3) {
  for (retry in seq_len(max_retry)) {
    year_btn <- find_dropdown_btn("Year")
    remDr$executeScript("arguments[0].click();", list(year_btn))
    Sys.sleep(2.5)  # 기존 2초 → 2.5초로 증가
    
    open_ul <- get_open_ul_fallback()
    if (is.null(open_ul)) {
      cat("  [재시도", retry, "/", max_retry, "] Year 드롭다운 open_ul NULL\n")
      Sys.sleep(1)
      next
    }
    
    yr_li <- tryCatch(
      open_ul$findChildElements("xpath",
                                paste0(".//li[@role='option']//span[normalize-space(text())='", yr, "']")),
      error = function(e) list()
    )
    if (length(yr_li) == 0) {
      yr_li <- tryCatch(
        open_ul$findChildElements("xpath",
                                  paste0(".//li[@role='option' and normalize-space(.)='", yr, "']")),
        error = function(e) list()
      )
    }
    if (length(yr_li) == 0) {
      cat("  [재시도", retry, "/", max_retry, "] 연도 항목을 찾지 못함:", yr, "\n")
      # 드롭다운 닫고 재시도
      remDr$executeScript("arguments[0].click();", list(year_btn))
      Sys.sleep(1)
      next
    }
    
    remDr$executeScript("arguments[0].click();", list(yr_li[[1]]))
    Sys.sleep(2.5)
    return(TRUE)
  }
  cat("  연도 선택 최종 실패:", yr, "\n")
  return(FALSE)
}

# ══════════════════════════════════════════════
# 섹션 4. Show full rankings 버튼 최초 1회 클릭
# [변경] 별도 섹션으로 분리 → 루프 안에서는 호출 안 함
# ══════════════════════════════════════════════
cat("\n[섹션 4] Show full rankings 버튼 탐색\n")

# 페이지 기본 상태에서 스크롤하며 버튼 탐색
show_btn_clicked <- FALSE
for (s in 1:20) {
  remDr$executeScript("window.scrollBy(0, 400);")
  Sys.sleep(0.4)
  show_btn <- tryCatch(
    remDr$findElement("xpath",
                      "//button[@aria-label='Show full rankings' or contains(normalize-space(.), 'Show full rankings')]"),
    error = function(e) NULL
  )
  if (!is.null(show_btn)) {
    remDr$executeScript("arguments[0].click();", list(show_btn))
    Sys.sleep(3)
    cat("Show full rankings 클릭 완료\n")
    show_btn_clicked <- TRUE
    break
  }
}
if (!show_btn_clicked) cat("Show full rankings 버튼 없음 (스크롤 로드 방식 사용)\n")
remDr$executeScript("window.scrollTo(0, 0);")
Sys.sleep(1)

# ══════════════════════════════════════════════
# 섹션 5. Filters 패널 열기
# ══════════════════════════════════════════════
open_filters_panel()

# ══════════════════════════════════════════════
# 섹션 6. 연도 목록 수집
# ══════════════════════════════════════════════
year_btn <- find_dropdown_btn("Year")
remDr$executeScript("arguments[0].click();", list(year_btn))
Sys.sleep(2.5)

open_ul <- get_open_ul_fallback()
if (is.null(open_ul)) stop("Year 드롭다운 열기 실패")

year_items  <- open_ul$findChildElements("xpath", ".//li[@role='option']")
year_labels <- sapply(year_items, function(el) trimws(el$getElementText()[[1]]))
cat("수집된 연도 수:", length(year_labels), "\n")
print(year_labels)

# Year 드롭다운 닫기
year_btn <- find_dropdown_btn("Year")
remDr$executeScript("arguments[0].click();", list(year_btn))
Sys.sleep(1)

# ══════════════════════════════════════════════
# 섹션 7. 재시작 복구 로직
# ══════════════════════════════════════════════
checkpoint_file <- "fifa_rankings_checkpoint.rds"

if (file.exists(checkpoint_file)) {
  fifa_rankings <- readRDS(checkpoint_file)
  cat("\n[복구] 기존 체크포인트 로드 완료\n")
  cat("  이미 수집된 행 수:", nrow(fifa_rankings), "\n")
  cat("  마지막 수집:", tail(fifa_rankings$year, 1),
      tail(fifa_rankings$date_label, 1), "\n\n")
} else {
  fifa_rankings <- data.frame(
    year       = character(),
    date_label = character(),
    rank       = integer(),
    country    = character(),
    points     = numeric(),
    stringsAsFactors = FALSE
  )
  cat("[신규 시작] 체크포인트 없음\n\n")
}

done_keys     <- paste(fifa_rankings$year, fifa_rankings$date_label, sep = "_")
total_collected <- 0

# ══════════════════════════════════════════════
# 섹션 8. 메인 크롤링 루프
# 순서: 오래된 연도부터 → 각 연도 내 오래된 날짜부터
# ══════════════════════════════════════════════
year_labels_rev <- rev(year_labels)   # 오래된 연도부터

for (yr in year_labels_rev) {
  
  cat("\n════════════════════════════════\n")
  cat("[연도]", yr, "\n")
  cat("════════════════════════════════\n")
  
  # Year 선택 (재시도 포함)
  if (!select_year(yr)) next
  
  # 해당 연도의 날짜 목록 수집
  date_btn <- find_dropdown_btn("Date")
  remDr$executeScript("arguments[0].click();", list(date_btn))
  Sys.sleep(2)
  
  open_ul <- get_open_ul_fallback()
  if (is.null(open_ul)) {
    cat("  Date 드롭다운 열기 실패:", yr, "\n")
    # 드롭다운 닫기 시도 후 next
    tryCatch({
      db <- find_dropdown_btn("Date")
      remDr$executeScript("arguments[0].click();", list(db))
      Sys.sleep(1)
    }, error = function(e) NULL)
    next
  }
  
  date_items  <- tryCatch(
    open_ul$findChildElements("xpath", ".//li[@role='option']"),
    error = function(e) list()
  )
  date_labels_raw <- sapply(date_items, function(el)
    tryCatch(trimws(el$getElementText()[[1]]), error = function(e) ""))
  date_labels_raw <- date_labels_raw[nchar(date_labels_raw) > 0]
  
  cat("  날짜 수:", length(date_labels_raw), "\n")
  if (length(date_labels_raw) == 0) {
    tryCatch({
      db <- find_dropdown_btn("Date")
      remDr$executeScript("arguments[0].click();", list(db))
      Sys.sleep(1)
    }, error = function(e) NULL)
    next
  }
  
  # [변경] 오래된 날짜부터 처리하기 위해 rev() 적용
  # 드롭다운에서 위쪽이 최신, 아래쪽이 오래된 날짜이므로 rev()
  date_labels <- rev(date_labels_raw)
  
  # Date 드롭다운 닫기
  date_btn <- find_dropdown_btn("Date")
  remDr$executeScript("arguments[0].click();", list(date_btn))
  Sys.sleep(1)
  
  # 날짜별 랭킹 수집
  for (i in seq_along(date_labels)) {
    
    dt       <- date_labels[i]
    this_key <- paste(yr, dt, sep = "_")
    
    # 이미 수집한 날짜 건너뜀
    if (this_key %in% done_keys) {
      cat("  [SKIP] 이미 수집됨:", yr, dt, "\n")
      next
    }
    
    cat("\n  [", i, "/", length(date_labels), "]", yr, dt, "\n")
    
    # Year 재선택 (날짜 클릭 후 Year가 리셋될 수 있음)
    if (!select_year(yr)) next
    
    # Date 선택
    # [변경] date_labels가 rev() 적용됐으므로
    #        드롭다운의 실제 인덱스는 (전체 길이 - i + 1)
    actual_i <- length(date_labels) - i + 1
    
    date_btn <- find_dropdown_btn("Date")
    remDr$executeScript("arguments[0].click();", list(date_btn))
    Sys.sleep(2)
    
    date_ok <- tryCatch({
      open_ul <- get_open_ul_fallback()
      if (is.null(open_ul)) stop("Date ul 열기 실패")
      lis <- open_ul$findChildElements("xpath", ".//li[@role='option']")
      if (actual_i > length(lis)) stop(paste("인덱스 초과:", actual_i, ">", length(lis)))
      remDr$executeScript("arguments[0].click();", list(lis[[actual_i]]))
      Sys.sleep(2.5)
      TRUE
    }, error = function(e) {
      cat("    날짜 클릭 실패:", conditionMessage(e), "\n")
      tryCatch({
        db <- find_dropdown_btn("Date")
        remDr$executeScript("arguments[0].click();", list(db))
        Sys.sleep(1)
      }, error = function(e2) NULL)
      FALSE
    })
    
    if (!date_ok) next
    
    # 스크롤로 전체 랭킹 로드
    # [변경] Show full rankings 버튼 탐색 제거
    #        순수 스크롤 방식만 사용
    # ────────────────────────────────────────────
    # [수정 후] JS로 국가 수 체크 → getPageSource는 1회만
    # ────────────────────────────────────────────
    remDr$executeScript("window.scrollTo(0, 0);")
    Sys.sleep(1)
    prev_count <- 0
    stable_cnt <- 0
    for (sc in 1:60) {
      remDr$executeScript("window.scrollBy(0, 800);")
      Sys.sleep(0.8)
      # getPageSource() 대신 JS로 DOM 요소 수만 카운트 → 메모리 절약
      cur_count <- tryCatch(
        remDr$executeScript(
          "return document.querySelectorAll('[class*=\"teamName\"]').length;",
          list())[[1]],
        error = function(e) prev_count
      )
      if (cur_count == prev_count) {
        stable_cnt <- stable_cnt + 1
        if (stable_cnt >= 3) {
          cat("    스크롤 로드 완료 (국가 수:", cur_count, ")\n")
          break
        }
      } else {
        stable_cnt <- 0
        prev_count <- cur_count
      }
    }
    remDr$executeScript("window.scrollTo(0, 0);")
    Sys.sleep(1)
    
    # 랭킹 데이터 파싱
    html      <- remDr$getPageSource()[[1]] %>% read_html()
    ranks     <- html %>% html_nodes("h3.custom-rank-cell_rankNumber__RORLl")    %>% html_text(trim = TRUE)
    countries <- html %>% html_nodes("a.custom-team-cell_teamName__c_tEs")        %>% html_text(trim = TRUE)
    points    <- html %>% html_nodes("h4.custom-points-cell_points__Lt6_7 span") %>% html_text(trim = TRUE)
    
    # fallback selector
    if (length(ranks) == 0) {
      ranks     <- html %>% html_nodes("[class*='rankNumber']")  %>% html_text(trim = TRUE)
      countries <- html %>% html_nodes("[class*='teamName']")    %>% html_text(trim = TRUE)
      points    <- html %>% html_nodes("[class*='points'] span") %>% html_text(trim = TRUE)
    }
    
    min_len <- min(length(ranks), length(countries), length(points))
    
    if (min_len == 0) {
      # points 없는 연도 대응 (rank + country만 수집)
      min_len2 <- min(length(ranks), length(countries))
      if (min_len2 > 0) {
        df_tmp <- data.frame(
          year       = yr,
          date_label = dt,
          rank       = as.integer(ranks[1:min_len2]),
          country    = countries[1:min_len2],
          points     = NA_real_,
          stringsAsFactors = FALSE
        )
        fifa_rankings   <- rbind(fifa_rankings, df_tmp)
        done_keys       <- c(done_keys, this_key)
        total_collected <- total_collected + 1
        cat("    수집 완료 (포인트 없음) | 국가 수:", min_len2, "\n")
      } else {
        cat("    데이터 없음:", yr, dt, "\n")
      }
    } else {
      df_tmp <- data.frame(
        year       = yr,
        date_label = dt,
        rank       = as.integer(ranks[1:min_len]),
        country    = countries[1:min_len],
        points     = suppressWarnings(as.numeric(gsub(",", "", points[1:min_len]))),
        stringsAsFactors = FALSE
      )
      fifa_rankings   <- rbind(fifa_rankings, df_tmp)
      done_keys       <- c(done_keys, this_key)
      total_collected <- total_collected + 1
      cat("    수집 완료 | 국가 수:", min_len, "\n")
    }
    
    # 중간 저장 (10개 날짜마다)
    if (total_collected %% 10 == 0 && total_collected > 0) {
      saveRDS(fifa_rankings, checkpoint_file)
      cat("  [체크포인트 저장] 누적 날짜:", total_collected,
          "| 누적 행:", nrow(fifa_rankings), "\n")
    }
    
    remDr$executeScript("window.scrollTo(0, 0);")
    Sys.sleep(1)
  }
  
  # 연도 완료 시 저장
  saveRDS(fifa_rankings, checkpoint_file)
  cat("\n  [연도 완료 저장]", yr, "| 누적 행:", nrow(fifa_rankings), "\n")
}

# ══════════════════════════════════════════════
# 섹션 9. 최종 저장
# ══════════════════════════════════════════════
saveRDS(fifa_rankings, "fifa_rankings_all.rds")

fwrite(fifa_rankings, "fifa_rankings_men.tsv", sep = "\t")
write.table(fifa_rankings, "fifa_rankings_men_euckr.tsv", sep = "\t",
            row.names = FALSE, fileEncoding = "euc-kr")

cat("Saved", nrow(fifa_rankings), "rows to file\n")

# 브라우저 세션 종료
remDr$close()
