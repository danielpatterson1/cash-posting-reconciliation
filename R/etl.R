# Load the bank and posting extracts, standardize them, and run the matching.
#
# Steps
#   1. Read raw CSVs as text and standardize dates, amounts and references
#   2. Write curated Parquet
#   3. Run the DuckDB matching passes and the daily unposted cash rebuild

suppressPackageStartupMessages({
  library(dplyr)
  library(arrow)
  library(DBI)
  library(duckdb)
})

run_etl <- function(raw_dir = "data/raw", out_dir = "data/curated",
                    start = as.Date("2026-04-01"), as_of = as.Date("2026-09-30")) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  read_raw <- function(f) read.csv(file.path(raw_dir, f), colClasses = "character",
                                   na.strings = "")
  owners <- read_raw("payer_owners.csv")

  bank <- read_raw("bank_deposits.csv") |>
    transmute(
      deposit_id   = DEPOSIT_ID,
      deposit_date = as.Date(DEPOSIT_DT, format = "%m/%d/%Y"),
      deposit_type = TYPE,
      bank_ref     = toupper(trimws(BANK_REF)),
      payer_name   = trimws(PAYER),
      facility     = LOCATION,
      amount       = as.numeric(AMOUNT)
    ) |>
    left_join(owners, by = "payer_name")

  postings <- read_raw("postings.csv") |>
    transmute(
      posting_id    = POSTING_ID,
      account_id    = ACCT_NO,
      post_date     = as.Date(POST_DT, format = "%Y%m%d"),
      payer_name    = trimws(PAYER),
      ref           = toupper(trimws(REF)),
      lockbox_batch = LOCKBOX_BATCH,
      amount        = as.numeric(AMOUNT)
    )

  write_parquet(bank,     file.path(out_dir, "bank_deposits.parquet"))
  write_parquet(postings, file.path(out_dir, "postings.parquet"))

  con <- dbConnect(duckdb(shared_home = FALSE))
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  run_sql <- function(file) {
    sql <- paste(readLines(file.path("sql", file)), collapse = "\n")
    sql <- gsub("{{curated}}", out_dir, sql, fixed = TRUE)
    sql <- gsub("{{as_of}}", format(as_of), sql, fixed = TRUE)
    sql <- gsub("{{start}}", format(start), sql, fixed = TRUE)
    dbGetQuery(con, sql)
  }
  matches <- run_sql("match_deposits.sql")
  write_parquet(matches, file.path(out_dir, "matches.parquet"))
  trend <- run_sql("daily_unposted_trend.sql")
  write_parquet(trend, file.path(out_dir, "daily_unposted_trend.parquet"))

  list(bank = bank, postings = postings, matches = matches, trend = trend)
}
