# Generate a synthetic bank deposit file and patient accounting posting file.
#
# Every value is randomly generated. Facility names, payers, trace numbers,
# account IDs and amounts do not describe any real organization or person.
#
# Planted scenarios the reconciliation has to find:
#   clean           posted in full within a few days with the bank reference
#   late lines      some lines posted days or weeks after the rest
#   keying error    one line posted with a small amount error
#   no reference    lockbox check posted without the bank reference
#   duplicate       one line posted twice
#   unposted        nothing posted for days or weeks
#
# A daily reconciliation report "launches" partway through the period. After
# launch the backlog is worked down and new exceptions are resolved faster.

suppressPackageStartupMessages(library(dplyr))

add_bdays <- function(d, n) {
  out <- d + n
  wd <- as.integer(format(out, "%u"))
  out + ifelse(wd == 6, 2, ifelse(wd == 7, 1, 0))
}

generate_cash_extracts <- function(out_dir = "data/raw",
                                   start = as.Date("2026-04-01"),
                                   as_of = as.Date("2026-09-30"),
                                   launch = as.Date("2026-08-03"),
                                   seed = 2027) {
  set.seed(seed)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  payers <- tibble(
    payer_name = c("Medicare", "Medicare Advantage Plan A", "Medicare Advantage Plan B",
                   "State Medicaid", "Medicaid Managed Care", "Commercial Plan A",
                   "Commercial Plan B", "Workers Comp", "Tricare"),
    owner      = c("Medicare team", "Medicare team", "Medicare team",
                   "Medicaid team", "Medicaid team", "Commercial team",
                   "Commercial team", "Commercial team", "Commercial team"),
    weight     = c(0.32, 0.12, 0.08, 0.08, 0.08, 0.12, 0.10, 0.04, 0.06),
    eft_share  = c(0.98, 0.9, 0.85, 0.8, 0.85, 0.75, 0.7, 0.3, 0.9)
  )
  facilities <- sprintf("Facility %02d", 1:12)

  days <- seq(start, as_of, by = "day")
  days <- days[!format(days, "%u") %in% c("6", "7")]

  n_per_day <- rpois(length(days), 26)
  dep <- tibble(deposit_date = rep(days, n_per_day)) |>
    mutate(
      deposit_id = sprintf("D%06d", row_number()),
      p          = sample(seq_len(nrow(payers)), n(), TRUE, payers$weight),
      payer_name = payers$payer_name[p],
      owner      = payers$owner[p],
      eft        = runif(n()) < payers$eft_share[p],
      deposit_type = ifelse(eft, "EFT", "Lockbox check"),
      reference  = ifelse(eft, sprintf("TRN%09d", sample(1e8:9e8, n())),
                          sprintf("CHK%07d", sample(1e6:9e6, n()))),
      facility   = sample(facilities, n(), TRUE),
      n_lines    = 1 + rpois(n(), ifelse(eft, 6, 1.5)),
      scenario   = sample(c("clean", "late lines", "keying error", "no reference",
                            "duplicate", "unposted"), n(), TRUE,
                          c(0.80, 0.06, 0.035, 0.04, 0.015, 0.05)),
      scenario   = ifelse(scenario == "no reference" & eft, "clean", scenario)
    ) |>
    select(-p, -eft)

  # One row per account level payment line inside each deposit
  lines <- dep |>
    slice(rep(seq_len(n()), n_lines)) |>
    group_by(deposit_id) |>
    mutate(line_no = row_number()) |>
    ungroup() |>
    mutate(
      account_id = sprintf("A%07d", sample(1e6:9e6, n(), TRUE)),
      amount     = round(rlnorm(n(), log(2200), 0.9), 2)
    )

  # How long an exception takes to resolve. Before launch exceptions sit for
  # weeks; after launch they are worked within days. Items still open at
  # launch are worked down within about three weeks, and a small share stay
  # stuck.
  resolve_date <- function(origin) {
    n <- length(origin)
    lag <- ifelse(origin < launch,
                  round(rgamma(n, shape = 2, scale = 22)),
                  round(rgamma(n, shape = 2, scale = 3)))
    out <- add_bdays(origin, 3 + lag)
    worked <- origin < launch & out > launch & runif(n) < 0.88
    out[worked] <- add_bdays(launch, sample(0:19, sum(worked), TRUE))
    out
  }

  # When does each line post?
  lines <- lines |>
    mutate(
      is_problem = scenario == "unposted" | (scenario == "late lines" & line_no > 1),
      post_date  = add_bdays(deposit_date, rpois(n(), 1))
    )
  lines$post_date[lines$is_problem] <- resolve_date(lines$deposit_date[lines$is_problem])

  # Keying errors and duplicates
  key_line <- lines$scenario == "keying error" & lines$line_no == 1
  posted_amount <- lines$amount
  posted_amount[key_line] <- round(lines$amount[key_line] +
                                   sample(c(-1, 1), sum(key_line), TRUE) *
                                   sample(c(0.09, 0.9, 9, 90), sum(key_line), TRUE), 2)

  postings <- lines |>
    mutate(posted_amount = posted_amount,
           batch_ref = ifelse(scenario == "no reference", NA, reference),
           lockbox_batch = ifelse(scenario == "no reference",
                                  sprintf("LBX%06d", as.integer(factor(deposit_id))), NA)) |>
    filter(post_date <= as_of)
  dup_rows <- postings |> filter(scenario == "duplicate", line_no == 1) |>
    mutate(post_date = add_bdays(post_date, 2))

  # Correcting entries: keying errors are adjusted and duplicates reversed
  key_fix <- postings |> filter(scenario == "keying error", line_no == 1) |>
    mutate(posted_amount = round(amount - posted_amount, 2),
           post_date = resolve_date(post_date))
  dup_fix <- dup_rows |>
    mutate(posted_amount = -posted_amount, post_date = resolve_date(post_date))

  postings <- bind_rows(postings, dup_rows, key_fix, dup_fix) |>
    filter(post_date <= as_of) |>
    mutate(posting_id = sprintf("P%07d", row_number()))

  bank <- lines |>
    group_by(deposit_id, deposit_date, deposit_type, reference, payer_name, facility) |>
    summarise(amount = round(sum(amount), 2), .groups = "drop")

  write.csv(bank |> transmute(DEPOSIT_ID = deposit_id,
                              DEPOSIT_DT = format(deposit_date, "%m/%d/%Y"),
                              TYPE = deposit_type, BANK_REF = reference,
                              PAYER = payer_name, LOCATION = facility,
                              AMOUNT = sprintf("%.2f", amount)),
            file.path(out_dir, "bank_deposits.csv"), row.names = FALSE)
  write.csv(postings |> transmute(POSTING_ID = posting_id, ACCT_NO = account_id,
                                  POST_DT = format(post_date, "%Y%m%d"),
                                  PAYER = payer_name, REF = batch_ref,
                                  LOCKBOX_BATCH = lockbox_batch,
                                  AMOUNT = sprintf("%.2f", posted_amount)),
            file.path(out_dir, "postings.csv"), row.names = FALSE, na = "")
  write.csv(payers |> select(payer_name, owner),
            file.path(out_dir, "payer_owners.csv"), row.names = FALSE)
  invisible(list(deposits = nrow(bank), postings = nrow(postings)))
}
