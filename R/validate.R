# Data checks that must pass before the dashboard is rendered.

validate_recon <- function(res) {
  m <- res$matches
  p <- res$postings
  linked_refs <- unique(c(m$bank_ref, m$lockbox_batch[!is.na(m$lockbox_batch)]))

  checks <- tibble::tribble(
    ~check, ~passed,
    "Deposit IDs and bank references are unique",
      !anyDuplicated(res$bank$deposit_id) && !anyDuplicated(res$bank$bank_ref),
    "Every deposit has a date, amount and owner",
      all(!is.na(res$bank$deposit_date) & !is.na(res$bank$amount) & !is.na(res$bank$owner)),
    "Every posting carries a bank reference or a lockbox batch",
      all(!is.na(p$ref) | !is.na(p$lockbox_batch)),
    "Every referenced posting links to a bank deposit",
      all(p$ref[!is.na(p$ref)] %in% res$bank$bank_ref),
    "Each deposit appears exactly once in the match results",
      nrow(m) == nrow(res$bank) && !anyDuplicated(m$deposit_id),
    "Deposits tie out: posted plus unposted equals deposited",
      isTRUE(all.equal(sum(m$posted) + sum(m$unposted), sum(m$amount))),
    "Every open item has an owner and a reason",
      all(!is.na(m$owner[m$result != "Matched"]) & m$result[m$result != "Matched"] != ""),
    "Latest day of the trend ties to open items in the match results",
      isTRUE(all.equal(tail(res$trend$open_unposted, 1),
                       sum(m$unposted[m$unposted > 0.005]), tolerance = 1e-6))
  )

  if (!all(checks$passed)) {
    print(checks[!checks$passed, ])
    stop("Data checks failed. Dashboard not rendered.")
  }
  checks
}
