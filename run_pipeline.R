# One command build: generate synthetic extracts, match, validate, render.
#   Rscript run_pipeline.R

source("R/generate_synthetic_data.R")
source("R/etl.R")
source("R/validate.R")

start  <- as.Date("2026-04-01")
as_of  <- as.Date("2026-09-30")
launch <- as.Date("2026-08-03")

generate_cash_extracts(start = start, as_of = as_of, launch = launch)
res    <- run_etl(start = start, as_of = as_of)
checks <- validate_recon(res)
saveRDS(checks, "data/curated/checks.rds")

message(sprintf("Reconciliation complete: %s deposits, %s postings, %s open items, %s of %s checks passed",
                format(nrow(res$bank), big.mark = ","), format(nrow(res$postings), big.mark = ","),
                sum(res$matches$result != "Matched"), sum(checks$passed), nrow(checks)))

rmarkdown::render("recon_dashboard.Rmd", output_file = "index.html", quiet = TRUE)
message("Dashboard written to index.html")
