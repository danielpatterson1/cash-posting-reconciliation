/*
  Rebuild the open unposted cash position at the end of every business day.
  A deposit contributes the part of its amount not yet posted as of that day.
  Postings are linked to deposits through the match results, so lockbox
  batches without a reference count once they are matched.
*/
WITH days AS (
  SELECT CAST(d AS DATE) AS day
  FROM range(DATE '{{start}}', DATE '{{as_of}}' + INTERVAL 1 DAY, INTERVAL 1 DAY) t(d)
  WHERE dayofweek(d) BETWEEN 1 AND 5
),
m AS (
  SELECT * FROM read_parquet('{{curated}}/matches.parquet')
),
p AS (
  SELECT * FROM read_parquet('{{curated}}/postings.parquet')
),
linked_postings AS (
  SELECT m.deposit_id, p.post_date, p.amount
  FROM m JOIN p ON p.ref = m.bank_ref
  UNION ALL
  SELECT m.deposit_id, p.post_date, p.amount
  FROM m JOIN p ON p.lockbox_batch = m.lockbox_batch
),
daily AS (
  SELECT
    days.day,
    m.deposit_id,
    m.deposit_date,
    m.amount - COALESCE(SUM(lp.amount), 0) AS open_amount
  FROM days
  JOIN m ON m.deposit_date <= days.day
  LEFT JOIN linked_postings lp
    ON lp.deposit_id = m.deposit_id AND lp.post_date <= days.day
  GROUP BY days.day, m.deposit_id, m.deposit_date, m.amount
)
SELECT
  day,
  COUNT(*) FILTER (WHERE open_amount > 0.005)                                AS open_deposits,
  SUM(open_amount) FILTER (WHERE open_amount > 0.005)                        AS open_unposted,
  COUNT(*) FILTER (WHERE open_amount > 0.005 AND day - deposit_date > 30)    AS aged_deposits,
  COALESCE(SUM(open_amount) FILTER (WHERE open_amount > 0.005 AND day - deposit_date > 30), 0) AS aged_unposted
FROM daily
GROUP BY day
ORDER BY day
