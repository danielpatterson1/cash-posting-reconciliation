/*
  Match every bank deposit to patient accounting postings.

  Pass 1  Reference: postings carrying the bank trace or check number.
  Pass 2  Amount and date: lockbox batches posted without a reference are
          matched to an unreferenced deposit from the same payer with the
          same total, posted within 10 days of the deposit. Ties go to the
          closest date, and each batch is used once.
  Pass 3  Anything left is an open item, classified by what went wrong.
*/
WITH deposits AS (
  SELECT * FROM read_parquet('{{curated}}/bank_deposits.parquet')
),
postings AS (
  SELECT * FROM read_parquet('{{curated}}/postings.parquet')
),
by_ref AS (
  SELECT ref, SUM(amount) AS posted, COUNT(*) AS lines, MAX(post_date) AS last_post
  FROM postings WHERE ref IS NOT NULL GROUP BY ref
),
pass1 AS (
  SELECT d.deposit_id, r.posted, r.lines, r.last_post, 'Reference' AS method
  FROM deposits d JOIN by_ref r ON r.ref = d.bank_ref
),
batches AS (
  SELECT lockbox_batch, payer_name, SUM(amount) AS posted, COUNT(*) AS lines,
         MIN(post_date) AS first_post, MAX(post_date) AS last_post
  FROM postings WHERE lockbox_batch IS NOT NULL
  GROUP BY lockbox_batch, payer_name
),
pass2_candidates AS (
  SELECT d.deposit_id, b.lockbox_batch, b.posted, b.lines, b.last_post,
         ROW_NUMBER() OVER (PARTITION BY b.lockbox_batch
                            ORDER BY b.first_post - d.deposit_date, d.deposit_id) AS rn_batch,
         ROW_NUMBER() OVER (PARTITION BY d.deposit_id
                            ORDER BY b.first_post - d.deposit_date, b.lockbox_batch) AS rn_dep
  FROM deposits d
  JOIN batches b
    ON b.payer_name = d.payer_name
   AND ABS(b.posted - d.amount) < 0.005
   AND b.first_post BETWEEN d.deposit_date AND d.deposit_date + 10
  WHERE d.deposit_id NOT IN (SELECT deposit_id FROM pass1)
),
pass2 AS (
  SELECT deposit_id, lockbox_batch, posted, lines, last_post, 'Amount and date' AS method
  FROM pass2_candidates WHERE rn_batch = 1 AND rn_dep = 1
),
linked AS (
  SELECT deposit_id, posted, lines, last_post, method, NULL AS lockbox_batch FROM pass1
  UNION ALL
  SELECT deposit_id, posted, lines, last_post, method, lockbox_batch FROM pass2
)
SELECT
  d.deposit_id, d.deposit_date, d.deposit_type, d.bank_ref, d.payer_name, d.owner,
  d.facility, d.amount,
  COALESCE(l.posted, 0)                   AS posted,
  ROUND(d.amount - COALESCE(l.posted, 0), 2) AS unposted,
  COALESCE(l.lines, 0)                    AS lines_posted,
  l.last_post,
  l.lockbox_batch,
  COALESCE(l.method, 'None')              AS match_method,
  CASE
    WHEN l.deposit_id IS NULL                       THEN 'No matching posting'
    WHEN ABS(d.amount - l.posted) < 0.005           THEN 'Matched'
    WHEN ABS(d.amount - l.posted) < 100             THEN 'Amount variance'
    WHEN l.posted > d.amount                        THEN 'Over posted or duplicate'
    ELSE 'Partial posting'
  END AS result,
  DATE '{{as_of}}' - d.deposit_date AS age_days
FROM deposits d
LEFT JOIN linked l USING (deposit_id)
ORDER BY d.deposit_date, d.deposit_id
