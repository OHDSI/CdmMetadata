SELECT
  num.STRATUM_2,
  round(1000 * (1.0 * num.count_value / denom.count_value), 5) AS COUNT_VALUE
FROM
(SELECT *
 FROM achillesResults WHERE analysis_id = 802) num
INNER JOIN
(@denomSelects) denom ON num.stratum_2 = denom.stratum_1
WHERE num.stratum_1 = '@conceptId'
;