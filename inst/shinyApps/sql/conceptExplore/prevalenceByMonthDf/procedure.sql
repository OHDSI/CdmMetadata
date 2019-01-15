SELECT
  num.STRATUM_2,
  round(1000 * (1.0 * num.count_value / denom.count_value), 5) AS COUNT_VALUE
  FROM (
  	SELECT stratum_1, stratum_2, count_value
  	FROM achillesResults 
  	WHERE analysis_id = 602
  	GROUP BY stratum_1, stratum_2, count_value
  ) num
  INNER JOIN (
  	@denomSelects
  ) denom ON num.stratum_2 = denom.stratum_1
  WHERE num.stratum_1 = '@conceptId'
;