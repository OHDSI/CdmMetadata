with cte
as
(
  SELECT
    num.stratum_2,
    -- calendar year, note, there could be blanks
    round(1000 * (1.0 * num.count_value / denom.count_value), 5) AS count_value --prevalence, per 1000 persons
  FROM
    (SELECT
       CAST(stratum_1 AS BIGINT) stratum_1,
       CAST(stratum_2 AS INT) stratum_2,
       count_value
     FROM
       @resultsDatabaseSchema.ACHILLES_results WHERE analysis_id = 402 GROUP BY analysis_id, stratum_1, stratum_2, count_value) num
    INNER JOIN
    (SELECT
       CAST(stratum_1 AS INT) stratum_1,
       count_value
     FROM
       @resultsDatabaseSchema.ACHILLES_results WHERE analysis_id = 117 GROUP BY analysis_id, stratum_1, count_value) denom
      ON num.stratum_2 = denom.stratum_1
    --calendar year
    INNER JOIN
    @vocabDatabaseSchema.concept c1
  ON num.stratum_1 = c1.concept_id
  WHERE c1.concept_id = @conceptId
)
select distinct A.stratum_2, A.count_value,
  B.activity_start_date, 
  B.activity_end_date,
  C.value_as_string,
  C.value_as_number
from cte A
left join @resultsDatabaseSchema.meta_entity_activity B on convert(DATE, concat(A.stratum_2, '01')) = B.activity_start_date
  and B.activity_as_string = 'Temporal Event'
  and B.entity_concept_id = @conceptId
left join @resultsDatabaseSchema.meta_value C on B.meta_entity_activity_id = C.meta_entity_activity_id
order by A.stratum_2
;