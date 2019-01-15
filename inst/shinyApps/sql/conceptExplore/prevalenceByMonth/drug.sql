with cte
as
(
  SELECT
  num.stratum_2,
  round(1000 * (1.0 * num.count_value / denom.count_value), 5) AS count_value
  FROM
    (SELECT *
     FROM @resultsDatabaseSchema.ACHILLES_results WHERE analysis_id = 702) num
    INNER JOIN
    (SELECT *
     FROM @resultsDatabaseSchema.ACHILLES_results WHERE analysis_id = 117) denom
      ON num.stratum_2 = denom.stratum_1
    --calendar year
    INNER JOIN
    @vocabDatabaseSchema.concept c1
  ON CAST(num.stratum_1 AS INT) = c1.concept_id
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