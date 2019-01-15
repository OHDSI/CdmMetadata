with cte
as
(
  @cteSelects
)
select distinct A.STRATUM_2, A.COUNT_VALUE,
  B.ACTIVITY_START_DATE, 
  B.ACTIVITY_END_DATE,
  C.VALUE_AS_STRING,
  C.VALUE_AS_NUMBER
from cte A
left join @resultsDatabaseSchema.meta_entity_activity B on convert(DATE, concat(A.stratum_2, '01')) = B.activity_start_date
  and B.activity_as_string = 'Temporal Event'
  and B.entity_concept_id = @conceptId
left join @resultsDatabaseSchema.meta_value C on B.meta_entity_activity_id = C.meta_entity_activity_id
order by A.stratum_2;