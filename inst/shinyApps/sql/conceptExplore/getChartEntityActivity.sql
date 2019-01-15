

select 
  A.activity_start_date, 
  A.activity_end_date,
  B.value_as_string,
  B.value_as_number
from @resultsDatabaseSchema.meta_entity_activity A
join @resultsDatabaseSchema.meta_value B on A.meta_entity_activity_id = B.meta_entity_activity_id
where entity_concept_id = @entityConceptId
and activity_as_string = 'Temporal Event'
;