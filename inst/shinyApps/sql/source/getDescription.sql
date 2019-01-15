select B.value_as_string 
from @resultsDatabaseSchema.meta_entity_activity A
join @resultsDatabaseSchema.meta_value B on A.meta_entity_activity_id = B.meta_entity_activity_id
where A.activity_as_string = 'Source Provenance';