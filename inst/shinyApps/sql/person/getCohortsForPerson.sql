select distinct cohort_definition_id
from @resultsDatabaseSchema.cohort
where subject_id = @personId
;