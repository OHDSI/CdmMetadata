select stratum_1, count_value
from @resultsDatabaseSchema.achilles_results where analysis_id = 110
order by stratum_1;