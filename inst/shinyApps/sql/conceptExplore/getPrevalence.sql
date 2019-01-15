select * from @resultsDatabaseSchema.achilles_results
where stratum_1 = '@conceptId'
and analysis_id = @analysisId;