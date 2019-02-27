select row_id, concept_id, concept_name, domain_id, start_date, count_value, prevalence
from (
select dense_rank() over (order by num.analysis_id,c.concept_id) row_id,
count(*) over (partition by c.concept_id) as total,
  c.concept_id,  
  c.concept_name,
  c.domain_id,
  concat(num.stratum_2,'01') start_date,
  num.count_value,
  round(1000 * (1.0 * num.count_value / denom.count_value), 5) as prevalence
from @resultsDatabaseSchema.achilles_results num 
join @resultsDatabaseSchema.achilles_results denom on cast(num.stratum_2 as int) = cast(denom.stratum_1 as int)
join @vocabDatabaseSchema.concept c
  on cast(num.stratum_1 as int) = c.concept_id
where num.analysis_id in (202,602,802,902,1002,1802,2102) 
 and denom.analysis_id = 117
     ) tmp 
where total > 36
order by row_id, concept_id, cast(start_date as int)
;