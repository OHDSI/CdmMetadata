select distinct A.*, 
case
  when C.annotation_as_string is null then 'Needs Review'
    else C.annotation_as_string
  end as annotation_as_string, 
D.value_as_string, concat(E.agent_first_name, ' ', E.agent_last_name) as agent
from @resultsDatabaseSchema.achilles_heel_results A
left join @resultsDatabaseSchema.meta_entity_activity B on A.achilles_heel_warning = B.activity_as_string
left join @resultsDatabaseSchema.meta_annotation C on B.meta_entity_activity_id = C.meta_entity_activity_id
left join @resultsDatabaseSchema.meta_value D on C.meta_annotation_id = D.meta_annotation_id 
left join @resultsDatabaseSchema.meta_agent E on C.meta_agent_id = E.meta_agent_id
order by A.analysis_id;