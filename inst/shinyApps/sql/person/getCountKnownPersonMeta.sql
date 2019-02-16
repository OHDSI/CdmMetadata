select A.entity_identifier as person_id, count(*) as num_annotations from @resultsDatabaseSchema.meta_entity_activity A
join @resultsDatabaseSchema.meta_annotation B on A.meta_entity_activity_id = B.meta_entity_activity_id
where A.entity_as_string = 'person_id'
group by A.entity_identifier
;