UPDATE @resultsDatabaseSchema.meta_entity_activity
   SET meta_agent_id = @metaAgentId
WHERE meta_entity_activity_id = @metaEntityActivityId;


UPDATE @resultsDatabaseSchema.meta_value
   SET value_as_string = '@valueAsString'
WHERE meta_value_id = @metaValueId;

