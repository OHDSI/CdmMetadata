update @resultsDatabaseSchema.meta_entity_activity
  set entity_concept_id = @entityConceptId,
      entity_as_string = '@entityAsString',
      entity_identifier = @entityIdentifier,
      activity_concept_id = @activityConceptId,
      activity_type_concept_id = @activityTypeConceptId,
      activity_as_string = '@activityAsString',
      activity_start_date = '@activityStartDate',
      activity_end_date = '@activityEndDate',
      security_concept_id = @securityConceptId
  where meta_entity_activity_id = @metaEntityActivityId
;