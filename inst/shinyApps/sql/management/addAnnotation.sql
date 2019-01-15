INSERT INTO @resultsDatabaseSchema.meta_annotation
(
  meta_annotation_id,
  meta_agent_id,
  meta_entity_activity_id,
  annotation_concept_id,
  annotation_type_concept_id,
  security_concept_id
)
VALUES
(
  @meta_annotation_id,
  @meta_agent_id,
  @meta_entity_activity_id,
  @annotation_concept_id,
  @annotation_type_concept_id,
  @security_concept_id
);

