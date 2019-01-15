INSERT INTO @resultsDatabaseSchema.meta_value
(
  meta_value_id,
  value_ordinal,
  meta_entity_activity_id,
  meta_annotation_id,
  value_concept_id,
  value_type_concept_id,
  value_as_string,
  value_as_number,
  operator_concept_id
)
VALUES
(
  @meta_value_id,
  @value_ordinal,
  @meta_entity_activity_id,
  @meta_annotation_id,
  @value_concept_id,
  @value_type_concept_id,
  @value_as_string,
  @value_as_number,
  @operator_concept_id
);

