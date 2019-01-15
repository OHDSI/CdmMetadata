
create table @resultsDatabaseSchema.meta_value
(
  meta_value_id                     integer not null,
  value_ordinal                     integer null,
  meta_entity_activity_id           integer null,
  meta_annotation_id                integer null,
  value_concept_id                  integer null,
  value_type_concept_id             integer null,
  value_as_string                   varchar(1000) null,
  value_as_number                   float null,
  operator_concept_id               integer not null
);