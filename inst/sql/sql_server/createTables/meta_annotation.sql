
create table @resultsDatabaseSchema.meta_annotation
(
  meta_annotation_id               integer not null,
  meta_agent_id                    integer not null,
  meta_entity_activity_id          integer not null,
  annotation_concept_id            integer not null,
  annotation_as_string             varchar(250) null,
  annotation_type_concept_id       integer not null,
  security_concept_id              integer not null
);