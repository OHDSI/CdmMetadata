create table @resultsDatabaseSchema.meta_entity_activity
(
  meta_entity_activity_id       integer not null,
  meta_agent_id                 integer not null,
  entity_concept_id             integer not null,
  entity_as_string              varchar(1000) null,
  entity_identifier             integer null,
  activity_concept_id           integer not null,
  activity_type_concept_id      integer not null,
  activity_as_string            varchar(1000) null,
  activity_start_date           date null,
  activity_end_date             date null,
  security_concept_id           integer not null
);