
insert into @resultsDatabaseSchema.meta_agent
(
    meta_agent_id,
    meta_agent_concept_id,
    agent_first_name,
    agent_last_name,
    agent_suffix,
    agent_algorithm,
    agent_description
)
values
(
    @meta_agent_id,
    @meta_agent_concept_id,
    @agent_first_name,
    @agent_last_name,
    @agent_suffix,
    @agent_algorithm,
    @agent_description
);