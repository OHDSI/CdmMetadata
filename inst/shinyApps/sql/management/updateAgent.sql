update @resultsDatabaseSchema.meta_agent
set agent_first_name = '@agentFirstName', agent_last_name = '@agentLastName', agent_suffix = '@agentSuffix',
  agent_algorithm = '@agentAlgorithm', agent_description = '@agentDescription'
where meta_agent_id = '@metaAgentId';