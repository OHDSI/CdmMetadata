update @resultsDatabaseSchema.meta_value
   set value_ordinal = @valueOrdinal,
       value_concept_id = @valueConceptId,
       value_type_concept_id = @valueTypeConceptId,
       value_as_string = '@valueAsString',
       value_as_number = @valueAsNumber,
       operator_concept_id = @operatorConceptId
where meta_value_id = @metaValueId;

