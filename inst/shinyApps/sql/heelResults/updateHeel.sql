update @resultsDatabaseSchema.meta_annotation
set meta_agent_id = @metaAgentId
where meta_annotation_id = @metaAnnotationId
;


update @resultsDatabaseSchema.meta_annotation
set annotation_as_string = '@annotationAsString'
where meta_annotation_id = @metaAnnotationId
;


update @resultsDatabaseSchema.meta_value
set value_as_string = '@valueAsString'
where meta_value_id = @metaValueId
;