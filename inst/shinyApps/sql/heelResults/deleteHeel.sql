delete from @resultsDatabaseSchema.meta_entity_activity
where meta_entity_activity_id in (@metaEntityActivityId)
;


delete from @resultsDatabaseSchema.meta_annotation
where meta_annotation_id in (@metaAnnotationId)
;


delete from @resultsDatabaseSchema.meta_value
where meta_value_id in (@metaValueId)
;