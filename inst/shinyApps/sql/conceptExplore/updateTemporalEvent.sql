update @resultsDatabaseSchema.meta_value
   set value_as_string = '@valueAsString'
where meta_value_id = @metaValueId;