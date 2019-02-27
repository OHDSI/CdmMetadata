
.getAchillesData <- function(connectionDetails,
                             vocabDatabaseSchema,
                             resultsDatabaseSchema) {
  
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "kronos/getAchillesData.sql", 
                                           packageName = "CdmMetadata", 
                                           dbms = connectionDetails$dbms, 
                                           resultsDatabaseSchema = resultsDatabaseSchema,
                                           vocabDatabaseSchema = vocabDatabaseSchema)
  
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection = connection))
  DatabaseConnector::querySql(connection = connection, sql = sql)
}