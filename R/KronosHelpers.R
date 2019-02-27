
.getMonth <- function(AchillesTs,indices = NULL) {
  tsObj <- AchillesTs
  
  if (is.null(indices)) {
    (time(tsObj) - floor(time(tsObj)))*12+1	
  } else {
    (time(tsObj)[indices] - floor(time(tsObj)[indices]))*12+1	
  }
}

.getYear <- function(AchillesTs,indices = NULL) {
  tsObj <- AchillesTs
  
  if (is.null(indices)) {
    floor(time(tsObj))
  } else {
    floor(time(tsObj)[indices])
  }
}

.getAgents <- function(connectionDetails,
                       resultsDatabaseSchema) {
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection = connection))
  
  sql <- SqlRender::render("select * from @resultsDatabaseSchema.meta_agent
                                order by meta_agent_id;",
                           resultsDatabaseSchema = resultsDatabaseSchema)
  sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)
  DatabaseConnector::querySql(connection = connection, sql = sql)
}

.agentRecordExists <- function(connectionDetails, resultsDatabaseSchema) {
  library(magrittr)
  df <- .getAgents(connectionDetails, resultsDatabaseSchema) %>%
    dplyr::filter(META_AGENT_CONCEPT_ID == 2000 &
                    AGENT_ALGORITHM == "Kronos")
  
  nrow(df) > 0
}

.getMaxId <- function(tableName, fieldName, connectionDetails, resultsDatabaseSchema) {
  
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection = connection))
  
  sql <- SqlRender::render(sql = "select max(@fieldName) as MAX_ID from @resultsDatabaseSchema.@tableName;",
                           fieldName = fieldName,
                           tableName = tableName,
                           resultsDatabaseSchema = resultsDatabaseSchema)
  sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)
  
  result <- DatabaseConnector::querySql(connection = connection, sql = sql)
  
  if (is.na(result$MAX_ID)) {
    0
  } else {
    as.integer(result$MAX_ID)
  }
}

.getKronosAgent <- function(connectionDetails, resultsDatabaseSchema) {
  
  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection = connection))
  
  if (!.agentRecordExists(connectionDetails, resultsDatabaseSchema)) {
    df <- data.frame(
      meta_agent_id = .getMaxId(tableName = "meta_agent", fieldName = "meta_agent_id", 
                                connectionDetails = connectionDetails,
                                resultsDatabaseSchema = resultsDatabaseSchema) + 1,
      meta_agent_concept_id = 2000,
      agent_first_name = NA,
      agent_last_name = NA,
      agent_suffix = NA,
      agent_algorithm = "Kronos",
      agent_description = "Time Series Analysis", stringsAsFactors = FALSE
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_agent", resultsDatabaseSchema), 
                                   data = df, 
                                   dropTableIfExists = FALSE, createTable = FALSE, useMppBulkLoad = FALSE) 
  }
  library(magrittr)
  df <- .getAgents(connectionDetails, resultsDatabaseSchema) %>%
    dplyr::filter(META_AGENT_CONCEPT_ID == 2000 &
                    AGENT_ALGORITHM == "Kronos")
  df$META_AGENT_ID
}