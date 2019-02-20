#' Creates metadata tables
#'
#' @description
#' \code{CdmMetadata} creates metadata tables within a results database schema.
#'
#' @details
#' \code{CdmMetadata} creates metadata tables within a results database schema.
#' 
#' @param connectionDetails                An R object of type \code{connectionDetails} created using the function \code{createConnectionDetails} 
#'                                         in the \code{DatabaseConnector} package.
#' @param resultsDatabaseSchema            Fully qualified name of database schema that we can write final results to. Default is cdmDatabaseSchema. 
#'                                         On SQL Server, this should specifiy both the database and the schema, so for example, on SQL Server, 'cdm_results.dbo'.
#' @param outputFolder                     A folder to hold the logs.
#' 
#' @return                                 Nothing

#' @export
createTables <- function(connectionDetails,
                         resultsDatabaseSchema,
                         outputFolder = "output") {
  
  if (!dir.exists(outputFolder)) {
    dir.create(path = outputFolder)
  }

  ParallelLogger::clearLoggers()
  unlink(file.path(outputFolder, "log.txt"))
  
  appenders <- list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel, 
                                                         fileName = file.path(outputFolder, "log.txt")))
  
  logger <- ParallelLogger::createLogger(name = "CdmMetadata",
                                         threshold = "INFO",
                                         appenders = appenders)
  ParallelLogger::registerLogger(logger) 
  
  sqlFiles <- list.files(path = file.path(system.file(package = "CdmMetadata"), 
                                          "sql", "sql_server", "createTables"), 
                         recursive = TRUE, 
                         full.names = FALSE, 
                         all.files = FALSE,
                         pattern = "\\.sql$")
  
  for (sqlFile in sqlFiles) {
    
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = file.path("createTables", sqlFile), 
                                             packageName = "CdmMetadata", 
                                             dbms = connectionDetails$dbms,
                                             resultsDatabaseSchema = resultsDatabaseSchema)
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    exists <- tryCatch({
      existsSql <- SqlRender::render("select top 1 * from @resultsDatabaseSchema.@table;",
                                        resultsDatabaseSchema = resultsDatabaseSchema,
                                        table = tools::file_path_sans_ext(sqlFile))
      existsSql <- SqlRender::translate(sql = existsSql, targetDialect = connectionDetails$dbms)
      
      dummy <- DatabaseConnector::querySql(connection = connection, sql = existsSql)
      
      ParallelLogger::logInfo(sprintf("%s table was found, will skip it.", basename(sqlFile)))
      TRUE
    }, error = function(e) {
      ParallelLogger::logInfo(sprintf("%s table not found, will create it.", basename(sqlFile)))
      FALSE
    }, finally = function(f) {
      DatabaseConnector::disconnect(connection = connection)
    })
    
    if (!exists) {
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      DatabaseConnector::executeSql(connection = connection, sql = sql)
      DatabaseConnector::disconnect(connection = connection)
    }
  }
}