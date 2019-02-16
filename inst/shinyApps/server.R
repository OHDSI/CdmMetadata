library(ffbase)
library(shiny)
library(DT)
library(magrittr)
library(tidyr)
library(httr)
library(shinyjs)
library(lubridate)
library(shinyWidgets)

shinyServer(function(input, output, session) {
  
  refresh <- reactiveValues(conceptKb = FALSE)

  if (!dir.exists(file.path(dataPath, "ff"))) {
    dir.create(file.path(dataPath, "ff"))
  }
  options('fftempdir' = file.path(dataPath, "ff"))
    
  # hides ---------------------------------------------------------------
  
  if (!file.exists(jsonPath)) {
    shinyjs::hide(selector = "#sidebarCollapsed li a[data-value=overview]")
    shinyjs::hide(selector = "#sidebarCollapsed li a[data-value=provenance]")
    shinyjs::hide(selector = "#sidebarCollapsed li a[data-value=heelResults]")
    shinyjs::hide(selector = "#sidebarCollapsed li a[data-value=knowledgeBase]")
    shinyjs::hide(selector = "#sidebarCollapsed li a[data-value=design]")
    shinyjs::hide(id = "sidebarSelects")  
  }
  
  # globals ------------------------------------------------------------
  
  .getConnectionDetails <- function(cdmSource) {
    if (is.null(cdmSource$user)) {
      connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = cdmSource$dbms,
                                                                      server = cdmSource$server,
                                                                      port = cdmSource$port,
                                                                      extraSettings = cdmSource$extraSettings)
    } else {
      connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = cdmSource$dbms,
                                                                      server = cdmSource$server,
                                                                      port = cdmSource$port,
                                                                      user = cdmSource$user,
                                                                      password = cdmSource$password,
                                                                      extraSettings = cdmSource$extraSettings)
    }
    connectionDetails  
  }
  
  baseUrl <- reactive({
    if (file.exists(jsonPath)) {
      (readRDS(jsonPath))$baseUrl  
    } else {
      FALSE
    }
  })
  
  .warmCaches <- function() {
    
    showModal(
      modalDialog(size = "m",
                  title = "Warming Achilles caches",
                  "Warming Achilles caches in order to serve up metadata faster"
      )
    )
    
    cdmSources <- (readRDS(jsonPath))$sources
    
    for (cdmSource in cdmSources) {
      connectionDetails <- .getConnectionDetails(cdmSource)
      
      ffDir <- file.path(dataPath, "persons", cdmSource$name, cdmSource$loadId)
      if (!dir.exists(file.path(dataPath, "persons"))) {
        dir.create(file.path(dataPath, "persons"), recursive = TRUE)
      }
      
      if (!dir.exists(ffDir)) {
        connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
        
        sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "person/getPersons.sql"))
        sql <- SqlRender::renderSql(sql = sql, 
                                    cdmDatabaseSchema = cdmSource$cdmDatabaseSchema)$sql
        sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
        
        persons <- DatabaseConnector::querySql.ffdf(connection = connection, sql = sql)
        
        ffbase::save.ffdf(persons, dir = ffDir)
        DatabaseConnector::disconnect(connection = connection)
      }
      
      ffDir <- file.path(dataPath, "achillesConcepts", cdmSource$name, cdmSource$loadId)
      if (!dir.exists(file.path(dataPath, "achillesConcepts"))) {
        dir.create(file.path(dataPath, "achillesConcepts"), recursive = TRUE)
      }
      
      if (!dir.exists(ffDir)) {
        sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "conceptExplore/getAchillesConcepts.sql"))
        sql <- SqlRender::renderSql(sql = sql, 
                                    resultsDatabaseSchema = cdmSource$resultsDatabaseSchema,
                                    vocabDatabaseSchema = cdmSource$vocabDatabaseSchema, 
                                    warnOnMissingParameters = FALSE)$sql
        sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
        
        connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
        achillesConcepts <- DatabaseConnector::querySql.ffdf(connection = connection, sql = sql)
        
        ffbase::save.ffdf(achillesConcepts, dir = ffDir)
        DatabaseConnector::disconnect(connection = connection)
      }
      
      ffDir <- file.path(dataPath, "achillesResults", cdmSource$name, cdmSource$loadId)
      if (!dir.exists(file.path(dataPath, "achillesResults"))) {
        dir.create(file.path(dataPath, "achillesResults"), recursive = TRUE)
      }
      
      if (!dir.exists(ffDir)) {
        sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "conceptExplore/getAchillesResults.sql"))
        sql <- SqlRender::renderSql(sql = sql, 
                                    resultsDatabaseSchema = cdmSource$resultsDatabaseSchema,
                                    vocabDatabaseSchema = cdmSource$vocabDatabaseSchema, 
                                    warnOnMissingParameters = FALSE)$sql
        sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
        
        connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
        achillesResults <- DatabaseConnector::querySql.ffdf(connection = connection, sql = sql)
        
        ffbase::save.ffdf(achillesResults, dir = ffDir)
        DatabaseConnector::disconnect(connection = connection)
      }
      
      ffDir <- file.path(dataPath, "observationPeriods", cdmSource$name, cdmSource$loadId)
      if (!dir.exists(file.path(dataPath, "observationPeriods"))) {
        dir.create(file.path(dataPath, "observationPeriods"), recursive = TRUE)
      }
      
      if (!dir.exists(ffDir)) {
        sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "source/getObservationPeriods.sql"))
        sql <- SqlRender::renderSql(sql = sql, 
                                    resultsDatabaseSchema = cdmSource$resultsDatabaseSchema,
                                    vocabDatabaseSchema = cdmSource$vocabDatabaseSchema, 
                                    warnOnMissingParameters = FALSE)$sql
        sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
        
        connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
        observationPeriods <- DatabaseConnector::querySql.ffdf(connection = connection, sql = sql)
        
        ffbase::save.ffdf(observationPeriods, dir = ffDir)
        DatabaseConnector::disconnect(connection = connection)
      }
      
      ffDir <- file.path(dataPath, "oneDayObs", cdmSource$name, cdmSource$loadId)
      if (!dir.exists(file.path(dataPath, "oneDayObs"))) {
        dir.create(file.path(dataPath, "oneDayObs"), recursive = TRUE)
      }
      
      if (!dir.exists(ffDir)) {
        sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "source/getOneDayObs.sql"))
        sql <- SqlRender::renderSql(sql = sql, 
                                    resultsDatabaseSchema = cdmSource$resultsDatabaseSchema,
                                    vocabDatabaseSchema = cdmSource$vocabDatabaseSchema, 
                                    warnOnMissingParameters = FALSE)$sql
        sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
        
        connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
        oneDayObs <- DatabaseConnector::querySql.ffdf(connection = connection, sql = sql)
        
        ffbase::save.ffdf(oneDayObs, dir = ffDir)
        DatabaseConnector::disconnect(connection = connection)
      }
    }
    
    releases <- (readRDS(jsonPath))$releases
    for (release in releases) {
      theseSources <- cdmSources[sapply(cdmSources, function(c) c$release == release)]
      popRds <- file.path(dataPath, release, "totalPop.rds")
      if (!dir.exists(dirname(popRds))) { dir.create(path = dirname(popRds), recursive = TRUE)}
      if (!file.exists(popRds)) {
        results <- lapply(theseSources, function(cdmSource) {
          connectionDetails <- .getConnectionDetails(cdmSource)
          connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
          sql <- SqlRender::renderSql(sql = "select '@cdmSource' as cdm_source, count_value 
                                  from @resultsDatabaseSchema.achilles_results where analysis_id = 1;",
                                      cdmSource = cdmSource$name,
                                      resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
          sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
          pop <- DatabaseConnector::querySql(connection = connection, sql = sql)
          DatabaseConnector::disconnect(connection = connection)
          pop
        })
        
        totalPop <- do.call("rbind", results)
        saveRDS(object = totalPop, file = popRds)
      }
    }
    
    removeModal(session = session)
    #sql <- SqlRender::renderSql("select distinct vocabulary_version from @cdmDatabaseSchema.vocabulary where vocabulary_id = 'None'")$sql
    
  }
  
  .createAtlasLink <- function() {
    atlasUrl <- (readRDS(jsonPath))$atlasUrl
    output$atlasLink <- renderUI({
      actionButton(inputId = "btnAtlas", label = "Open Atlas", 
                   onclick = sprintf("window.open('%s', '_blank')", atlasUrl))  
    })
  }
  
  .getAllLoads <- function() {
    cdmSources <- (readRDS(jsonPath))$sources
    choices <- c()
    for (release in (readRDS(jsonPath))$releases) {
      theseSources <- cdmSources[sapply(cdmSources, function(c) c$release == release)]
      theseSources <- lapply(theseSources, function(s) { list(name = s$name, loadId = s$loadId) })
      
      items <- lapply(theseSources, function(s) {
        item <- list(s$loadId)
        names(item) <- sprintf("%s (v%d)", s$name, s$loadId)
        item
      })
      
      names(items) <- c(release)
      choices <- c(choices, items)
    }
    
    choices
  }

  .initSources <- function() {
    
    siteSource <- list(
      "All Instances" = -999
    )
    
    choices <- .getAllLoads()
      
    updateSelectInput(session = session, inputId = "cdmSource", choices = c(siteSource, choices))
  }
  
  if (!file.exists(jsonPath)) {
    updateTabItems(session = session, inputId = "tabs", selected = "config")
  } else {
    .warmCaches()
    .createAtlasLink()
    .initSources()
    updateTabItems(session = session, inputId = "tabs", selected = "overview")
  }
  
  # knowledge base --------------------------------------------------------
  
  .getPersonKb <- function() {
    
    #removeUI(selector = sprintf("#%s div:has(> .box)", parentDiv), session = session)
    
    newTabs <- list(
      tabPanel(title = "Data Quality",
               br()),
      tabPanel(title = "Chart Review",
               br())
    )

    output$kbContent <- renderUI({
      do.call(tabsetPanel, newTabs)
    })

  }
  
  .getConceptKb <- function() {
    newTabs <- list(
      tabPanel(title = "Data Quality",
               DT::dataTableOutput(outputId = "dtConceptHeels") %>% withSpinner(color = spinnerColor)),
      tabPanel(title = "Time Series",
               helpText("Time-based metadata about the selected concept"),
               h4("Click on a point on the plot to add a temporal event. Or, click on an existing temporal event in the table below in order to edit or delete it."),
               box(width = 8,
                  plotlyOutput(outputId = "metaTimePlot")  %>% withSpinner(color = spinnerColor)
               ),
               box(width = 4,
                  selectInput(inputId = "temporalStartDate", label = "Select Date", choices = c()),
                  textAreaInput(inputId = "temporalEventValue", label = "Temporal Event Description",
                                placeholder = "Enter a description of the temporal event connected to this concept at the above date"),
                  actionButton(inputId = "btnAddTemporalEvent", label = "Add", icon = icon("check")),
                  actionButton(inputId = "btnEditTemporalEvent", label = "Edit", icon = icon("edit")),
                  actionButton(inputId = "btnDeleteTemporalEvent", label = "Delete", icon = icon("minus"))),
               box(width = 12,
                  DT::dataTableOutput(outputId = "dtTemporalEvent") %>% withSpinner(color="#0dc5c1"))
               )
      )
    
    output$kbContent <- renderUI({
      do.call(tabsetPanel, newTabs)
    })
  }
  
  .getDomainKb <- function() {
    newTabs <- list(
      tabPanel(title = "Data Quality",
               DT::dataTableOutput(outputId = "dtDomainHeels") %>% withSpinner(color = spinnerColor))
    )
  }
  
  .getConceptSetKb <- function() {
    
  }
  
  .getCohortDefKb <- function() {
    
  }
  
  .getPleKb <- function() {
    
  }
  
  .getPlpKb <- function() {
    
  }

  # Heel Results download / upload --------------------------------------------
  
  heelFileInput <- reactiveValues(
    clear = FALSE
  )
  
  sourcesFileInput <- reactiveValues(
    clear = FALSE
  )

  observeEvent(input$uploadSourcesJson, {
    if (!is.null(input$uploadSourcesJson$datapath)) {
      for (f in list.files(path = dataPath, all.files = TRUE, full.names = TRUE)) {
        unlink(f)
      }
      
      json <- jsonlite::read_json(input$uploadSourcesJson$datapath)
      saveRDS(object = json, file = jsonPath)
      shinyjs::show(selector = "#sidebarCollapsed li a[data-value=overview]")
      shinyjs::show(id = "sidebarSelects")
      .initSources()
      reset(id = "uploadSourcesJson")
      sourcesFileInput$clear <- TRUE
      updateTabItems(session = session, inputId = "tabs", selected = "overview")
    }
  }, priority = 1000)
  
  observeEvent(input$uploadHeelAnnotations, {
    heelFileInput$clear <- FALSE
  }, priority = 1000)
  
  observeEvent(input$uploadSourcesJson, {
    sourcesFileInput$clear <- FALSE
  }, priority = 1000)
  
  .handleHeelResultsUpload <- function() {
    
    if (!is.null(input$uploadHeelAnnotations$datapath) & !heelFileInput$clear) {
      df <- read.csv(input$uploadHeelAnnotations$datapath,
                     header = TRUE, stringsAsFactors = FALSE, as.is = TRUE)
      for (i in 1:nrow(df)) {
        row <- df[i,]
        current <- .getHeelResults()[i,]
        
        if (row$Issue.Status %in% heelIssueTypes &
            !is.na(row$Issue.Annotation)) {
          if (current$ANNOTATION_AS_STRING != "Needs Review") {
            if (row$Issue.Status != current$ANNOTATION_AS_STRING |
                row$Issue.Annotation != current$VALUE_AS_STRING) {
              .updateHeelAnnotation(activityAsString = row$Message, 
                                    annotationAsString = row$Issue.Status, 
                                    valueAsString = row$Issue.Annotation)  
            }
          } else {
            .addHeelAnnotation(activityAsString = row$Message, 
                               annotationAsString = row$Issue.Status, 
                               valueAsString = row$Issue.Annotation)
          }
        }
      }
      
      reset(id = "uploadHeelAnnotations")
      heelFileInput$clear <- TRUE
    }
   
  }
  
  output$downloadHeelResults <- downloadHandler(
    filename = function() {
      paste('heelResults-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      df <- .getHeelResults()
      
      df <- dplyr::arrange(df, ANALYSIS_ID) %>%
        dplyr::select(`Analysis Id` = ANALYSIS_ID,
                      `Rule Id` = RULE_ID,
                      `Message` = ACHILLES_HEEL_WARNING,
                      `Record Count` = RECORD_COUNT,
                      `Issue Status` = ANNOTATION_AS_STRING,
                      `Issue Annotation` = VALUE_AS_STRING,
                      `Agent` = AGENT)
      
      write.csv(df, con)
    }
  )
  
  # WebAPI calls ----------------------------------
  
  .getCohortConceptSetConcepts <- function() {
    row_count <- input$dtCohortPicker_rows_selected
    cohortId <- cohortDefinitions()[row_count,]$ID
    
    url <- sprintf("%1s/cohortdefinition/%1d", baseUrl(), cohortId)
    vocabSourceKey <- OhdsiRTools::getPriorityVocabKey(baseUrl = baseUrl())
    
    content <- httr::content(x = httr::GET(url = url))
    json <- rjson::fromJSON(content$expression)
    
    lapply(json$ConceptSets, function(j) {
      url <- sprintf("%1s/vocabulary/%2s/resolveConceptSetExpression", baseUrl(), vocabSourceKey)
      httpheader <- c(Accept = "application/json; charset=UTF-8", `Content-Type` = "application/json")
      body <- rjson::toJSON(j$expression)
      req <- httr::POST(url, body = body, config = httr::add_headers(httpheader))
      req <- httr::content(req)
      concepts <- unlist(req)  
      list(
        id = j$id,
        name = j$name,
        concepts = concepts
      )
    })
  }
  
  output$metaTimePlot <- renderPlotly({
    req(input$conceptId)
    
    
    if (nrow(conceptsMeta()) > 0) {
      
      dates <- dplyr::select(conceptsMeta(),
                             DATE) %>%
        dplyr::distinct() %>%
        dplyr::arrange(DATE)
      
      updateSelectInput(session = session, inputId = "temporalStartDate",
                        choices = dates)
    }
    
    .refreshConceptPlot()
  })
  
  output$dtTemporalEvent <- renderDataTable(expr = {
    req(input$conceptId)
    meta <- associatedTempEvents()

    metaDataTable <- dplyr::select(meta,
                                   `Date` = DATE,
                                   `Temporal Event` = VALUE_AS_STRING)

    options <- list(pageLength = 10,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '15vh')
    selection <- list(mode = "single", target = "row")

    table <- datatable(metaDataTable,
                       options = options,
                       selection = "single",
                       rownames = FALSE,
                       class = "stripe nowrap compact", extensions = c("Responsive"))

    table
  })
  
  # CRUD buttons -----------------------------------
  
  .createCrudButtons <- function(parentDiv, crudTypes = c("Submit", "Edit", "Delete")) {
    
    suffix <- gsub(pattern = "Crud", replacement = "", x = parentDiv)
    
    divs <- lapply(crudTypes, function(crudType) {
      if (crudType == "Submit") {
        iconName <- "plus"
      } else if (crudType == "Edit") {
        iconName <- "edit"
      } else {
        iconName <- "minus"
      }
      div(style = "display:inline-block;text-align: left;padding-bottom: 20px",
          actionButton(inputId = sprintf("btnModal%1s%2s", crudType, suffix),
                       label = crudType, icon = icon(iconName)))
    })
    
    insertUI(selector = sprintf("#%s", parentDiv), session = session,
             ui = {
               divs
             })
  }
  
  .createCrudButtons(parentDiv = "SourceDescCrud", crudTypes = c("Edit"))
  #.createCrudButtons(parentDiv = "TempEventCrud", crudTypes = c("Edit", "Delete"))
  #.createCrudButtons(parentDiv = "HeelResultsCrud", crudTypes = c("Edit", "Delete"))
  
  # Reactives ---------------------------------------------------------------------
  
  persons <- reactive({
    load.ffdf(file.path(dataPath, "persons", currentSource()$name, input$cdmSource))
    df <- (as.data.frame(persons))
    
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "person", "getCountKnownPersonMeta.sql"))
    sql <- SqlRender::renderSql(sql = sql, 
                                resultsDatabaseSchema = resultsDatabaseSchema())$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    knownMeta <- DatabaseConnector::querySql(connection = connection, sql = sql)
    df <- dplyr::left_join(x = df, y = knownMeta, by = "PERSON_ID")
    df$NUM_ANNOTATIONS[is.na(df$NUM_ANNOTATIONS)] <- 0
    df$GENDER_CONCEPT_ID[df$GENDER_CONCEPT_ID == 8532] <- "Female"
    df$GENDER_CONCEPT_ID[df$GENDER_CONCEPT_ID == 8507] <- "Male"
    
    df <- dplyr::arrange(df, NUM_ANNOTATIONS) %>%
      dplyr::select(`Person Id` = PERSON_ID,
                    `Birth Year` = YEAR_OF_BIRTH,
                    `Gender` = GENDER_CONCEPT_ID,
                    `Number of Annotations` = NUM_ANNOTATIONS)
  })
  
  conceptsMeta <- reactive({
    
    refresh$conceptKb
    
    domainDf <- as.data.frame(domainConceptIds)
    domainDf$name <- rownames(domainDf)
    
    #req(input$domainId)
    
    load.ffdf(file.path(dataPath, "achillesResults", currentSource()$name, input$cdmSource))
    load.ffdf(file.path(dataPath, "oneDayObs", currentSource()$name, input$cdmSource))
    
    denoms <- as.data.frame(oneDayObs, stringsAsFactors = FALSE)
    
    denomSelects <- apply(X = denoms, MARGIN = 1, function(row) {
      sql <- SqlRender::renderSql(sql = "select 117 as ANALYSIS_ID,
                                    @stratum1 as STRATUM_1,
                                    @stratum2 as STRATUM_2, 
                                    @stratum3 as STRATUM_3,
                                    @stratum4 as STRATUM_4,
                                    @stratum5 as STRATUM_5,
                                    @countValue as COUNT_VALUE",
                           stratum1 = row["STRATUM_1"],
                           stratum2 = row["STRATUM_2"],
                           stratum3 = row["STRATUM_3"],
                           stratum4 = row["STRATUM_4"],
                           stratum5 = row["STRATUM_5"],
                           countValue = row["COUNT_VALUE"])$sql
      sql <- gsub(pattern = "NA as ", replacement = "NULL as ", x = sql)
    })
    
    result <- tryCatch({
      
      nums <- subset.ffdf(x = achillesResults, subset = CONCEPT_ID == input$conceptId)
      
      matched <- dplyr::inner_join(x = as.data.frame(nums, stringsAsFactors = FALSE), 
                                   y = denoms, by = c("STRATUM_2" = "STRATUM_1"))
      
      df <- dplyr::select(matched, STRATUM_2, COUNT_VALUE.x, COUNT_VALUE.y)
      df$COUNT_VALUE <- round(1000 * (1.0 * df$COUNT_VALUE.x / df$COUNT_VALUE.y), 5)
      
      
      cteSelects <- apply(X = df, MARGIN = 1, function(row) {
        SqlRender::renderSql(sql = "select @stratum2 as STRATUM_2, @countValue as COUNT_VALUE",
                             stratum2 = row["STRATUM_2"], 
                             countValue = row["COUNT_VALUE"])$sql
      })
      
      sql <- SqlRender::readSql(file.path(sqlRoot, "conceptExplore/prevalenceByMonthDf/getPrevalenceAndMeta.sql"))
      sql <- SqlRender::renderSql(sql = sql, 
                                  resultsDatabaseSchema = resultsDatabaseSchema(),
                                  conceptId = input$conceptId,
                                  cteSelects = paste(cteSelects, collapse = " union all "))$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
      on.exit(DatabaseConnector::disconnect(connection = connection))
      
      df <- DatabaseConnector::querySql(connection = connection, sql = sql)
      if (nrow(df) > 0) {
        df$DATE <- as.Date(paste0(df$STRATUM_2, "01"), format = "%Y%m%d")
      }
      refresh$conceptKb <- FALSE
      df
    }, error = function(e) {
      refresh$conceptKb <- FALSE
      data.frame()
    })
    
    result
  })
  
  associatedTempEvents <- function() {
    
    if (nrow(conceptsMeta()) > 0) {
      conceptsMeta()[!is.na(conceptsMeta()$VALUE_AS_STRING),]  
    } else {
      data.frame()
    }
  }
  
  currentSource <- reactive({
    req(input$cdmSource)
    cdmSources <- (readRDS(jsonPath))$sources
    siteSource <- list(
      list(name = "All Instances",
           loadId = -999)
    )
    cdmSources <- c(siteSource, cdmSources)
    cdmSources[sapply(cdmSources, function(c) c$loadId == input$cdmSource)][[1]]
  })
  
  connectionDetails <- reactive({
    req(currentSource())
    if (input$cdmSource != -999) {
      .getConnectionDetails(currentSource())
    } else {
      FALSE
    }
  })
  
  resultsDatabaseSchema <- reactive({
    if (file.exists(jsonPath)) {
      if (input$cdmSource != -999) {
        currentSource()$resultsDatabaseSchema
      } else {
        FALSE
      }
    } else {
      FALSE
    }
  })
  
  cdmDatabaseSchema <- reactive({
    if (file.exists(jsonPath)) {
      if (input$cdmSource != -999) {
        currentSource()$cdmDatabaseSchema
      } else {
        FALSE
      }
    } else {
      FALSE
    }
  })
  
  vocabDatabaseSchema <- reactive({
    if (file.exists(jsonPath)) {
      if (input$cdmSource != -999) {
        currentSource()$vocabDatabaseSchema
      } else {
        FALSE
      }
    } else {
      FALSE
    }
  })
  
  cohortDefinitions <- reactive({
    if (input$cdmSource != -999) {
      url <- sprintf("%1s/cohortdefinition", baseUrl())
      cohorts <- httr::content(httr::GET(url))
      cohorts <- lapply(cohorts, function(c) {
        data.frame(ID = c$id, Name = c$name)
      })
      do.call("rbind", cohorts)  
    } else {
      FALSE
    }
  })

  conceptSets <- reactive({
    if (input$cdmSource != -999) {
      url <- sprintf("%1s/conceptset", baseUrl())
      sets <- httr::content(httr::GET(url))
      sets <- lapply(sets, function(c) {
        data.frame(ID = c$id, Name = c$name)
      })
      do.call("rbind", sets)  
    } else {
      FALSE
    }
  })
  
  
  # Source Queries ------------------------------------------------------

  .getChartMeta <- function() {
    if (input$conceptId != "") {
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
      on.exit(DatabaseConnector::disconnect(connection = connection))
      
      sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "conceptExplore/getChartEntityActivity.sql"))
      sql <- SqlRender::renderSql(sql = sql, 
                                   resultsDatabaseSchema = resultsDatabaseSchema(),
                                   entityConceptId = input$conceptId)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql

      DatabaseConnector::querySql(connection = connection, sql = sql)
    } else {
      data.frame()
    }
  }
  
  .getSourceDescription <- function(connectionDetails,
                                    resultsDatabaseSchema) {
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "source/getDescription.sql"))
    sql <- SqlRender::renderSql(sql = sql, 
                                resultsDatabaseSchema = resultsDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  .deleteTemporalEvent <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql(sql = "select A.meta_entity_activity_id, B.meta_value_id from @resultsDatabaseSchema.meta_entity_activity A
                                      join @resultsDatabaseSchema.meta_value B on A.meta_entity_activity_id = B.meta_entity_activity_id
                                      where A.entity_concept_id = @entityConceptId and A.activity_start_date = '@activityStartDate' 
                                      and B.value_as_string = '@valueAsString';",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                entityConceptId = input$conceptId,
                                activityStartDate = input$temporalStartDate,
                                valueAsString = input$temporalEventValue)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "conceptExplore/deleteTemporalEvent.sql"))
    sql <- SqlRender::renderSql(sql = sql,
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                               metaValueId = df$META_VALUE_ID,
                               metaEntityActivityId = df$META_ENTITY_ACTIVITY_ID)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification("Temporal Event Deleted")
    removeModal(session = session)
  }
  
  .addTemporalEvent <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    metaEntityActivityId <- .getMaxId("meta_entity_activity", "meta_entity_activity_id") + 1
    
    df <- data.frame(
      meta_entity_activity_Id = metaEntityActivityId,
      meta_agent_id = as.integer(input$selectAgent),
      entity_concept_Id = as.integer(input$conceptId),
      entity_as_string = NA,
      entity_identifier = NA,
      activity_concept_id = 0,
      activity_type_concept_id = 0,
      activity_as_string = "Temporal Event",
      activity_start_date = ymd(format(as.Date(input$temporalStartDate), "%Y-%m-01")),
      activity_end_date = ymd(format(as.Date(input$temporalStartDate), "%Y-%m-01")),
      security_concept_id = 0, stringsAsFactors = FALSE
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_entity_activity", resultsDatabaseSchema()),
                                   data = df, dropTableIfExists = F, createTable = F, useMppBulkLoad = F)
    
    metaValueId <- .getMaxId(tableName = "meta_value",
                             fieldName = "meta_value_id") + 1
    
    value <- data.frame(
      meta_value_id = metaValueId,
      value_ordinal = 1, 
      meta_entity_activity_id = metaEntityActivityId,
      meta_annotation_id = NA,
      value_concept_id = 0,
      value_type_concept_id = 0,
      value_as_string = input$temporalEventValue,
      value_as_number = NA,
      operator_concept_id = 0, stringsAsFactors = FALSE
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_value", resultsDatabaseSchema()), 
                                   data = value, 
                                   dropTableIfExists = F, createTable = F)
    
    #.refreshPlotAndTable()
    
    showNotification(sprintf("New Temporal Event added"))
  }
  
  .editTemporalEvent <- function() {
    
    df <- associatedTempEvents()
    row_count <- input$dtTemporalEvent_rows_selected
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql(sql = "select B.meta_value_id from @resultsDatabaseSchema.meta_entity_activity A
                                      join @resultsDatabaseSchema.meta_value B on A.meta_entity_activity_id = B.meta_entity_activity_id
                                      where A.entity_concept_id = @entityConceptId and A.activity_start_date = '@activityStartDate' 
                                      and B.value_as_string = '@valueAsString';",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                entityConceptId = input$conceptId,
                                activityStartDate = input$temporalStartDate,
                                valueAsString = df[row_count,]$VALUE_AS_STRING)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    metaValueId <- DatabaseConnector::querySql(connection = connection, sql = sql)
    
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "conceptExplore/updateTemporalEvent.sql"))
    sql <- SqlRender::renderSql(sql = sql, 
                                 resultsDatabaseSchema = resultsDatabaseSchema(),
                                 metaValueId = metaValueId,
                                 valueAsString = input$temporalEventValue)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification("Temporal Event Changes Submitted")
  }
  
  .submitSourceDescription <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql("select A.meta_entity_activity_id, B.meta_value_id
                                  from @resultsDatabaseSchema.meta_entity_activity A
                                  join @resultsDatabaseSchema.meta_value B on A.meta_entity_activity_id = B.meta_entity_activity_id
                                  where A.entity_as_string = 'Source' and A.activity_as_string = 'Source Provenance';",
                                resultsDatabaseSchema = resultsDatabaseSchema())$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    df <- DatabaseConnector::querySql(connection = connection, sql = sql)
    
    if (nrow(df) == 0) {
      metaEntityActivityId <- .getMaxId(tableName = "meta_entity_activity",
                                        fieldName = "meta_entity_activity_id") + 1
      
      entityActivity <- data.frame(
        meta_entity_activity_id = metaEntityActivityId,
        meta_agent_id = as.integer(input$selectAgent),
        entity_concept_id = 0,
        entity_as_string = "Source",
        entity_identifier = NA,
        activity_concept_id = 0,
        activity_type_concept_id = 0,
        activity_as_string = "Source Provenance",
        activity_start_date = NA,
        activity_end_date = NA,
        security_concept_id = 0, stringsAsFactors = FALSE
      )
      
      DatabaseConnector::insertTable(connection = connection, 
                                     tableName = sprintf("%s.meta_entity_activity", 
                                                         resultsDatabaseSchema()), 
                                     data = entityActivity, 
                                     dropTableIfExists = F, createTable = F)
      
      metaValueId <- .getMaxId(tableName = "meta_value",
                               fieldName = "meta_value_id") + 1
      
      value <- data.frame(
        meta_value_id = metaValueId,
        value_ordinal = 1, 
        meta_entity_activity_id = metaEntityActivityId,
        meta_annotation_id = NA,
        value_concept_id = 0,
        value_type_concept_id = 0,
        value_as_string = input$sourceDescEdit,
        value_as_number = NA,
        operator_concept_id = 0, stringsAsFactors = FALSE
      )
      
      DatabaseConnector::insertTable(connection = connection, 
                                     tableName = sprintf("%s.meta_value", resultsDatabaseSchema()), 
                                     data = value, 
                                     dropTableIfExists = F, createTable = F)
      
    } else {
      sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "source/updateDescription.sql"))
      sql <- SqlRender::renderSql(sql = sql,
                                   resultsDatabaseSchema = resultsDatabaseSchema(),
                                   metaValueId = df$META_VALUE_ID,
                                   metaEntityActivityId = df$META_ENTITY_ACTIVITY_ID,
                                   metaAgentId = input$selectAgent,
                                   valueAsString = input$sourceDescEdit)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      
      DatabaseConnector::executeSql(connection = connection, sql = sql)
    }
    
    .createSourceOverview(cdmSource = currentSource(), parentDiv = "overviewBox", width = 12)
    showNotification("Source Description Submitted")
    
    removeModal(session = session)
  }
  
  .getSourcePopulation <- function() {
    df <- readRDS(file.path(dataPath, "totalPop.rds"))
    prettyNum(df$COUNT_VALUE[df$CDM_SOURCE == currentSource()$name], big.mark = ",")
  }
  
  .createSourceOverview <- function(cdmSource, parentDiv, width = 12) {
    connectionDetails <- .getConnectionDetails(cdmSource)
    
    #df <- readRDS(file.path(dataPath, "observationPeriods", sprintf("%s.rds", cdmSource$name)))
    load.ffdf(file.path(dataPath, "observationPeriods", currentSource()$name, input$cdmSource))
    
    df <- as.data.frame(observationPeriods, stringsAsFactors = FALSE)
    
    df$DATE <- as.Date(paste0(df$STRATUM_1, '01'), format='%Y%m%d') 
    
    plot <- plot_ly(df, x = ~DATE, y = ~COUNT_VALUE, type = "scatter", mode = "lines+markers", 
                    source = "C") %>%
      layout(xaxis = list(title = "Date", showspikes = TRUE), yaxis = list(title = "Persons With Continuous Observation By Month"))
    
    removeUI(selector = sprintf("#%s div:has(> .box)", parentDiv), session = session)
    
    insertUI(selector = sprintf("#%s", parentDiv), 
             ui = {
               shinydashboard::box(title = cdmSource$name, collapsible = TRUE, width = width,
                                   div(.getSourceDescription(connectionDetails,
                                                             cdmSource$resultsDatabaseSchema)),
                                   plot
               )
             }, session = session)
    
    output$provStartDate <- renderInfoBox({
      infoBox("Start Date", 
              min(df$DATE), color = "green",
              icon = icon("hourglass-start"), fill = TRUE)  
    })
    output$provEndDate <- renderInfoBox({
      infoBox("End Date", 
              max(df$DATE), color = "red",
              icon = icon("hourglass-end"), fill = TRUE)  
    })
    output$provNumPersons <- renderInfoBox({
      infoBox("Total Persons", 
              .getSourcePopulation(), color = "purple",
              icon = icon("users"), fill = TRUE)  
    })
  }
  
  .refreshAgents <- function() {
    df <- .getAgents()  
    humans <- df[df$META_AGENT_CONCEPT_ID == 1000,]
    algs <- df[df$META_AGENT_CONCEPT_ID == 2000,]
    
    choices <- list(`Human` = setNames(as.integer(humans$META_AGENT_ID), paste(humans$AGENT_LAST_NAME, 
                                                                               humans$AGENT_FIRST_NAME, sep = ", ")),
                    `Algorithm` = setNames(as.integer(algs$META_AGENT_ID), algs$AGENT_ALGORITHM))
    updateSelectInput(session = session, inputId = "selectAgent", choices = choices)
  }
  
  # Observes ----------------------------------------------------------
  
  observe({
    if (input$tabs == "overview") {
      
      releases <- (readRDS(jsonPath))$releases
      
      tabs <- lapply(releases, function(release) {
        cdmSources <- (readRDS(jsonPath))$sources
        cdmSources <- cdmSources[sapply(cdmSources, function(c) c$release == release)]
        
        df <- readRDS(file.path(dataPath, release, "totalPop.rds"))
        totalPop <- sum(df$COUNT_VALUE)
        
        humanAgents <- lapply(cdmSources[sapply(cdmSources, function(c) c$loadId != -999)], function(cdmSource) {
          connectionDetails <- .getConnectionDetails(cdmSource)
          connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
          sql <- SqlRender::renderSql(sql = "select agent_first_name, agent_last_name 
                                      from @resultsDatabaseSchema.meta_agent where meta_agent_concept_id = 1000;",
                                      resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
          sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
          agents <- DatabaseConnector::querySql(connection = connection, sql = sql)
          DatabaseConnector::disconnect(connection = connection)
          agents
        })
        
        algorithmAgents <- lapply(cdmSources[sapply(cdmSources, function(c) c$loadId != -999)], function(cdmSource) {
          connectionDetails <- .getConnectionDetails(cdmSource)
          connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
          sql <- SqlRender::renderSql(sql = "select agent_algorithm 
                                      from @resultsDatabaseSchema.meta_agent where meta_agent_concept_id = 2000;",
                                      resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
          sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
          agents <- DatabaseConnector::querySql(connection = connection, sql = sql)
          DatabaseConnector::disconnect(connection = connection)
          agents
        })
        
        uniqueHumans <- dplyr::distinct(do.call("rbind", humanAgents))
        uniqueAlgorithms <- dplyr::distinct(do.call("rbind", algorithmAgents))
        
        counts <- lapply(cdmSources[sapply(cdmSources, function(c) c$loadId != -999)], function(cdmSource) {
          connectionDetails <- .getConnectionDetails(cdmSource)
          connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
          sql <- SqlRender::renderSql(sql = "select count(*) from @resultsDatabaseSchema.meta_entity_activity;",
                                      resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
          sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
          count <- DatabaseConnector::querySql(connection = connection, sql = sql)
          DatabaseConnector::disconnect(connection = connection)
          as.integer(count > 0)
        })
        
        prop <- sum(unlist(counts)) / length(cdmSources)
        
        
        tabPanel(title = sprintf("Release %s", release),
                 
          infoBox("Number of CDM Instances", 
                  length(cdmSources[sapply(cdmSources, function(c) c$loadId != -999)]), 
                  icon = icon("sitemap"), fill = TRUE),
          infoBox(
            "Total Persons", prettyNum(totalPop, big.mark = ","), icon = icon("users"),
            color = "purple", fill = TRUE
          ),
          infoBox(
            "Number of Distinct Human Agents", nrow(uniqueHumans), icon = icon("user-tag"),
            color = "red", fill = TRUE
          ),
          infoBox(
            "Number of Distinct Algorithm Agents", nrow(uniqueAlgorithms), icon = icon("code"),
            color = "yellow", fill = TRUE
          ),
          infoBox(
            "Proportion of Sources with Metadata", prop * 100.00, icon = icon("percent"),
            color = "green", fill = TRUE
          )
        )
      })
      
      output$overviewBoxes <- renderUI({
        do.call(tabsetPanel, tabs)
      })
    }
    
  })
  
  
  observe({
    req(input$cdmSource)
    if (input$cdmSource != -999) {
      .refreshAgents()
      
      choices <- .getAllLoads()
      choices <- choices[sapply(choices, function(c) { c != input$cdmSource})]
      updateSelectInput(session = session, inputId = "comparatorSource", choices = choices)
    }  
  })
  
  observe({
    req(currentSource())
    
    if (input$tabs == "knowledgeBase" & currentSource()$loadId != -999
        & input$kbTabs == "Person") {
      
      df <- persons()
      
      output$dtPersonPicker <- renderDataTable({
        options <- list(pageLength = 50,
                        searching = TRUE,
                        lengthChange = FALSE,
                        ordering = TRUE,
                        paging = TRUE,
                        scrollY = '35vh')
        selection <- list(mode = "single", target = "row")
        
        table <- datatable(df,
                           options = options,
                           selection = "single",
                           rownames = FALSE, 
                           class = "stripe wrap compact", extensions = c("Responsive"))
        
        table
      })
    }
    
    
    if (input$tabs == "knowledgeBase" & currentSource()$loadId != -999) {
      if (input$kbTabs == "Domain") {
        updateSelectInput(session = session, inputId = "domainIdKb", choices = domainConceptIds)  
      }  
    
      if (input$kbTabs == "Concept" & input$kbConcept == "Time Series") {
        
        if (input$domainId == "") {
          updateSelectInput(session = session, inputId = "domainId", choices = domainConceptIds)  
          .refreshConceptId(domainConceptIds[[1]])
        }
        
        #nothing in the textbox, so can't do anything
        # if (input$temporalEventValue == "" | input$temporalStartDate == "") {
        #   shinyjs::disable(id = "btnAddTemporalEvent")
        #   shinyjs::disable(id = "btnEditTemporalEvent")
        #   shinyjs::disable(id = "btnDeleteTemporalEvent")
        # } else if (length(input$dtTemporalEvent_rows_selected) == 0) {
        #   shinyjs::enable(id = "btnAddTemporalEvent")
        #   shinyjs::disable(id = "btnEditTemporalEvent")
        #   shinyjs::disable(id = "btnDeleteTemporalEvent")
        #   #updateTextInput(session = session, inputId = "temporalEventValue", value = "")
        # } else {
        #   shinyjs::enable(id = "btnEditTemporalEvent")
        #   shinyjs::enable(id = "btnDeleteTemporalEvent")
        # }
      }
    }
  })
  
  observe({
    req(currentSource())
    input$btnSubmitHeel
    input$btnDeleteHeel
    if (currentSource()$loadId != -999) {
      shinyjs::show(id = "tasksDropdown")
      
      sourceDesc <- .getSourceDescription(connectionDetails = connectionDetails(), 
                                          resultsDatabaseSchema = resultsDatabaseSchema())
      
      if (nrow(sourceDesc) == 0) {
        sourceDesc <- list(VALUE_AS_STRING = "")
      }
      
      sourceDescItem <- taskItem(text = "Source Description Available", 
                                 value = 100 * as.integer(sourceDesc$VALUE_AS_STRING != ""), 
                                 color = "orange")  
      df <- .getHeelResults()
      ratio <- nrow(df[!is.na(df$VALUE_AS_STRING),])/nrow(df)
      
      heelItem <- taskItem(value = round(ratio * 100.00, digits = 2), color = "blue", 
                           text = "Heel Results Reviewed")
      
      allDone <- FALSE
      headerText <- "Open task(s) to review"
      if (ratio == 1 & sourceDesc$VALUE_AS_STRING != "") {
        allDone <- TRUE
        headerText <- "No tasks remaining"
      }
      output$tasksDropdown <- renderMenu({
        dropdownMenu(
          type = "tasks", badgeStatus = ifelse(allDone, "success", "danger"),
          .list = list(sourceDescItem, heelItem), headerText = headerText
        )
      })
    } else {
      hide(id = "tasksDropdown")
    }
  })
  
  observe({
    clicked <- event_data(event = "plotly_click", source = "C", session = session)
    if (!is.null(clicked)) {
      updateSelectInput(session = session, inputId = "temporalStartDate", selected = clicked$x)
    }
    
  })
  
  
  observe({
    req(input$cdmSource)
    if (input$cdmSource == -999) {
      shinyjs::hide(selector = "#sidebarCollapsed li a[data-value=provenance]")
      shinyjs::hide(selector = "#sidebarCollapsed li a[data-value=heelResults]")  
      shinyjs::hide(selector = "#sidebarCollapsed li a[data-value=knowledgeBase]")  
      shinyjs::hide(selector = "#sidebarCollapsed li a[data-value=design]") 
      
      updateTabItems(session = session, inputId = "tabs", selected = "overview")
      
    } else {
      shinyjs::show(selector = "#sidebarCollapsed li a[data-value=provenance]")
      shinyjs::show(selector = "#sidebarCollapsed li a[data-value=heelResults]")  
      shinyjs::show(selector = "#sidebarCollapsed li a[data-value=knowledgeBase]")  
      shinyjs::show(selector = "#sidebarCollapsed li a[data-value=design]")    
      updateTabItems(session = session, inputId = "tabs", selected = "provenance")
    }  
    
    
  })
  
  observe({
    req(input$cdmSource)

    if (input$tabs == "provenance" & input$cdmSource != -999) {
      .createSourceOverview(cdmSource = currentSource(), parentDiv = "overviewBox", width = 12)
    }
  }) 

  
  # Output DataTable renders ------------------------------------------
  
  output$dtSourcesOverview <- renderDataTable({
    cdmSources <- (readRDS(jsonPath))$sources
    
    df <- as.data.frame(cdmSources)
    df <- dplyr::select(df,
                        `Instance Name` = name)
    options <- list(pageLength = 10,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '15vh')
    selection <- list(mode = "single", target = "row")
    
    table <- datatable(df,
                       options = options,
                       selection = "single",
                       rownames = FALSE,
                       class = "stripe nowrap compact", extensions = c("Responsive"))
    
    table
  })
  
  output$dtCohortPicker <- renderDataTable({
    df <- cohortDefinitions() %>%
      dplyr::arrange(ID)
    
    options <- list(pageLength = 50,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '35vh')
    selection <- list(mode = "single", target = "row")
    
    table <- datatable(df,
                       options = options,
                       selection = "single",
                       rownames = FALSE, 
                       class = "stripe wrap compact", extensions = c("Responsive"))
    
    table
  })
  
  output$dtConceptSetPicker <- renderDataTable({
    df <- conceptSets() %>%
      dplyr::arrange(ID)
    
    options <- list(pageLength = 50,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '35vh')
    selection <- list(mode = "single", target = "row")
    
    table <- datatable(df,
                       options = options,
                       selection = "single",
                       rownames = FALSE, 
                       class = "stripe wrap compact", extensions = c("Responsive"))
    
    table
  })
  
  output$dtConceptSetMeta <- renderDataTable(expr = {
    
    row_count <- input$dtConceptSetPicker_rows_selected
    
    if (!is.null(row_count)) {
      conceptSetId <- conceptSets()[row_count, ]$ID
      concepts <- OhdsiRTools::getConceptSetConceptIds(baseUrl = baseUrl(), 
                                                       setId = conceptSetId)
      
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
      on.exit(DatabaseConnector::disconnect(connection = connection))
      
      sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "conceptSet/getKnownMeta.sql"))
      sql <- SqlRender::renderSql(sql = sql, 
                                  vocabDatabaseSchema = vocabDatabaseSchema(),
                                 resultsDatabaseSchema = resultsDatabaseSchema(),
                                 entityConceptIds = paste(concepts, collapse = ","))$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      
      df <- DatabaseConnector::querySql(connection = connection, sql = sql) %>%
        dplyr::select(`Concept Id` = CONCEPT_ID,
                      `Concept Name` = CONCEPT_NAME,
                      `Metadata` = VALUE_AS_STRING,
                      `Start Date` = ACTIVITY_START_DATE,
                      `End Date` = ACTIVITY_END_DATE)
    } else {
      df <- data.frame()
    }
    
    options <- list(pageLength = 10,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '15vh')
    selection <- list(mode = "single", target = "row")
    
    table <- datatable(df,
                       options = options,
                       selection = "single",
                       rownames = FALSE, 
                       class = "stripe nowrap compact", extensions = c("Responsive"))
    
    table  
  })
  
  
  
  output$dtHeelResults <- renderDataTable(expr = {
    
    input$btnSubmitHeel
    input$btnDeleteHeel


    .handleHeelResultsUpload()

    df <- .getHeelResults()

    df <- dplyr::arrange(df, ANALYSIS_ID) %>%
      dplyr::select(`Analysis Id` = ANALYSIS_ID,
                    `Rule Id` = RULE_ID,
                    `Message` = ACHILLES_HEEL_WARNING,
                    `Record Count` = RECORD_COUNT,
                    `Issue Status` = ANNOTATION_AS_STRING,
                    `Issue Annotation` = VALUE_AS_STRING,
                    `Agent` = AGENT)

    options <- list(pageLength = 50,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '50vh')
    selection <- list(mode = "single", target = "row")

    table <- datatable(df,
                       options = options,
                       selection = "single",
                       rownames = FALSE,
                       class = "stripe wrap compact", extensions = c("Responsive")) %>%
      formatStyle("Issue Status", #"Warning Type",
                  target = "row",
                  backgroundColor = styleEqual(heelIssueTypes,
                                               c("#deffc9", "#fffedb", "#ffdbdb")))

    table
  })

  observeEvent(eventExpr = input$btnGetCohortMeta, handlerExpr = {
    options <- list(pageLength = 10,
                    searching = TRUE,
                    lengthChange = FALSE,
                    ordering = TRUE,
                    paging = TRUE,
                    scrollY = '15vh')
    selection <- list(mode = "single", target = "row")
    
    showModal(
      modalDialog(size = "s",
        title = "Get Known Cohort Metadata",
        "Loading..."
      )
    )
    
    row_count <- input$dtCohortPicker_rows_selected
    cohortId <- cohortDefinitions()[row_count, ]$ID
    sets <- .getCohortConceptSetConcepts()
    
    tables <- lapply(sets, function(set) {
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
      on.exit(DatabaseConnector::disconnect(connection = connection))
      
      sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "conceptSet/getKnownMeta.sql"))
      sql <- SqlRender::renderSql(sql = sql, 
                                  vocabDatabaseSchema = vocabDatabaseSchema(),
                                 resultsDatabaseSchema = resultsDatabaseSchema(),
                                 entityConceptIds = paste(set$concepts, collapse = ","))$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      
      df <- DatabaseConnector::querySql(connection = connection, sql = sql) 
      
      if (nrow(df) > 0) {
        list(id = set$id,
             name = set$name, 
             data = dplyr::select(df,
                      `Concept Id` = CONCEPT_ID,
                      `Concept Name` = CONCEPT_NAME,
                      `Metadata` = VALUE_AS_STRING,
                      `Start Date` = ACTIVITY_START_DATE,
                      `End Date` = ACTIVITY_END_DATE))
      } else {
        NULL
      }
    })
    removeModal(session = session)
    tables <- tables[sapply(tables, function(t) !is.null(t))]
    
    newTabs <- lapply(tables, function(t) {
      tabPanel(title = t$name, 
               dataTableOutput(outputId = sprintf("kcm%d", t$id))) 
    })
    
    output$knownCohortMeta <- renderUI({
      do.call(tabsetPanel, newTabs)
    })
    
    for (t in tables) {
      output[[sprintf("kcm%d", t$id)]] <- renderDataTable(t$data)
    }
  })
  
  observeEvent(eventExpr = input$dtConceptSetPicker_rows_selected, handlerExpr = {
    row_count <- input$dtConceptSetPicker_rows_selected
    conceptSetId <- conceptSets()[row_count, ]$ID
    concepts <- OhdsiRTools::getConceptSetConceptIds(baseUrl = baseUrl(), 
                                                     setId = conceptSetId)
    
    output$includedConcepts <- renderText({ paste(concepts, collapse = ",") })
  })
  
  observeEvent(eventExpr = input$dtCohortPicker_rows_selected, handlerExpr = {
    row_count <- input$dtCohortPicker_rows_selected
    cohortId <- cohortDefinitions()[row_count, ]$ID
    
    
  })
  
  # Helpers ---------------------------------------------------
  
  agentTextInputs <- c("agentFirstName",
                       "agentLastName",
                       "agentSuffix",
                       "agentAlgorithm",
                       "agentDescription")
  
  .clearTextInputs <- function(textInputNames, submitButtonId) {
    for (t in textInputNames) {
      updateTextInput(session, t, value = "")
    }  
    if (!is.null(submitButtonId)) {
      updateActionButton(session = session, inputId = submitButtonId, label = "Add New")
    }
  }
  
  .getMaxId <- function(tableName, fieldName) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql(sql = "select max(@fieldName) as MAX_ID from @resultsDatabaseSchema.@tableName;",
                                fieldName = fieldName,
                                tableName = tableName,
                                resultsDatabaseSchema = resultsDatabaseSchema())$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    result <- DatabaseConnector::querySql(connection = connection, sql = sql)
    
    if (is.na(result)) {
      0
    } else {
      as.integer(result$MAX_ID)
    }
  }

  # Exists functions ---------------------------------------------------------------
  
  .agentRecordExists <- function() {
    df <- .getAgents() %>%
      dplyr::filter(META_AGENT_CONCEPT_ID == input$agentConceptId &
                    AGENT_FIRST_NAME == input$agentFirstName & 
                    AGENT_LAST_NAME == input$agentLastName & 
                    AGENT_SUFFIX == input$agentSuffix &
                    AGENT_ALGORITHM == input$agentAlgorithm &
                    AGENT_DESCRIPTION == input$agentDescription)

      nrow(df) > 0
  }
  
  .entityActivityRecordExists <- function() {
    df <- .getEntityActivities() %>%
      dplyr::filter(ACTIVITY_CONCEPT_ID == input$activityConceptId,
                    ACTIVITY_TYPE_CONCEPT_ID == input$activityTypeConceptId,
                    ACTIVITY_AS_STRING == input$activityAsString,
                    ENTITY_CONCEPT_ID == input$entityConceptId,
                    ENTITY_AS_STRING == input$entityAsString,
                    ENTITY_IDENTIFIER == input$entityIdentifier,
                    ACTIVITY_START_DATE == input$activityDates[1],
                    ACTIVITY_END_DATE == input$activityDates[2])
    nrow(df) > 0
  }
  
  .annotationRecordExists <- function() {
    
    row_count <- input$dtAgent_rows_selected
    metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    
    df <- .getAnnotations() %>%
      dplyr::filter(META_ENTITY_ACTIVITY_ID == metaEntityActivityId,
                    ANNOTATION_CONCEPT_ID == input$annotationConceptId,
                    ANNOTATION_TYPE_CONCEPT_ID == input$annotationTypeConceptId
                    )
    
    nrow(df) > 0
  }
  
  # Query Metadata tables -----------------------------------------------------------------
  
  .refreshConceptPlot <- function() {

    
    conceptsMeta <- conceptsMeta()
    meta <- associatedTempEvents()
    
    if (nrow(conceptsMeta) > 0) {
      if (nrow(meta) > 0) {
        plot_ly(data = conceptsMeta, x = ~DATE, y = ~COUNT_VALUE, name = "Concept Prevalance",
                type = "scatter", mode = "lines", source = "C") %>%
          add_trace(data = meta, x = ~DATE, y = ~COUNT_VALUE, text = ~VALUE_AS_STRING, 
                    name = "Temporal Event", mode = "markers") %>%
          layout(hovermode = "closest", xaxis = list(title = "Date", showspikes = TRUE), 
                 yaxis = list(title = "Prevalence Per 1000 People", showspikes = TRUE))
        
        
      } else {
        plot_ly(data = conceptsMeta, x = ~DATE, y = ~COUNT_VALUE, name = "Concept Prevalance",
                type = "scatter", mode = "lines", source = "C") %>%
          layout(xaxis = list(title = "Date"), yaxis = list(title = "Prevalence Per 1000 People"))
      }
      
    } else {
      NULL
    }
  }
  
  .getHeelResults <- function() {
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "heelResults/getHeels.sql"))
    sql <- SqlRender::renderSql(sql = sql, 
                                resultsDatabaseSchema = resultsDatabaseSchema())$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  .getAgents <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_agent
                                order by meta_agent_id;",
                                resultsDatabaseSchema = resultsDatabaseSchema())$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    DatabaseConnector::querySql(connection = connection, sql = sql)
  }
  
  .getEntityActivities <- function(subsetByAgent = TRUE) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (subsetByAgent) {
      row_count <- input$dtAgent_rows_selected
      metaAgentId <- .getAgents()[row_count, ]$META_AGENT_ID
    } else {
      metaAgentId <- NA
    }
    
    df <- tryCatch({
      sql <- SqlRender::renderSql("select * from @resultsDatabaseSchema.meta_entity_activity
                                where 1=1 {@subsetByAgent}?{and meta_agent_id = @metaAgentId}
                                order by meta_entity_activity_id;",
                                  resultsDatabaseSchema = resultsDatabaseSchema(),
                                  subsetByAgent = subsetByAgent,
                                  metaAgentId = metaAgentId)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      DatabaseConnector::querySql(connection = connection, sql = sql)  
    }, error = function(e) {
      df <- NULL 
    })
    
    df
  }
  
  .getAnnotations <- function(metaEntityActivityId = NULL) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtEntityActivity_rows_selected
    if (is.null(metaEntityActivityId)) {
      metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    }
    
    df <- tryCatch({
      sql <- SqlRender::renderSql(sql = "select * from @resultsDatabaseSchema.meta_annotation 
                                  where meta_entity_activity_id = @metaEntityActivityId
                                  order by meta_annotation_id;",
                                  resultsDatabaseSchema = resultsDatabaseSchema(),
                                  metaEntityActivityId = metaEntityActivityId)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      DatabaseConnector::querySql(connection = connection, sql = sql)  
    }, error = function(e) {
      data.frame() 
    })
    
    df
  }
  
  .getValues <- function(metaEntityActivityId = NULL, metaAnnotationId = NULL) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (is.null(metaEntityActivityId)) {
      row_count <- input$dtEntityActivity_rows_selected
      metaEntityActivityId <- .getEntityActivities()[row_count, ]$META_ENTITY_ACTIVITY_ID
    }
    
    df <- tryCatch({
      sql <- SqlRender::renderSql(sql = "select * from @resultsDatabaseSchema.meta_value 
                                  where meta_entity_activity_id = @metaEntityActivityId
                                  {@isAnnotation}?{and meta_annotation_id = @metaAnnotationId}
                                  order by meta_annotation_id;",
                                  resultsDatabaseSchema = resultsDatabaseSchema(),
                                  metaEntityActivityId = metaEntityActivityId,
                                  metaAnnotationId = metaAnnotationId,
                                  isAnnotation = !is.null(metaAnnotationId))$sql
      
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      DatabaseConnector::querySql(connection = connection, sql = sql)  
    }, error = function(e) {
      df <- NULL 
    })
    
    df
  }


  # InfoBox Renders -----------------------------------------------------
  
  output$numSources <- renderInfoBox({
    cdmSources <- (readRDS(jsonPath))$sources
    infoBox("Number of CDM Instances", 
            length(cdmSources[sapply(cdmSources, function(c) c$loadId != -999)]), 
            icon = icon("sitemap"), fill = TRUE)
  })
 
  output$numPersons <- renderInfoBox({
    df <- readRDS(file.path(dataPath, "totalPop.rds"))
    totalPop <- sum(df$COUNT_VALUE)
    infoBox(
      "Total Persons", prettyNum(totalPop, big.mark = ","), icon = icon("users"),
      color = "purple", fill = TRUE
    )
  })
  
  output$numHumanAgents <- renderInfoBox({
    cdmSources <- (readRDS(jsonPath))$sources
    agents <- lapply(cdmSources[sapply(cdmSources, function(c) c$loadId != -999)], function(cdmSource) {
      connectionDetails <- .getConnectionDetails(cdmSource)
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      sql <- SqlRender::renderSql(sql = "select agent_first_name, agent_last_name 
                                  from @resultsDatabaseSchema.meta_agent where meta_agent_concept_id = 1000;",
                                  resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
      agents <- DatabaseConnector::querySql(connection = connection, sql = sql)
      DatabaseConnector::disconnect(connection = connection)
      agents
    })
    
    uniques <- dplyr::distinct(do.call("rbind", agents))
    
    infoBox(
      "Number of Distinct Human Agents", nrow(uniques), icon = icon("user-tag"),
      color = "red", fill = TRUE
    )
  })
  
  output$numAlgorithmAgents <- renderInfoBox({
    cdmSources <- (readRDS(jsonPath))$sources
    agents <- lapply(cdmSources[sapply(cdmSources, function(c) c$loadId != -999)], function(cdmSource) {
      connectionDetails <- .getConnectionDetails(cdmSource)
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      sql <- SqlRender::renderSql(sql = "select agent_algorithm 
                                  from @resultsDatabaseSchema.meta_agent where meta_agent_concept_id = 2000;",
                                  resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
      agents <- DatabaseConnector::querySql(connection = connection, sql = sql)
      DatabaseConnector::disconnect(connection = connection)
      agents
    })
    
    uniques <- dplyr::distinct(do.call("rbind", agents))
    infoBox(
      "Number of Distinct Algorithm Agents", nrow(uniques), icon = icon("code"),
      color = "yellow", fill = TRUE
    )
  })
  
  output$propTagged <- renderInfoBox({
    cdmSources <- (readRDS(jsonPath))$sources
    counts <- lapply(cdmSources[sapply(cdmSources, function(c) c$loadId != -999)], function(cdmSource) {
      connectionDetails <- .getConnectionDetails(cdmSource)
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      sql <- SqlRender::renderSql(sql = "select count(*) from @resultsDatabaseSchema.meta_entity_activity;",
                                  resultsDatabaseSchema = cdmSource$resultsDatabaseSchema)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails$dbms)$sql
      count <- DatabaseConnector::querySql(connection = connection, sql = sql)
      DatabaseConnector::disconnect(connection = connection)
      as.integer(count > 0)
    })
    
    prop <- sum(unlist(counts)) / length(cdmSources[sapply(cdmSources, function(c) c$loadId != -999)])
    
    infoBox(
      "Proportion of Sources with Metadata", prop * 100.00, icon = icon("percent"),
      color = "green", fill = TRUE
    )
  })
  
  .refreshConceptId <- function(domainId = NULL) {
    
    if (is.null(domainId)) {
      domainId <- input$domainId
    }
    shinyjs::disable(id = "domainId")
    shinyjs::disable(id = "conceptId")
    showModal(modalDialog(title = "Initializing", "Initializing"))
    
    load.ffdf(file.path(dataPath, "achillesConcepts", currentSource()$name, input$cdmSource))
    
    selected <- subset.ffdf(achillesConcepts, subset = ANALYSIS_ID == domainId)
    selected <- as.data.frame(selected, stringsAsFactors = FALSE) %>%
      dplyr::distinct()
    
    choices <- setNames(as.integer(selected$CONCEPT_ID),
                        paste(selected$CONCEPT_ID, as.character(selected$CONCEPT_NAME), sep = " - "))
    updateSelectInput(session = session, inputId = "conceptId", choices = choices) 
    
    shinyjs::enable(id = "domainId")
    shinyjs::enable(id = "conceptId")
    removeModal(session = session)
  }
  
  # Observe Events ------------------------------------------------------
  
  observeEvent(eventExpr = input$domainId, handlerExpr = {
  # observe({
    req(currentSource())
    req(input$domainId)
    
    .refreshConceptId()
  })
  
  
  
  observeEvent(input$btnEditTemporalEvent, handlerExpr = {
    .editTemporalEvent()
    updateTextAreaInput(session = session, inputId = "temporalEventValue", value = "")
    updateTextInput(session = session, inputId = "temporalStartDate", value = "")
    
    refresh$conceptKb <- TRUE
  }, priority = 1000)
  
  observeEvent(input$btnDeleteTemporalEvent, {
    confirmSweetAlert(session = session, inputId = "confirmDeleteTempEvent", 
                    title = "Delete Temporal Event?", text = "Are you sure you want to delete this temporal event?")
  })

  observeEvent(input$confirmDeleteTempEvent, {
    if (input$confirmDeleteTempEvent) {
      .deleteTemporalEvent()
      updateTextAreaInput(session = session, inputId = "temporalEventValue", value = "")
      updateTextInput(session = session, inputId = "temporalStartDate", value = "")  
      refresh$conceptKb <- TRUE
    }
  })
  
  
  observeEvent(input$btnAddTemporalEvent, handlerExpr = {
    .addTemporalEvent()
    updateTextInput(session = session, inputId = "temporalStartDate", value = "")
    updateTextAreaInput(session = session, inputId = "temporalEventValue", value = "")
    refresh$conceptKb <- TRUE
  }, priority = 1000)
  
  observeEvent(eventExpr = input$dtPersonPicker_rows_selected, handlerExpr = {
    row_count <- input$dtPersonPicker_rows_selected
    
    df <- persons()
    
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "person", "getCohortsForPerson.sql"))
    sql <- SqlRender::renderSql(sql = sql, resultsDatabaseSchema = resultsDatabaseSchema(),
                                personId = df[row_count,]$`Person Id`)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    cohorts <- DatabaseConnector::querySql(connection = connection, sql = sql)
    
    if (length(cohorts) > 0) {
      cohorts <- lapply(cohorts, function(c) {
        setNames(c, sprintf("%d - %s", c, 
                            OhdsiRTools::getCohortDefinitionName(baseUrl = baseUrl(), definitionId = c, formatName = FALSE)))
      })
      
      updateSelectInput(session = session, inputId = "cohortsForPerson", choices = cohorts)
    }
    
  })
  
  observeEvent(eventExpr = input$dtTemporalEvent_rows_selected, handlerExpr = {
    
    # input$btnAddTemporalEvent
    # input$btnEditTemporalEvent
    # input$confirmDeleteTemporalEvent
    
    row_count <- input$dtTemporalEvent_rows_selected
    
    df <- associatedTempEvents()
    
    activityStartDate <- df[row_count,]$DATE
    valueAsString <- df[row_count,]$VALUE_AS_STRING
    updateTextInput(session = session, inputId = "temporalStartDate", value = activityStartDate)
    updateTextAreaInput(session = session, inputId = "temporalEventValue", value = valueAsString)
    
  }, priority = 1000)
  

  observeEvent(eventExpr = input$btnModalEditSourceDesc, handlerExpr = {
    showModal(modalDialog(
      title = "Edit Source Description",
      textAreaInput(inputId = "sourceDescEdit", label = "Source Description", 
                    placeholder = "Needs a source description", width = "500px", height = "300px",
                    value = .getSourceDescription(connectionDetails = connectionDetails(),
                                                  resultsDatabaseSchema = resultsDatabaseSchema())),
      actionButton(inputId = "btnEditSourceDesc", label = "Submit Changes")
    )
    )
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnEditSourceDesc, handlerExpr = {
    .submitSourceDescription()
  }, priority = 1)

  observeEvent(eventExpr = input$dtHeelResults_rows_selected, handlerExpr = {
    row_count <- input$dtHeelResults_rows_selected
    annotationAsString <- .getHeelResults()[row_count,]$ANNOTATION_AS_STRING
    
    valueAsString <- .getHeelResults()[row_count,]$VALUE_AS_STRING
    if (!is.na(annotationAsString)) {
      updateSelectInput(session = session, inputId = "heelStatus", selected = annotationAsString)
    }
    if (!is.na(valueAsString)) {
      updateTextInput(session = session, inputId = "heelAnnotation", value = valueAsString)
    } else {
      updateTextInput(session = session, inputId = "heelAnnotation", value = "")
    }
  })
  
  observeEvent(eventExpr = input$btnDeleteHeel, handlerExpr = {
    .deleteHeelAnnotation()
    updateTextInput(session = session, inputId = "heelAnnotation", value = "")
    updateSelectInput(session = session, inputId = "heelStatus", selected = "")
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnSubmitHeel, handlerExpr = {
    if (input$heelStatus == "Needs Review" | input$heelAnnotation == "") {
      showNotification(ui = "Please change Issue Status and add Annotation", type = "error")
    } else {
      row_count <- input$dtHeelResults_rows_selected
      newAnnotation <- .getHeelResults()[row_count,]$ANNOTATION_AS_STRING == "Needs Review"
      
      if (!newAnnotation) {
        .updateHeelAnnotation()
      } else {
        .addHeelAnnotation()
      }
      updateTextInput(session = session, inputId = "heelAnnotation", value = "")
      updateSelectInput(session = session, inputId = "heelStatus", selected = "")
    }
  }, priority = 1)
  
  observeEvent(input$btnAddNewAgent, handlerExpr = {
    showModal(modalDialog(
      title = "Add New Agent",
      selectInput(inputId = "agentConceptId", label = "Agent Type", 
                  choices = c("Human" = 1000, "Algorithm" = 2000), width = "250px"),
      conditionalPanel(condition = "input.agentConceptId == 1000",
                       textInput(inputId = "agentFirstName", label = "First Name", width = "250px"),
                       textInput(inputId = "agentLastName", label = "Last Name", width = "250px"),
                       textInput(inputId = "agentSuffix", label = "Suffix", width = "250px")
      ),
      conditionalPanel(condition = "input.agentConceptId == 2000",
                       textInput(inputId = "agentAlgorithm", label = "Algorithm Name", width = "250px"),
                       textAreaInput(inputId = "agentDescription", label = "Description", 
                                     rows = 4, resize = "none", width = "250px")
      ),
      actionButton(inputId = "btnSubmitAgent", label = "Add New", icon = icon("check")),
      width = 3)
    )
  }, priority = 1)
  
  observeEvent(input$btnDeleteAgent, handlerExpr = {
    
    confirmSweetAlert(inputId = "confirmDeleteAgent", session = session, title = "Delete Agent?", 
                      text = "Are you sure you want to delete this agent?")
  }, priority = 1)
  
  observeEvent(input$confirmDeleteAgent, handlerExpr = {
    if (input$confirmDeleteAgent) {
      .deleteAgent(input$selectAgent)
      df <- .getAgents()
      humans <- df[df$META_AGENT_CONCEPT_ID == 1000,]
      algs <- df[df$META_AGENT_CONCEPT_ID == 2000,]
      choices <- list(`Human` = setNames(as.integer(humans$META_AGENT_ID), paste(humans$AGENT_LAST_NAME, 
                                                                                 humans$AGENT_FIRST_NAME, sep = ", ")),
                      `Algorithm` = setNames(as.integer(algs$META_AGENT_ID), algs$AGENT_ALGORITHM))
      updateSelectInput(session = session, inputId = "selectAgent", choices = choices)  
    }
  }, priority = 1)
  
  observeEvent(input$btnEditAgent, handlerExpr = {
    
    df <- .getAgents()
    thisAgent <- df[df$META_AGENT_ID == input$selectAgent, ]
    
    if (thisAgent$META_AGENT_CONCEPT_ID == 1000) {
      showModal(modalDialog(
        title = "Edit Agent",
        textInput(inputId = "agentFirstName", label = "First Name", width = "250px"),
        textInput(inputId = "agentLastName", label = "Last Name", width = "250px"),
        textInput(inputId = "agentSuffix", label = "Suffix", width = "250px"),
        actionButton(inputId = "btnUpdateAgent", label = "Submit Changes", icon = icon("check"))
      ))
      updateTextInput(session = session, inputId = "agentFirstName", value = thisAgent$AGENT_FIRST_NAME)
      updateTextInput(session = session, inputId = "agentLastName", value = thisAgent$AGENT_LAST_NAME)
      updateTextInput(session = session, inputId = "agentSuffix", value = thisAgent$AGENT_SUFFIX)
    } else {
      showModal(modalDialog(
        title = "Edit Agent",
        textInput(inputId = "agentAlgorithm", label = "Algorithm Name", width = "250px"),
        textAreaInput(inputId = "agentDescription", label = "Description", 
                      rows = 4, resize = "none", width = "250px"),
        actionButton(inputId = "btnUpdateAgent", label = "Submit Changes", icon = icon("check"))
      ))
      updateTextInput(session = session, inputId = "agentAlgorithm", value = thisAgent$AGENT_ALGORITHM)
      updateTextInput(session = session, inputId = "agentDescription", value = thisAgent$AGENT_DESCRIPTION)
    }
  }, priority = 1)
  
  observeEvent(input$btnUpdateAgent, handlerExpr = {
    df <- .getAgents()
    metaAgentConceptId <- df$META_AGENT_CONCEPT_ID[df$META_AGENT_ID == input$selectAgent]
    
    agentFirstName <- ""
    agentLastName <- ""
    agentSuffix <- ""
    agentAlgorithm <- ""
    agentDescription <- ""
    metaAgentId <- input$selectAgent
    
    if (metaAgentConceptId == 1000) {
      agentFirstName <- input$agentFirstName
      agentLastName <- input$agentLastName
      agentSuffix <- input$agentSuffix
    } else {
      agentAlgorithm <- input$agentAlgorithm
      agentDescription <- input$agentDescription
    }
    
    .updateAgent(agentFirstName = agentFirstName,
                 agentLastName = agentLastName,
                 agentSuffix = agentSuffix,
                 agentAlgorithm = agentAlgorithm,
                 agentDescription = agentDescription,
                 metaAgentId = metaAgentId
    )
  }, priority = 1)
  
  observeEvent(eventExpr = input$btnSubmitAgent, handlerExpr = {
    .addAgent()
    df <- .getAgents()
    humans <- df[df$META_AGENT_CONCEPT_ID == 1000,]
    algs <- df[df$META_AGENT_CONCEPT_ID == 2000,]
    choices <- list(`Human` = setNames(as.integer(humans$META_AGENT_ID), paste(humans$AGENT_LAST_NAME, 
                                                                               humans$AGENT_FIRST_NAME, sep = ", ")),
                    `Algorithm` = setNames(as.integer(algs$META_AGENT_ID), algs$AGENT_ALGORITHM))
    updateSelectInput(session = session, inputId = "selectAgent", choices = choices)
    removeModal(session = session)
  }, priority = 1)
  
  observeEvent(eventExpr = input$dtAgent_rows_selected, handlerExpr = {
    updateActionButton(session = session, inputId = "btnSubmitAgent", label = "Update Selected")
    row_count <- input$dtAgent_rows_selected
    
    if (.getAgents()[row_count,]$META_AGENT_CONCEPT_ID == 1000) {
      updateSelectInput(session = session, inputId = "agentConcept", selected = agentChoices[1])
    } else {
      updateSelectInput(session = session, inputId = "agentConcept", selected = agentChoices[2])
    }
    
    for (t in agentTextInputs) {
      updateTextInput(session = session,
                      inputId = t,
                      value = .getAgents()[row_count,][toupper(SqlRender::camelCaseToSnakeCase(t))][[1]])
    }
    
  })
  

  # Crud operations -----------------------------------------------------------
  
  .addConceptAnnotation <- function() {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    
    metaEntityActivityId <- .getMaxId(tableName = "meta_entity_activity",
                                      fieldName = "meta_entity_activity_id") + 1
    
    entityActivity <- data.frame(
      meta_entity_activity_id = metaEntityActivityId,
      meta_agent_id = as.integer(input$selectAgent),
      entity_concept_id = as.integer(input$conceptId),
      entity_as_string = "",
      entity_identifier = NA,
      activity_concept_id = 0,
      activity_type_concept_id = 0,
      activity_as_string = "Temporal Event",
      activity_start_date = as.Date(input$temporalStartDate),
      activity_end_date = NA,
      security_concept_id = 0, stringsAsFactors = FALSE
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_entity_activity", resultsDatabaseSchema()), 
                                   data = entityActivity, 
                                   dropTableIfExists = F, createTable = F)
    
    metaAnnotationId <- .getMaxId(tableName = "meta_annotation",
                                  fieldName = "meta_annotation_id") + 1
    
    metaValueId <- .getMaxId(tableName = "meta_value",
                             fieldName = "meta_value_id") + 1
    
    value <- data.frame(
      meta_value_id = metaValueId,
      value_ordinal = 1, 
      meta_entity_activity_id = metaEntityActivityId,
      meta_annotation_id = NA,
      value_concept_id = 0,
      value_type_concept_id = 0,
      value_as_string = input$conceptAnnotation,
      value_as_number = NA,
      operator_concept_id = 0, stringsAsFactors = FALSE
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_value", resultsDatabaseSchema()), 
                                   data = value, 
                                   dropTableIfExists = F, createTable = F)
    
    showNotification(sprintf("New Concept Annotation added"))
  }
  
  .addHeelAnnotation <- function(activityAsString = NULL,
                                 annotationAsString = NULL, 
                                 valueAsString = NULL) {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (is.null(annotationAsString)) {
      annotationAsString <- input$heelStatus 
    }
    if (is.null(valueAsString)) {
      valueAsString <- input$heelAnnotation
    }
    if (is.null(activityAsString)) {
      row_count <- input$dtHeelResults_rows_selected
      activityAsString <- .getHeelResults()[row_count,]$ACHILLES_HEEL_WARNING
    }

    metaEntityActivityId <- .getMaxId(tableName = "meta_entity_activity",
                                      fieldName = "meta_entity_activity_id") + 1
    
    entityActivity <- data.frame(
      meta_entity_activity_id = metaEntityActivityId,
      meta_agent_id = as.integer(input$selectAgent),
      entity_concept_id = 0,
      entity_as_string = "",
      entity_identifier = NA,
      activity_concept_id = 0,
      activity_type_concept_id = 0,
      activity_as_string = activityAsString,
      activity_start_date = NA,
      activity_end_date = NA,
      security_concept_id = 0, stringsAsFactors = FALSE
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_entity_activity", resultsDatabaseSchema()), 
                                   data = entityActivity, 
                                   dropTableIfExists = F, createTable = F)
    
    metaAnnotationId <- .getMaxId(tableName = "meta_annotation",
                                    fieldName = "meta_annotation_id") + 1
    
    annotation <- data.frame(
      meta_annotation_id = metaAnnotationId,
      meta_agent_id = as.integer(input$selectAgent),
      meta_entity_activity_id = metaEntityActivityId,
      annotation_concept_id = 0,
      annotation_as_string = annotationAsString,
      annotation_type_concept_id = 0,
      security_concept_id = 0, stringsAsFactors = FALSE
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_annotation", resultsDatabaseSchema()), 
                                   data = annotation, 
                                   dropTableIfExists = F, createTable = F)
    
    metaValueId <- .getMaxId(tableName = "meta_value",
                             fieldName = "meta_value_id") + 1

    value <- data.frame(
      meta_value_id = metaValueId,
      value_ordinal = 1, 
      meta_entity_activity_id = metaEntityActivityId,
      meta_annotation_id = metaAnnotationId,
      value_concept_id = 0,
      value_type_concept_id = 0,
      value_as_string = valueAsString,
      value_as_number = NA,
      operator_concept_id = 0, stringsAsFactors = FALSE
    )
    
    DatabaseConnector::insertTable(connection = connection, 
                                   tableName = sprintf("%s.meta_value", resultsDatabaseSchema()), 
                                   data = value, 
                                   dropTableIfExists = F, createTable = F)
    
    showNotification(sprintf("New Heel Annotation added"))
  }
  
  .updateHeelAnnotation <- function(activityAsString = NULL,
                                    annotationAsString = NULL, 
                                    valueAsString = NULL) {
    
    if (is.null(annotationAsString)) {
      annotationAsString <- input$heelStatus
    }
    if (is.null(valueAsString)) {
      valueAsString <- input$heelAnnotation
    }
    
    if (is.null(activityAsString)) {
      row_count <- input$dtHeelResults_rows_selected
      activityAsString <- .getHeelResults()[row_count,]$ACHILLES_HEEL_WARNING
    }
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    metaEntityActivityId <- (.getEntityActivities(subsetByAgent = FALSE) %>%
      dplyr::filter(ACTIVITY_AS_STRING == activityAsString))$META_ENTITY_ACTIVITY_ID
    
    
    cat(metaEntityActivityId)
    
    metaAnnotationId <- .getAnnotations(metaEntityActivityId = metaEntityActivityId)$META_ANNOTATION_ID
    
    metaValueId <- .getValues(metaEntityActivityId = metaEntityActivityId, 
                              metaAnnotationId = metaAnnotationId)$META_VALUE_ID
    
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "heelResults/updateHeel.sql"))
    sql <- SqlRender::renderSql(sql = sql,
                               resultsDatabaseSchema = resultsDatabaseSchema(),
                               metaAgentId = input$selectAgent,
                               annotationAsString = annotationAsString,
                               metaAnnotationId = metaAnnotationId,
                               metaValueId = metaValueId,
                               valueAsString = valueAsString)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification("Heel Annotation Updated")
  }
  
  .deleteHeelAnnotation <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtHeelResults_rows_selected
    activityAsString <- .getHeelResults()[row_count,]$ACHILLES_HEEL_WARNING
    
    metaEntityActivityId <- (.getEntityActivities(subsetByAgent = FALSE) %>%
      dplyr::filter(ACTIVITY_AS_STRING == activityAsString))$META_ENTITY_ACTIVITY_ID
    
    metaAnnotationId <- (.getAnnotations(metaEntityActivityId = metaEntityActivityId))$META_ANNOTATION_ID
    
    metaValueId <- (.getValues(metaEntityActivityId = metaEntityActivityId,
                               metaAnnotationId = metaAnnotationId))$META_VALUE_ID
    
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "heelResults/deleteHeel.sql"))
    sql <- SqlRender::renderSql(sql = sql,
                                 resultsDatabaseSchema = resultsDatabaseSchema(),
                                 metaEntityActivityId = metaEntityActivityId,
                                 metaAnnotationId = metaAnnotationId,
                                 metaValueId = metaValueId)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql

    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification("Heel Annotation Removed")
  }
  
  .addAgent <- function() {

    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))

    if (.agentRecordExists()) {
      showNotification(ui = "Agent already exists", type = "error")
    } else {
      df <- data.frame(
        meta_agent_id = .getMaxId(tableName = "meta_agent", fieldName = "meta_agent_id") + 1,
        meta_agent_concept_id = as.integer(input$agentConceptId),
        agent_first_name = input$agentFirstName,
        agent_last_name = input$agentLastName,
        agent_suffix = input$agentSuffix,
        agent_algorithm = input$agentAlgorithm,
        agent_description = input$agentDescription, stringsAsFactors = FALSE
      )
      
      df[df == ""] <- NA
      DatabaseConnector::insertTable(connection = connection, 
                                     tableName = sprintf("%s.meta_agent", resultsDatabaseSchema()), 
                                     data = df, 
                                     dropTableIfExists = FALSE, createTable = FALSE, useMppBulkLoad = FALSE) 
      
      showNotification(sprintf("New agent added"))
      
      .clearTextInputs(textInputNames = agentTextInputs, "btnSubmitAgent")
    }
  }
  
  .addEntityActivity <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (.entityActivityRecordExists()) {
      showNotification(ui = "Entity/Activity already exists", type = "error")
    } else {
      
      row_count <- input$dtAgent_rows_selected
      metaAgentId <- .getAgents()[row_count, ]$META_AGENT_ID

      df <- data.frame(
        meta_entity_activity_Id = .getMaxId("meta_entity_activity", "meta_entity_activity_id") + 1,
        meta_agent_id = as.integer(metaAgentId),
        entity_Concept_Id = as.integer(input$entityConceptId),
        entity_As_String = input$entityAsString,
        entity_Identifier = ifelse(input$entityIdentifier == "", NA, as.integer(input$entityIdentifier)),
        activity_Concept_Id = as.integer(input$activityConceptId),
        activity_Type_Concept_Id = as.integer(input$activityTypeConceptId),
        activity_As_String = input$activityAsString,
        activity_Start_Date = input$activityDates[1],
        activity_End_Date = input$activityDates[2],
        security_Concept_Id = as.integer(input$securityConceptIdEA), stringsAsFactors = FALSE
      )
      
      DatabaseConnector::insertTable(connection = connection, 
                                     tableName = sprintf("%s.meta_entity_activity", resultsDatabaseSchema()),
                                     data = df, dropTableIfExists = F, createTable = F, useMppBulkLoad = F)
      
      showNotification(sprintf("New entity/activity added"))
      
      .clearTextInputs(entityActivityTextInputs, "btnSubmitEA")
    }
  }
  
 
  
  .updateAgent <- function(agentFirstName, 
                           agentLastName,
                           agentSuffix,
                           agentAlgorithm,
                           agentDescription,
                           metaAgentId) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "management/updateAgent.sql"))
    sql <- SqlRender::renderSql(sql = sql,
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                               agentFirstName = agentFirstName,
                               agentLastName = agentLastName,
                               agentSuffix = agentSuffix,
                               agentAlgorithm = agentAlgorithm,
                               agentDescription = agentDescription,
                               metaAgentId = metaAgentId)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    df <- .getAgents()
    humans <- df[df$META_AGENT_CONCEPT_ID == 1000,]
    algs <- df[df$META_AGENT_CONCEPT_ID == 2000,]
    choices <- list(`Human` = setNames(as.integer(humans$META_AGENT_ID), paste(humans$AGENT_LAST_NAME, 
                                                                               humans$AGENT_FIRST_NAME, sep = ", ")),
                    `Algorithm` = setNames(as.integer(algs$META_AGENT_ID), algs$AGENT_ALGORITHM))
    
    updateSelectInput(session = session, inputId = "selectAgent", choices = choices)
    
    showNotification(sprintf("Agent updated"))
    removeModal(session = session)
  }
  
  .updateEntityActivity <- function() {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    row_count <- input$dtEntityActivity_rows_selected
    
    sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "management/updateEntityActivity.sql"))
    sql <- SqlRender::renderSql(sql = sql,
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                               entityConceptId = input$entityConceptId,
                               entityAsString = input$entityAsString,
                               entityIdentifier = ifelse(input$entityIdentifier == "", "NULL", as.integer(input$entityIdentifier)),
                               activityConceptId = input$activityConceptId,
                               activityTypeConceptId = input$activityTypeConceptId,
                               activityAsString = input$activityAsString,
                               activityStartDate = input$activityDates[1],
                               activityEndDate = input$activityDates[2],
                               securityConceptId = input$securityConceptIdEA,
                               metaEntityActivityId = .getEntityActivities()[row_count,]$META_ENTITY_ACTIVITY_ID)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql

    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Entity/Activity record updated"))
  }
  
  .updateValue <- function(metaValueId, isAnnotation = FALSE) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    if (isAnnotation) {
      sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "management/updateValue.sql"))
      sql <- SqlRender::renderSql(sql = sql,
                                  resultsDatabaseSchema = resultsDatabaseSchema(),
                                 metaValueId = metaValueId,
                                 valueOrdinal = input$valueOrdinalAnn,
                                 valueConceptId = input$valueConceptIdAnn,
                                 valueTypeConceptId = input$valueTypeConceptIdAnn,
                                 valueAsString = input$valueAsStringAnn,
                                 valueAsNumber = input$valueAsNumberAnn,
                                 operatorConceptId = input$operatorConceptIdAnn)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      
    } else {
      sql <- SqlRender::readSql(sourceFile = file.path(sqlRoot, "management/updateValue.sql"))
      sql <- SqlRender::renderSql(sql = sql, 
                                  resultsDatabaseSchema = resultsDatabaseSchema(),
                                   metaValueId = metaValueId,
                                   valueOrdinal = input$valueOrdinalEA,
                                   valueConceptId = input$valueConceptIdEA,
                                   valueTypeConceptId = input$valueTypeConceptIdEA,
                                   valueAsString = input$valueAsStringEA,
                                   valueAsNumber = input$valueAsNumberEA,
                                   operatorConceptId = input$operatorConceptIdEA)$sql
      sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
      
    }
    
    
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Value record updated"))
  }
  
  .deleteAgent <- function(metaAgentId) {
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_agent where meta_agent_id = @metaAgentId;",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                metaAgentId = metaAgentId)$sql
    
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Agent deleted"))
  }
  

  .deleteValue <- function(metaValueId) {
    
    connection <- DatabaseConnector::connect(connectionDetails = connectionDetails())
    on.exit(DatabaseConnector::disconnect(connection = connection))
    sql <- SqlRender::renderSql("delete from @resultsDatabaseSchema.meta_value
                                where meta_value_id = @metaValueId;",
                                resultsDatabaseSchema = resultsDatabaseSchema(),
                                metaValueId = metaValueId)$sql
    sql <- SqlRender::translateSql(sql = sql, targetDialect = connectionDetails()$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql)
    
    showNotification(sprintf("Value record deleted"))
  }
  

})




# selectInput(inputId = "cohortsForPerson", multiple = FALSE, label = "Pick a Cohort", choices = c(), width = "400px"))