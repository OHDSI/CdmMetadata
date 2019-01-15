library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(DT)
library(shinydashboard)
library(shinycssloaders)

ui <- dashboardPage(
  dashboardHeader(title = "CDM METADATA", titleWidth = "300px",
                  dropdownMenuOutput(outputId = "tasksDropdown"),
                  tags$li(a(href = 'http://www.ohdsi.org', target = "_blank",
                            img(src = 'ohdsi_logo_mini.png',
                                title = "OHDSI", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  dashboardSidebar(width = "300px",
                   useShinyjs(),
                   div(id = "sidebarSelects",
                       selectInput(inputId = "cdmSource", label = "CDM Instance", choices = c(), width = "250px"),
                       conditionalPanel(condition = "input.cdmSource != 'All Instances'",
                                        selectInput(inputId = "selectAgent", label = "Select Agent", choices = c(), width = "250px"),
                                        
                                        div(style = "display:inline-block;text-align: left;",
                                            actionButton("btnAddNewAgent", label = "Add", icon = icon("plus"))),
                                        div(style = "display:inline-block;text-align: left;",
                                            actionButton("btnDeleteAgent", label = "Delete", icon = icon("minus"))),
                                        div(style = "display:inline-block;text-align: left;",
                                            actionButton("btnEditAgent", label = "Edit", icon = icon("edit")))
                       )),
    sidebarMenu(
      id = "tabs",
      menuItem("Site Overview", tabName = "overview", icon = icon("sitemap")),
      menuItem("Source Provenance", tabName = "provenance", icon = icon("database")),
      menuItem("Heel Results", tabName = "heelResults", icon = icon("table")),
      menuItem("Concept Knowledge Base", tabName = "conceptKb", icon = icon("line-chart")),
      menuItem("Concept Set Knowledge Base", tabName = "conceptSetKb", icon = icon("list")),
      menuItem("Cohort Knowledge Base", tabName = "cohortDefKb", icon = icon("globe")),
      menuItem("Configuration", tabName = "config", icon = icon("gear"), selected = TRUE)
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem("config",
              h3("Sources Configuration"),
              helpText("Upload a sources JSON configuration file"),
              fluidRow(
                column(10,
                       fileInput(inputId = "uploadSourcesJson", label = "Upload Sources JSON Config File",
                                 multiple = FALSE,
                                 accept = c(".json"))
                )
              )
              ),
      tabItem("overview",
              h3("Site Overview"),
              helpText("View aggregated metadata about all CDM instances in the site"),
              fluidRow(
                infoBoxOutput(outputId = "numSources"),
                infoBoxOutput(outputId = "numPersons"),
                infoBoxOutput(outputId = "numHumanAgents"),
                infoBoxOutput(outputId = "numAlgorithmAgents"),
                infoBoxOutput(outputId = "propTagged")
              )
              ),
      tabItem("provenance",
              h3("Source Provenance"),
              helpText("Create and view metadata about the selected CDM instance's provenance"),
              fluidRow(
                div(id = "SourceDescCrud"),
                div(id = "overviewBox")  
              )
              ),
      tabItem("conceptKb", 
              h3("Concept Knowledge Base"),
              helpText("Create and view known metadata about a concept in the selected CDM instance"),
              fluidRow(
                box(width = 3, 
                    h4("Select a domain and then a concept to view metadata about the concept."),
                    selectInput(inputId = "domainId", label = "Domain", selectize = TRUE,
                                           choices = c()),
                    selectInput(inputId = "conceptId", label = "Pick a concept", width = "400px", selectize = TRUE,
                                choices = c())
                ),
                column(9,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Temporal Events",
                                            helpText("Time-based metadata about the selected concept"),
                                            h4("Click on a point on the plot to add a temporal event. Or, click on an existing temporal event in the table below in order to edit or delete it."),
                                            box(width = 8, 
                                                plotlyOutput(outputId = "conceptKbPlot")  %>% withSpinner(color = spinnerColor) 
                                            ),
                                            box(width = 4,
                                                selectInput(inputId = "conceptStartDate", label = "Select Date", choices = c()),
                                                textAreaInput(inputId = "temporalEventValue", label = "Temporal Event Description", 
                                                              placeholder = "Enter a description of the temporal event connected to this concept at the above date"),
                                                actionButton(inputId = "btnAddTemporalEvent", label = "Add", icon = icon("check")),
                                                actionButton(inputId = "btnEditTemporalEvent", label = "Edit", icon = icon("edit")),
                                                actionButton(inputId = "btnDeleteTemporalEvent", label = "Delete", icon = icon("minus"))),
                                            box(width = 12,
                                                DT::dataTableOutput(outputId = "dtTemporalEvent") %>% withSpinner(color="#0dc5c1"))
                                            )
                                   )
                       )
                
              )
              ),
      tabItem("heelResults",
              h3("Achilles Heel Results"),
              helpText("Annotate and view Data Quality results from Achilles Heel"),
              fluidRow(
                box(width = 9, DT::dataTableOutput(outputId = "dtHeelResults") %>% withSpinner(color = spinnerColor)),
                box(width = 3, 
                    div(strong("(1) Download Current Heel Results"), downloadButton(outputId = "downloadHeelResults", label = "Download CSV")),
                    fileInput(inputId = "uploadHeelAnnotations", label = "(2) Annotate the CSV and re-upload",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    selectInput(inputId = "heelStatus", label = "Heel Status", selectize = TRUE,
                                           choices = c("",
                                                       "Needs Review", #= 900000519,
                                                       "Non-issue", # = 900000518,
                                                       "Issue"), # = 900000517),
                                width = "250px"),
                    textAreaInput(inputId = "heelAnnotation", label = "Heel Annotation", placeholder = "None selected",
                                  rows = 4, resize = "none", width = "250px"),
                    div(style = "display:inline-block;text-align: left;",
                        actionButton(inputId = "btnSubmitHeel", label = "Submit", icon = icon("check"))),
                    div(style = "display:inline-block;text-align: left;",
                        actionButton(inputId = "btnDeleteHeel", label = "Remove", icon = icon("minus"))))
              )
              ),
      tabItem("conceptSetKb",
              h3("Concept Set Knowledge Base"),
              helpText("Explore known metadata about a concept set in Atlas"),
              fluidRow(
                column(4, align = "center",
                       h3(img(src = "atlas_logo.png", height = "100px"), "Atlas Concept Sets"),
                       DT::dataTableOutput(outputId = "dtConceptSetPicker") %>% withSpinner(color="#0dc5c1")),
                column(8, align = "center",
                       DT::dataTableOutput(outputId = "dtConceptSetMeta") %>% withSpinner(color="#0dc5c1"))
              )
              ),
      tabItem("cohortDefKb",
              h3("Cohort Knowledge Base"),
              helpText("Explore known metadata about a cohort definition"),
              fluidRow(
                column(5, align = "center",
                       h3(img(src = "atlas_logo.png", height = "100px"), "Atlas Cohorts"),
                       DT::dataTableOutput(outputId = "dtCohortPicker") %>% withSpinner(color="#0dc5c1")),
                column(7, align = "center",
                       actionButton(inputId = "btnGetCohortMeta", label = "Fetch Known Metadata", icon = icon("database")),
                       br(),
                       uiOutput(outputId = "knownCohortMeta")
                )
              )
          )
      )
  )
)