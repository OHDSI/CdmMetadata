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
                       uiOutput(outputId = "atlasLink"),
                       selectInput(inputId = "cdmSource", label = "CDM Instance", choices = c(), width = "250px"),
                       conditionalPanel(condition = "input.cdmSource != '-999'",
                                        conditionalPanel(condition = "input.tabs == 'knowledgeBase' & input.kbTabs != 'Person'",
                                                         selectInput(inputId = "comparatorSource", multiple = TRUE,
                                                                     label = "(Optional) Pick CDM(s) to compare", choices = c(), width = "250px")),
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
      menuItem("Knowledge Base", tabName = "knowledgeBase", icon = icon("brain")),
      menuItem("Data Quality", tabName = "heelResults", icon = icon("table")),
      menuItem("Design", tabName = "design", icon = icon("drafting-compass")),
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
              ),
              fluidRow(
                box(width = 10,
                    actionButton(inputId = "btnClearAllCaches", label = "Clear all Caches", icon = icon("trash")))
              )
              ),
      tabItem("overview",
              h3("Site Overview"),
              helpText("View aggregated metadata about all CDM instances in the site"),
              uiOutput(outputId = "overviewBoxes")
              ),
      tabItem("design",
              h3("ETL/Design"),
              helpText("View the ETL and design choices made to create the selected CDM instance")
              ),
      tabItem("knowledgeBase",
              h3("Knowledge Base"),
              helpText("Create and view metadata about elements within the CDM instance"),
              fluidRow(
                box(width = 12, 
                    h4("Select an element below to explore/author metadata about the element"),
                    tabsetPanel(type = "pills", id = "kbTabs",
                                tabPanel("Database"),
                                tabPanel("Domain",
                                         selectInput(inputId = "domainIdKb", label = "Domain", selectize = TRUE,
                                                     choices = c(), width = "200px")),
                                tabPanel("Cohort Definition",
                                         DT::dataTableOutput(outputId = "dtCohortPicker") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("Concept Set",
                                         DT::dataTableOutput(outputId = "dtConceptSetPicker") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("Concept",
                                         div(style = "display:inline-block;text-align: left; width: 200px;",
                                             selectInput(inputId = "domainId", label = "Domain", selectize = TRUE,
                                                         choices = c(), width = "200px")),
                                         div(style = "display:inline-block;text-align: left; width: 50px;"),
                                         div(style = "display:inline-block;text-align: left; width: 600px;",
                                             selectInput(inputId = "conceptId", label = "Pick a concept", selectize = TRUE,
                                                         choices = c()), width = "600px")),
                                tabPanel("Person",
                                         DT::dataTableOutput(outputId = "dtPersonPicker") %>% withSpinner(color="#0dc5c1"))
                    )
                )
                ),
              fluidRow(
                # Viewer Box
                box(width = 12,
                    #h4("Known Metadata"),
                    conditionalPanel(condition = "input.kbTabs == 'Database'",
                                     fluidRow(
                                       infoBoxOutput(outputId = "provStartDate"),
                                       infoBoxOutput(outputId = "provEndDate"),
                                       infoBoxOutput(outputId = "provNumPersons"),
                                       box(width = 6,
                                           h4("CDM Description"),
                                           helpText("A free-text description about the selected CDM instance"),
                                           textOutput(outputId = "sourceDescription"),
                                           br(),br(),
                                           div(id = "SourceDescCrud")),
                                       box(width = 6,
                                           h4("Source Data Dictionary"),
                                           helpText("View/edit the data dictionary of the source database. 
                                                    The data dictionary should include field names, field descriptions, and provide the manner in which the field was collected"),
                                           DT::dataTableOutput(outputId = "dtDataDictionary"),
                                           fileInput(inputId = "uploadDictionary", label = "Upload Data Dictionary (FIELD_NAME, FIELD_DESCRIPTION, FIELD_PROVENANCE)",
                                                     multiple = FALSE,
                                                     accept = c(".csv"))
                                           ),
                                       box(width = 12,
                                         plotlyOutput(outputId = "dbTotalRowsPlot")  %>% withSpinner(color = spinnerColor),
                                         br(), br(),
                                         plotlyOutput(outputId = "dbRecordsPerPersonPlot")  %>% withSpinner(color = spinnerColor)
                                       )
                                     )),
                    conditionalPanel(condition = "input.kbTabs == 'Person'",
                                     tabsetPanel(type = "pills", id = "kbPerson",
                                                 tabPanel("Data Quality",
                                                          DT::dataTableOutput(outputId = "dtPersonDq")))
                                     ),
                    conditionalPanel(condition = "input.kbTabs == 'Concept'",
                                     tabsetPanel(type = "pills", id = "kbConcept",
                                                 tabPanel("Data Quality",
                                                          DT::dataTableOutput(outputId = "dtConceptDq")),
                                                 tabPanel("Time Series",
                                                          fluidRow(
                                                            box(width = 8, 
                                                                plotlyOutput(outputId = "metaConceptTimePlot")  %>% withSpinner(color = spinnerColor)
                                                            ),
                                                            box(width = 4,
                                                                DT::dataTableOutput(outputId = "dtConceptTemporalEvent") %>% withSpinner(color="#0dc5c1"),
                                                                DT::dataTableOutput(outputId = "dtConceptKronosEvents") %>% withSpinner(color="#0dc5c1")
                                                            )
                                                          )
                                                 )
                                              )
                                     ),
                    conditionalPanel(condition = "input.kbTabs == 'Domain'",
                                     tabPanel("Data Quality",
                                              DT::dataTableOutput(outputId = "dtDomainDq")),
                                     tabPanel("Time Series",
                                              fluidRow(
                                                box(width = 8, 
                                                    plotlyOutput(outputId = "domainTotalRowsPlot")  %>% withSpinner(color = spinnerColor),
                                                    plotlyOutput(outputId = "domainRecordsPerPersonPlot")  %>% withSpinner(color = spinnerColor)
                                                ),
                                                box(width = 4,
                                                    DT::dataTableOutput(outputId = "dtDomainTemporalEvent") %>% withSpinner(color="#0dc5c1")
                                                )
                                              )
                                     ))
                    ),
                # Editor Boxes
                  conditionalPanel(condition = "input.kbTabs == 'Concept' & input.kbConcept == 'Time Series'",
                                   box(width = 12,
                                     h4("Manage Metadata"),
                                     selectInput(inputId = "temporalStartDate", label = "Select Date", choices = c()),
                                     textAreaInput(inputId = "temporalEventValue", label = "Temporal Event Description",
                                     placeholder = "Enter a description of the temporal event connected to this concept at the above date"),
                                     actionButton(inputId = "btnAddTemporalEvent", label = "Add", icon = icon("check")),
                                     actionButton(inputId = "btnEditTemporalEvent", label = "Edit", icon = icon("edit")),
                                     actionButton(inputId = "btnDeleteTemporalEvent", label = "Delete", icon = icon("minus")),
                                     checkboxInput(inputId = "cbConceptDescendants", label = "Apply action to all descendant concepts?"))
                                   ),
                  conditionalPanel(condition = "input.kbTabs == 'Person' & input.kbPerson == 'Data Quality'",
                                   box(width = 12,
                                     h4("Manage Metadata"),
                                    actionButton(inputId = "btnAddPersonDq", label = "Add Metadata"),
                                    actionButton(inputId = "btnEditPersonDq", label = "Edit Selected Metadata"),
                                    actionButton(inputId = "btnDeletePersonDq", label = "Delete Metadata"))
                                  )
              )
      ),
      tabItem("heelResults",
              h3("Data Quality: Achilles Heel Results"),
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
              )
      )
  )
)