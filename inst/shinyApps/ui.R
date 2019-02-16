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
                                        conditionalPanel(condition = "input.tabs == 'knowledgeBase'",
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
      menuItem("Provenance", tabName = "provenance", icon = icon("database")),
      menuItem("Knowledge Base", tabName = "knowledgeBase", icon = icon("brain")),
      menuItem("Data Quality", tabName = "heelResults", icon = icon("table")),
      #menuItem("Design", tabName = "design", icon = icon("drafting-compass")),
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
              # fluidRow(
              #   infoBoxOutput(outputId = "numSources"),
              #   infoBoxOutput(outputId = "numPersons"),
              #   infoBoxOutput(outputId = "numHumanAgents"),
              #   infoBoxOutput(outputId = "numAlgorithmAgents"),
              #   infoBoxOutput(outputId = "propTagged")
              # )
              uiOutput(outputId = "overviewBoxes")
              ),
      tabItem("provenance",
              h3("Source Provenance"),
              helpText("Create and view metadata about the selected CDM instance's provenance"),
              fluidRow(
                div(id = "SourceDescCrud"),
                div(id = "overviewBox",
                    infoBoxOutput(outputId = "provStartDate"),
                    infoBoxOutput(outputId = "provEndDate"),
                    infoBoxOutput(outputId = "provNumPersons"))  
              )
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
                                tabPanel("Person",
                                         DT::dataTableOutput(outputId = "dtPersonPicker") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("Concept",
                                         div(style = "display:inline-block;text-align: left; width: 200px;",
                                             selectInput(inputId = "domainId", label = "Domain", selectize = TRUE,
                                                         choices = c(), width = "200px")),
                                         div(style = "display:inline-block;text-align: left; width: 50px;"),
                                         div(style = "display:inline-block;text-align: left; width: 600px;",
                                             selectInput(inputId = "conceptId", label = "Pick a concept", selectize = TRUE,
                                                         choices = c()), width = "600px"),
                                         div(style = "display:inline-block;text-align: left; width: 50px;",
                                             checkboxInput(inputId = "descendants", label = "Apply to descendant concepts?"))),
                                tabPanel("Domain",
                                         selectInput(inputId = "domainIdKb", label = "Domain", selectize = TRUE,
                                                     choices = c(), width = "200px")),
                                tabPanel("Concept Set",
                                          DT::dataTableOutput(outputId = "dtConceptSetPicker") %>% withSpinner(color="#0dc5c1")
                                ),
                                tabPanel("Cohort Definition",
                                         DT::dataTableOutput(outputId = "dtCohortPicker") %>% withSpinner(color="#0dc5c1")))
                )
                ),
              fluidRow(
                # Viewer Box
                box(width = 12,
                    uiOutput(outputId = "selectedElement"),
                    conditionalPanel(condition = "input.kbTabs == 'Concept'",
                                     tabsetPanel(type = "pills", id = "kbConcept",
                                                 tabPanel("Data Quality"),
                                                 tabPanel("Time Series",
                                                          fluidRow(
                                                            box(width = 8, 
                                                                plotlyOutput(outputId = "metaTimePlot")  %>% withSpinner(color = spinnerColor)
                                                            ),
                                                            box(width = 4,
                                                                DT::dataTableOutput(outputId = "dtTemporalEvent") %>% withSpinner(color="#0dc5c1")))  
                                                          )
                                                 )
                                     )
                    ),
                # Editor Box
                box(width = 12,
                    
                    conditionalPanel(condition = "input.kbTabs == 'Person'",
                                     tabsetPanel(type = "pills", id = "kbPerson",
                                                 tabPanel("Data Quality",
                                                          selectInput(inputId = "dqActivityPerson", label = "Activity: What DQ phenomenon is happening to this person?", 
                                                                      choices = c("Conformance: A value in the person's history does not adhere to an external or internal standard" = "conformance",
                                                                                  "Completeness: A value in the person's history is not present" = "completeness",
                                                                                  "Plausibility: A value in the person's history is not believable" = "plausibility"))),
                                                          selectInput(inputId = "dqActivityTypePerson", label = "Activity Type: A more granular way to define the DQ phenomenon happening to this person", 
                                                                      width = "250px", choices = c()),
                                                          div(style = "display:inline-block;text-align: left; width: 250px;", 
                                                            dateInput(inputId = "startDateDqPerson", label = "Activity Date: When did this phenomenon start to happen in the person's history?", width = "250px")),
                                                          div(style = "display:inline-block;text-align: left; width: 250px;", 
                                                            dateInput(inputId = "endDateDqPerson", label = "Activity Date: When did this phenomenon stop happening for this person?", width = "250px")),
                                                          br(), br(),
                                                          actionButton(inputId = "btnDqAddValues", label = "Add Values associated with the phenomenon (numbers, descriptions)", icon = icon("plus"))
                                                 
                                                 )
                                     ),
                    conditionalPanel(condition = "input.kbTabs == 'Concept'",
                                     tabsetPanel(type = "pills", id = "kbConcept",
                                                 tabPanel("Data Quality"),
                                                 tabPanel("Time Series",
                                                          box(width = 4,
                                                              selectInput(inputId = "temporalStartDate", label = "Select Date", choices = c()),
                                                              textAreaInput(inputId = "temporalEventValue", label = "Temporal Event Description",
                                                                placeholder = "Enter a description of the temporal event connected to this concept at the above date"),
                                                              actionButton(inputId = "btnAddTemporalEvent", label = "Add", icon = icon("check")),
                                                              actionButton(inputId = "btnEditTemporalEvent", label = "Edit", icon = icon("edit")),
                                                              actionButton(inputId = "btnDeleteTemporalEvent", label = "Delete", icon = icon("minus")))
                                                 )
                                     )
                    ),
                    conditionalPanel(condition = "input.kbTabs == 'Domain'",
                                     tabsetPanel(type = "pills", id = "kbPerson",
                                                 tabPanel("Data Quality"))
                    )
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