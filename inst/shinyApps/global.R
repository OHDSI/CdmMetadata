dataPath <- file.path(getwd(), "data")
achillesConceptsRoot <- file.path(dataPath, "achillesConcepts")
sqlRoot <- file.path(getwd(), "sql")
csvRoot <- file.path(getwd(), "csv")

if (!dir.exists(dataPath)) {
  dir.create(path = dataPath, recursive = TRUE)
}

sourcesPath <- file.path(dataPath, "sources.rds")
jsonPath <- file.path(getwd(), "json")

domainConceptIds <- c("Condition" = 402,
                      "Procedure" = 602, "Drug" = 702, "Measurement" = 1801, "Observation" = 802)

spinnerColor <- "#0dc5c1"
heelIssueTypes <- c("Non-issue", "Needs Review", "Issue")
annotationConceptIds <- c(
  "Suggestion",
  "Warning",
  "Adjudication",
  "Issue",
  "Non-Issue",
  "Needs Review"
)

dqActivityConceptIds <- c("Conformance - Validation", 
                          "Conformance - Verification",
                          "Plausibility - Validation",
                          "Plausibility - Verification",
                          "Completeness - Validation",
                          "Completeness - Verification")