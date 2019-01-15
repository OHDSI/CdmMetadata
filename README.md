# CdmMetadata

## An R Package / Shiny Application to Prototype CDM Metadata Tables

### Screenshots

**CdmMetadata** is an R package and Shiny Application developed by the OHDSI Metadata and Annotations Working Group in order to help prove the value of adding 4 new metadata tables to the CDM results schema:

![Site Overview: See aggregated metadata for all CDM instances in a site](extras/siteOverview.png?raw=true "Site Overview")

![Concept Knowledge Base: Create and view metadata about a concept within a CDM instance](extras/conceptKb.png?raw=true "Concept Knowledge Base")

1. **meta_agent**: A table that captures the agent of the metadata records.
2. **meta_entity_activity**: A table for capturing activities about metadata entities, which we define as objective facts about the CDM database or its usage that can be observed through query or obtained from other sources
3. **meta_annotation**: A table for capturing annotations, which we define as subjective assertions about record(s) in entity_activity from subject matter experts
4. **meta_value**: A table that is used to capture values associated with entity_activity or annotation record(s). Values can be represented in various formats and can be ordered using the value_ordinal field


###  Prerequisites

1. A CDM database with populated CDM tables and vocab tables in the CDM schema
2. Achilles tables in the results schema
3. Read/write permission on the results schema of the CDM database
4. Metadata tables created in the results schema (see Preparing CDMs)
5. A JSON configuration file, as demonstrated here: [sampleSources.json](extras/sampleSources.json)


### Preparing CDMs

To prepare a CDM database, run:

```r

connectionDetails <- DatabaseConnector::createConnectionDetails(
      dbms="redshift", 
      server="server.com", 
      user="secret", 
      password='secret', 
      port="5439")

CdmMetadata::createTables(
  connectionDetails = connectionDetails, 
  resultsDatabaseSchema = "results", 
  outputFolder = "output"
)
```



### Deploying the Shiny application

* CdmMetadata can be run from RStudio by opening inst/shinyApps/ui.R or inst/shinyApps/server.R and hitting the Run App button in RStudio.
* CdmMetadata can also be deployed to an R Shiny Server: just copy the files within inst/shinyApps to the R Shiny Server


### Technology

CdmMetadata is an R package with a Shiny application

### Getting Involved

If interested in improving this application, please add new feature enhancements or bugs to the Issue tracker. Also, join our next Metadata and Annotations WG call: http://www.ohdsi.org/web/wiki/doku.php?id=projects:workgroups:metadata_and_annotations

