# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of "PatientLevelPredictionValidationModule
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Module methods -------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

getSharedResourceByClassName <- function(sharedResources, className) {
  returnVal <- NULL
  for (i in 1:length(sharedResources)) {
    if (className %in% class(sharedResources[[i]])) {
      returnVal <- sharedResources[[i]]
      break
    }
  }
  invisible(returnVal)
}

createCohortDefinitionSetFromJobContext <- function(sharedResources, settings) {
  cohortDefinitions <- list()
  if (length(sharedResources) <= 0) {
    stop("No shared resources found")
  }
  cohortDefinitionSharedResource <- getSharedResourceByClassName(sharedResources = sharedResources, 
                                                                 class = "CohortDefinitionSharedResources")
  if (is.null(cohortDefinitionSharedResource)) {
    stop("Cohort definition shared resource not found!")
  }
  cohortDefinitions <- cohortDefinitionSharedResource$cohortDefinitions
  if (length(cohortDefinitions) <= 0) {
    stop("No cohort definitions found")
  }
  cohortDefinitionSet <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in 1:length(cohortDefinitions)) {
    cohortJson <- cohortDefinitions[[i]]$cohortDefinition
    cohortDefinitionSet <- rbind(cohortDefinitionSet, data.frame(
      cohortId = as.integer(cohortDefinitions[[i]]$cohortId),
      cohortName = cohortDefinitions[[i]]$cohortName,
      json = cohortJson,
      stringsAsFactors = FALSE
    ))
  }
  return(cohortDefinitionSet)
}

# Module methods -------------------------
execute <- function(jobContext) {
  rlang::inform("Validating inputs")
  inherits(jobContext, 'list')

  if (is.null(jobContext$settings)) {
    stop("Analysis settings not found in job context")
  }
  if (is.null(jobContext$sharedResources)) {
    stop("Shared resources not found in job context")
  }
  if (is.null(jobContext$moduleExecutionSettings)) {
    stop("Execution settings not found in job context")
  }
  
  workFolder <- jobContext$moduleExecutionSettings$workSubFolder
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  
  rlang::inform("Executing PLP Validation")
  moduleInfo <- getModuleInfo()
  
  # Creating database details list
  
  if(is.null(jobContext$settings$targetId)){
    tLength <- 1
  } else{
    tLength <- length(jobContext$settings$targetId)
  }
  if(is.null(jobContext$settings$outcomeId)){
    oLength <- 1
  } else{
    oLength <- length(jobContext$settings$outcomeId)
  }  
  
  databaseDetails <- list()
  ind <- 0
  for(tind in 1:tLength){
    for(oind in 1:oLength){ 
      ind <- ind + 1
      databaseDetails[[ind]] <- PatientLevelPrediction::createDatabaseDetails(
        connectionDetails = jobContext$moduleExecutionSettings$connectionDetails, 
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cdmDatabaseName = paste0(jobContext$moduleExecutionSettings$connectionDetailsReference,'_T',tind,'_O',oind ),
        cdmDatabaseId = jobContext$moduleExecutionSettings$databaseId,
        #tempEmulationSchema =  , is there s temp schema specified anywhere?
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, 
        outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema, 
        outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, 
        targetId = jobContext$settings$targetId[tind], # could make this a list
        outcomeIds = jobContext$settings$outcomeId[oind] # could make this a list
      )
    }
  }
  
  # find where cohortDefinitions are as sharedResources is a list
  cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(
    sharedResources = jobContext$sharedResources,
    settings = jobContext$settings
    )
  
  # check the model locations are valid and apply model

  modelLocationList <- jobContext$settings$modelLocationList
  
  modelInd <- 0
  for(modelLocation in modelLocationList){   
    modelInd <- modelInd + 1
    plpModel <- tryCatch(
      {PatientLevelPrediction::loadPlpModel(modelLocation)},
      error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    )
    if(!is.null(plpModel)){

      # append model ind to ensure analysis id is unique
      plpModel$trainDetails$analysisId <- paste0(plpModel$trainDetails$analysisId, '_', modelInd)
   
      PatientLevelPrediction::externalValidateDbPlp(
        plpModel = plpModel, 
        validationDatabaseDetails = databaseDetails, 
        validationRestrictPlpDataSettings = jobContext$settings$restrictPlpDataSettings, 
        settings = jobContext$settings$validationSettings, 
        #logSettings = , 
        outputFolder = workFolder
      )
  
    } else{
      ParallelLogger::logInfo(paste0('Issue loading model at ', modelLocation))
    }
  }

# move results into database
for(validationDatabaseDetail in databaseDetails){
  tryCatch({
    PatientLevelPrediction::insertResultsToSqlite(
      resultLocation = file.path(workFolder, validationDatabaseDetail$cdmDatabaseName), 
      cohortDefinitions = cohortDefinitionSet,
      databaseList = PatientLevelPrediction::createDatabaseList(
        cdmDatabaseSchemas = validationDatabaseDetail$cdmDatabaseSchema,
        cdmDatabaseNames = validationDatabaseDetail$cdmDatabaseName,
        databaseRefIds = validationDatabaseDetail$cdmDatabaseId 
      ),
      sqliteLocation = file.path(workFolder,'sqlite')
    )
  })
}
 

  # Export the results
  rlang::inform("Export data to csv files")

  sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = file.path(workFolder, "sqlite","databaseFile.sqlite")
  )
    
  PatientLevelPrediction::extractDatabaseToCsv(
    connectionDetails = sqliteConnectionDetails, 
    databaseSchemaSettings = PatientLevelPrediction::createDatabaseSchemaSettings(
      resultSchema = 'main', # sqlite settings
      tablePrefix = '', # sqlite settings
      targetDialect = 'sqlite', 
      tempEmulationSchema = NULL
    ), 
    csvFolder = file.path(workFolder, 'results'),
    fileAppend = NULL
  )
  
  # Zip the results
  rlang::inform("Zipping csv files")
  DatabaseConnector::createZipFile(
    zipFile = file.path(resultsFolder, 'results.zip'),
    files = file.path(workFolder, 'results')
  )
  
  
}