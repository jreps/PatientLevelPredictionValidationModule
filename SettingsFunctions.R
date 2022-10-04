createPatientLevelPredictionValidationModuleSpecifications <- function(
  modelLocationList, # a vector of plpModel locations
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
  validationSettings = PatientLevelPrediction::createValidationSettings(recalibrate = "weakRecalibration"),
  targetId, # a vector
  outcomeId # a vector
  # add a population setting for a different tar?
) {
  
  if(missing(targetId)){
    stop('targetId is needed')
  }
  if(missing(outcomeId)){
    stop('outcomeId is needed')
  }
  
  specifications <- list(
    module = "PatientLevelPredictionValidationModule",
    version = "0.0.5",
    remoteRepo = "github.com",
    remoteUsername = "jreps",
    settings = list(
      modelLocationList = modelLocationList,
      restrictPlpDataSettings = restrictPlpDataSettings,
      validationSettings = validationSettings,
      targetId = targetId,
      outcomeId = outcomeId
    )
  )
  class(specifications) <- c("PatientLevelPredictionValidationModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}