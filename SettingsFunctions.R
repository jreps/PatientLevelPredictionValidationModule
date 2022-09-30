createPatientLevelPredictionValidationModuleSpecifications <- function(
  modelLocationList,
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
  recalibrate = "weakRecalibration",
  targetId = NULL, # NULL or a list
  outcomeId = NULL # NULL or a list
  # add a population setting for a different tar?
) {
  
  specifications <- list(
    module = "PatientLevelPredictionValidationModule",
    version = "0.0.1",
    remoteRepo = "github.com",
    remoteUsername = "jreps",
    settings = list(
      modelLocationList = modelLocationList,
      restrictPlpDataSettings = restrictPlpDataSettings,
      recalibrate = recalibrate,
      targetId = targetId,
      outcomeId = outcomeId
    )
  )
  class(specifications) <- c("PatientLevelPredictionValidationModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}