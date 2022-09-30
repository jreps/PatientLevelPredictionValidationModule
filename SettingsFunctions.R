createPatientLevelPredictionValidationModuleSpecifications <- function(
  modelLocationList,
  modelGithubSettings = NULL, # list of github repos with models
  modelPackageSettings = NULL, # list of models from the model package
  restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
  validationSettings = PatientLevelPrediction::createValidationSettings(recalibrate = "weakRecalibration"),
  targetId = NULL, # NULL or a list
  outcomeId = NULL # NULL or a list
  # add a population setting for a different tar?
) {
  
  specifications <- list(
    module = "PatientLevelPredictionValidationModule",
    version = "0.0.3",
    remoteRepo = "github.com",
    remoteUsername = "jreps",
    settings = list(
      modelLocationList = modelLocationList,
      modelGithubSettings = modelGithubSettings,
      modelPackageSettings = modelPackageSettings,
      restrictPlpDataSettings = restrictPlpDataSettings,
      validationSettings = validationSettings,
      targetId = targetId,
      outcomeId = outcomeId
    )
  )
  class(specifications) <- c("PatientLevelPredictionValidationModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}