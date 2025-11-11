#### ---------------- Build an Empty mrIML Object for model stacking
#'
#' This is an early version of the object which will need to be updated to a S3
#' object soon. Currently still in testing phase.



new_mrIMLSObject <- function(){
  Ob <- list()
  Ob$Methodology <- list(Data = list()
                         , Models = list()
                         , Stacking = list()
                         , Processing = list()
                         , Visualisation = list())
  Ob$Data <- list()
  Ob$Fits <- list()
  Ob$SummaryStatistics <- list()
  Ob$Models <- list()
  Ob$System <- list()
  return(Ob)
}

mrAddModels <- function(Ob
                      ,modelNames = c("Model_lm")
                      ,desiredModels = c("logistic_reg")
                      ,desiredEngines = c("glm")
                      ,modelModes = c("classification")
                      ,stacking = c(TRUE)
                      ,trainProp = c(0.6)
                      ,balanceData = c(FALSE)
                      ,dummy = c(TRUE)
                      ,tune_grid_size = c(10)
                      ,k = c(5)
                      ,racing = c(FALSE)
                      ,modelParameters = c(
                        "")
                      ){
  
  for (i in 1:length(modelNames)){
   Ob$Methodology$Models[[modelNames[[i]]]] <- list(modelName = modelNames[i]
                                                    , modelFunction = desiredModels[i]
                                                    , modelEngine = desiredEngines[i]
                                                    , modelMode = modelModes[i]
                                                    , stacking = stacking[i]
                                                    , prop = trainProp[i]
                                                    , dummy = dummy[i]
                                                    , tune_grid_size = tune_grid_size[i]
                                                    , k = k[i]
                                                    , racing = racing[i]
                                                    , modelParameters = modelParameters[i])
    }
  return(Ob)
  
}

####---------------Add Only 1 model at a time given a list of settings -----#
#' This i
mrAddModel <- function(Ob, Settings){
  ## Some checking and defaults need to be added. Broken out so some can be 
  #' attributes at a later date.
  nameCheck <- attributes(Ob$Methodology$Models)$names
  modName <- Settings$modelName
  
  if(!length(grep(Settings$modelName,nameCheck)>0)){
    Ob$Methodology$Models[[modName]] <- list(modelName = Settings$modelName
                                           , modelFunction = Settings$modelFunction
                                           , modelEngine = Settings$modelEngine
                                           , modelMode = Settings$modelMode
                                           , stacking = Settings$stacking
                                           , prop = Settings$prop
                                           , dummy = Settings$dummy
                                           , tune_grid_size = Settings$tune_grid_size
                                           , k = Settings$k
                                           , racing = Settings$racing
                                           , modelParameters = Settings$modelParameters)
  }
  return(Ob)
  
}



#### ----------------- Basic function to update settings in a object. ----------
#'  
#'  Current version is manual. Future version should take anything in `...` and
#'  update the relevant field in the object that has been parsed.
#'


mrUpdateSettings <- function(Ob, ...){
  updateList <- ...names()
  for (i in updateList){
    if(i == "stacksOnly"){
      Ob$Methodology$Stacking$stacksOnly <- stacksOnly
    } 
    if(i == "vFolds"){
      Ob$Methodology$Stacking$vFolds <- vFolds
    }
    if(i == "stackProp"){
      Ob$Methodology$Stacking$stackProp <- stackProp
    }
    if(i == "bootstrapping"){
      Ob$Methodology$Processing$bootstrapping  <- bootstrapping
    } 
    if(i == "bootstrapNumber"){
      Ob$Methodology$Processing$bootstrapNumber  <- bootstrapNumber
    }

    if(i == "partialDependency"){
      Ob$Methodology$Visualisation$partialDependency <- partialDependency
    } 
    if(i == "partialDNumber"){
      Ob$Methodology$Visualisation$partialDNumber <- partialDNumber
    }
  }
  return(Ob)
}

mrBuildStack <- function(Ob){
  
  dStack_split <- initial_split(Ob$Data, prop = Ob$Methodology$Stacking$stackProp)
  dStack_train <- training(dStack_split)
  dStack_test  <- testing(dStack_split)
  folds <- rsample::vfold_cv(dStack_train,Ob$Methodology$Stacking$vFolds)
  ctrl_grid <- stacks::control_stack_grid()
  modStack <- stacks()
  #in the next line you have to make sure the 1 becomes multivariate...
  tempStack_recipe <- recipe(as.formula(paste0(Ob$Methodology$Data$YHeadings[[1]], "~ .")), data = dStack_train) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors())
  tempStack_wflow <- workflow() %>%
    add_recipe(tempStack_recipe)
  
  
    for (j in 1:length(Ob$Methodology$Models)){
      modelName <- Ob$Methodology$Models[[j]]$modelName
      Ob$System[[modelName]]<- list()
      
      eval(str2expression(paste0(
        "tempSpec"
        ," <- parsnip::",Ob$Methodology$Models[[modelName]]$modelFunction
        ,"(", Ob$Methodology$Models[[modelName]]$modelParameters
        ,")")))
      tempSpec <- set_mode(tempSpec,Ob$Methodology$Models[[modelName]]$modelMode)
      tempSpec <- set_engine(tempSpec,Ob$Methodology$Models[[modelName]]$modelEngine)
      
      Ob$System[[modelName]]$Spec <- tempSpec
      
      tempWorkflow <- tempStack_wflow
      tempWorkflow <- add_model(tempStack_wflow,tempSpec)
      
      
      Ob$System[[modelName]]$Workflow <- tempWorkflow

      tempRecipe <- tune_grid(object = tempWorkflow
                              , resamples = folds
                              , grid = 10
                              , control = ctrl_grid
      )
      
      Ob$System[[modelName]]$Recipe <- tune_grid(object = tempWorkflow
                                                  , resamples = folds
                                                  , grid = 10
                                                  , control = ctrl_grid
      )
      
      #tempObject$recipes[modelNames[[j]]] <- list(tempRecipe)
      #print(attributes(tempRecipe))
      modStack <- add_candidates(modStack,tempRecipe,name = modelName)
      #  add_model(paste0(tempObject$Models[modelNames[j]]))
      
    
    }
  Ob$Models$StackedModel <- blend_predictions(modStack,) %>%
    fit_members()
  return(Ob)
}

mrAddData <- function(Ob, data, XHeadings, YHeadings, X1Headings){
  Ob$Data <- data
  Ob$Methodology$Data$XHeadings = XHeadings
  Ob$Methodology$Data$YHeadings = YHeadings
  Ob$Methodology$Data$X1Headings = X1Headings
  return(Ob)
}

  
  
  
# 
# 
# mrIML_S_Object <- function(
#     dataLocation = ""
#     , XHeadings = ""
#     , YHeadings = ""
#     , X1Headings = ""
#     , modelNames = ""
#     , desiredModels = ""
#     , desiredEngines = ""
#     , modelModes = ""
#     , stacking = FALSE
#     , trainProp = c(0.7)
#     , balanceData = c(FALSE)
#     , dummy = c(FALSE)
#     , tune_grid_size = c(10)
#     , k = c(5)
#     , racing = c(FALSE)
#     , modelParameters = c("")
#     , Bootstrapping = c(FALSE)
#     , BootstrapNumber = c(10)
#     , stacksOnly = c(TRUE)
#     , stackMethod = "control_stack_grid"
#     , PartialDependency = PartialDependency
#     , PartialNumber = PartialNumber
# ){
#   
#   mrMethod <- list(XHeadings=XHeadings
#                    , YHeadings = YHeadings
#                    , X1Headings = X1Headings
#                    , Models = list()
#                    , BootStraping = list(Bootstrapping = FALSE, BootstrapNumber = 10)
#                    , Visualisation = list(PartialDependency = TRUE)
#   )
#   
#   
#   mrData <- list(X=tibble::tibble(),Y=tibble::tibble(),X1=tibble::tibble())
#   mrPreMod <- tibble::tibble();
#   mrModel <- list()
#   mrFits <- list()
#   mrBoot <- list()
#   mrSumStat <- list()
#   mrFL <- list()
#   mrWorkFlow <- list()
#   mrStack <-list()
#   # if (exists("O")){rm("O")}
#   
#   
#   ##%%%This should be a mrIMLObject function to create an S3 object  
#   O <- list(Methodology = mrMethod
#             , Models = mrModel
#             , Data = mrData
#             , PreModel = mrPreMod
#             , Fits = mrFits
#             , Bootstrap = mrBoot
#             , SummaryStatistics = mrSumStat
#             , Flashlight = mrFL
#             , Workflow = mrWorkFlow)
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   return(O)
# }