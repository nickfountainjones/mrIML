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
  if(is.null(Settings$modelMetric)){
    if(Settings$modelMode == "classification"){
      modelMetric <- "roc_auc"
    } else {
      modelMetric <- "rmse"
    }
    
  } else {
    modelMetric <- Settings$modelMetric
  }
  
  if(!length(grep(Settings$modelName,nameCheck)>0)){
    Ob$Methodology$Models[[modName]] <- list(modelName = Settings$modelName
                                           , modelFunction = Settings$modelFunction
                                           , modelEngine = Settings$modelEngine
                                           , modelMode = Settings$modelMode
                                           , stacking = Settings$stacking
                                           , balanceData = Settings$balanceData
                                           , prop = Settings$prop
                                           , dummy = Settings$dummy
                                           , tune_grid_size = Settings$tune_grid_size
                                           , k = Settings$k
                                           , racing = Settings$racing
                                           , modelParameters = Settings$modelParameters
                                           , modelMetric = modelMetric)
  }
  return(Ob)
  
}



#### ----------------- Basic function to update settings in a object. ----------
#'  
#'  Current version is manual. Future version should take anything in `...` and
#'  update the relevant field in the object that has been parsed if the data 
#'  type matches what is in the named field.
#'


mrUpdateSettingsOne <- function(Ob, ...){
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




#### ------------------------------Update Stack Settings -----------------------
mrUpdateSettings <- function(Ob 
                             , Stack = list()
                             , Process = list()
                             , Visual = list()
                             , ...){
### these checks ideally should be a 'if not null, match type and existence then apply.'  
  if (!is_empty(Stack)){
    stackList <- attributes(Stack)$names
    for (j in stackList){
      Ob$Methodology$Stacking[[j]] <- Stack[[j]]
    }
  }
  
  if (!is_empty(Process)){
    processList <- attributes(Process)$names
    for (j in processList){
      Ob$Methodology$Processing[[j]] <- Process[[j]]
    }
  }  
  
  if (!is_empty(Visual)){
    visualList <- attributes(Visual)$names
    for (j in visualList){
      Ob$Methodology$Visualisation[[j]] <- Visual[[j]]
    }
  }  
  
#' For later versions this should include individual points. 
#' See previous example.
  return(Ob)
}


####--------------------------Build base models--------------------------------

mrBuildModels <- function(Ob){
  

  dStack_split<- rsample::initial_split(Ob$Data
                                        , prop = Ob$Methodology$Stacking$stackProp)  
  Ob$Fits$Data$Config <- list()
  Ob$Fits$Data$Config$Split <- dStack_split
  dStack_train <- rsample::training(dStack_split)
  dStack_test  <- rsample::testing(dStack_split)
  folds <- rsample::vfold_cv(dStack_train,Ob$Methodology$Stacking$vFolds)
  modelStack <- list()
  ## Empty Stack
  
  
  
  
  ## Handle factors (note code needs changing to stop splitting and rejoining)
  
  # if (any(purrr::map_lgl(X, is.factor))) {
  #   dummy = TRUE
  #   message(
  #     "Setting dummy = TRUE because X contains factors."
  #   )
  # }
  
  ## Checks for equal rows
  
  ## Check X1 Headings?
  
  ## Check if there is balancing of data and if Thermis is needed.(moved from internal)
  
  for (i in Ob$Methodology$Data$YHeadings){
    modelStack[[i]] <- stacks::stacks()
    Ob$System[[i]] <- list()
    Ob$System[[i]]$ModelStack <- list()
    Ob$Models[[i]] <- list()
    Ob$Fits[[i]] <- list()
    
    for (j in 1:length(Ob$Methodology$Models)){
      modelName <- Ob$Methodology$Models[[j]]$modelName 
      Ob$System[[i]][[modelName]]<- list()  # Build Branch for Model
      
      ### Check Validity of settings
      
      
      ### Create Model
      
      eval(str2expression(paste0(
        "tempSpec"
        ," <- parsnip::",Ob$Methodology$Models[[modelName]]$modelFunction
        ,"(", Ob$Methodology$Models[[modelName]]$modelParameters
        ,")")))
      
      tempSpec <- parsnip::set_mode(tempSpec,Ob$Methodology$Models[[modelName]]$modelMode)
      tempSpec <- parsnip::set_engine(tempSpec,Ob$Methodology$Models[[modelName]]$modelEngine)
      
      
      Ob$System[[i]][[modelName]]$specification <- tempSpec
      
      ### Fit and organise Models
      
      Ob <- mrBuildModels_internal(Ob = Ob
                                   , yModel = i
                                   , modelName = modelName
                                   , data_train = dStack_train
                                   , data_test = dStack_test
                                   , data_cv = folds)


      Ob$System[[i]][[modelName]]$last_model_fit <- Ob$Models[[i]][[modelName]] %>%
        tune::last_fit(dStack_split)


      if(Ob$Methodology$Models[[modelName]]$stacking){
        tempRecipe <- Ob$System[[i]][[modelName]]$tune_s
        modelStack[[i]] <- stacks::add_candidates(modelStack[[i]]
                                                 , tempRecipe
                                                 , name = modelName)
      }
      
      # ## This is a processor intensive step. This is likely an area for parallel?
      # mod1_k <- Ob$System[[i]][[modelName]]$final_model %>%
      #   workflows::fit(data = dStack_train)
      # last_mod_fit <- tune::last_fit(Ob$System[[i]][[modelName]]$final_model
      #                                , dStack_split)
      # Ob$Models[[i]][[modelName]]$last_mod_fit <- last_mod_fit
      # 
      # Ob$Models[[i]][[modelName]]$final_model <- tune::finalize_workflow(
      #   mod_workflow,
      #   best_m) 

            
    }
  ## finishing off the stack
  Ob$Models[[i]]$ModelStack <- stacks::blend_predictions(modelStack[[i]],) %>%
    fit_members()
  Ob$Fits[[i]]$ModelStack <- modelStack[[i]] 
  
  
  
  
    
  }
  Ob <- mrPredictStack_internal(Ob)
  ## What has now been returned is all the finalised models.

  return(Ob)
}


####---------------------- Run the models to be stacked -----------------------




mrBuildModels_internal <- function(Ob
                                   , yModel
                                   , modelName
                                   , data_train
                                   , data_test
                                   , data_cv
                                   ){
  #' Move the checks outside
  #' Move the balance correction to remain here
  #' Cross fold validation moved outside (currently)
  #' Moved dummy step (refactoring) to remain here
  #' build recipe, workflow and tuned workflow
  
  
  ## Checks including if the modelName is blank.
  
  
  
  ## Breaking down object for readability and consistency
  data_train <- rsample::training(Ob$Fits$Data$Config$Split)
  data_test <- rsample::training(Ob$Fits$Data$Config$Split)
  
  
  balance_data <- Ob$Methodology$Models[[modelName]]$balanceData
  racing <- Ob$Methodology$Models[[modelName]]$racing
  mode <- Ob$Methodology$Models[[modelName]]$modelMode
  tune_grid_size <- Ob$Methodology$Models[[modelName]]$tune_grid_size
  ## Build recipe
  data_recipe <- recipes::recipe(as.formula(paste0(yModel,"~."))
                                , data = data_train)
                
  # Update with balancing if required
  
  
  # include dummy if required
  
  ## Build workflows
  mod_workflow <- workflows::workflow() %>%
    workflows::add_recipe(data_recipe) %>%
    workflows::add_model(Ob$System[[yModel]][[modelName]]$specification )
  
    
    
  ## Tune Model  
  if (racing) {
    tune_m <- finetune::tune_race_anova(
      mod_workflow,
      resamples = data_cv
    )
  } else {
    tune_m <- tune::tune_grid(
      mod_workflow,
      resamples = data_cv,
      grid = tune_grid_size
    )
  }
  
  ## Stacks tune setup 
  #' Note that this needs to have if statements for non grid implementations
  
  tune_s <- tune::tune_grid(
    mod_workflow,
    resamples = data_cv,
    grid = tune_grid_size,
    control = stacks::control_stack_grid()
    
  )
  
  
  if (mode == "classification") {
    best_m <- tune_m %>%
      tune::select_best(metric = "roc_auc")
  } else {
    best_m <- tune_m %>%
      tune::select_best(metric = "rmse")
  }
    
  final_model <- tune::finalize_workflow(
    mod_workflow,
    best_m
  )


  
  Ob$Models[[yModel]][[modelName]]$final_model <- tune::finalize_workflow(
    mod_workflow,
    best_m) 
  mod1_k <- final_model %>%
    workflows::fit(data = data_train)
  last_mod_fit <- tune::last_fit(final_model
                                 , Ob$Fits$Data$Config$Split)  
  

  
  
  
  
  
  last_model_fit <- tune::last_fit(final_model, data = Ob$Data, split = Ob$Fits$Data$Config$Split)

  # Last fit has been removed from here to avoid the requirement for test data

  ## Repack variables:
  
  Ob$Models[[yModel]][[modelName]] <- last_mod_fit  
  
  Ob$System[[yModel]][[modelName]]$recipe <- data_recipe
#  Ob$System[[yModel]][[modelName]]$workflow <- mod_workflow #unnecessary?
  Ob$System[[yModel]][[modelName]]$tune_s <- tune_s
  Ob$Models[[yModel]][[modelName]] <- final_model #moved to perform
  Ob$System[[yModel]][[modelName]]$final_fit <- last_model_fit
  return(Ob)
  
}

mrPredictStack_internal <- function(Ob){
  
  data_test <- rsample::testing(Ob$Fits$Data$Config$Split)
  
  response <- Ob$Methodology$Data$YHeadings
  Ob$System[[i]]$ModelStack$final_fit <- list()
  
  tempArray <- 1:length(Ob$Data[[1]])
  tempRow <- tempArray[-Ob$Fits$Data$Config$Split$in_id]
  
  for (i in response){
    Ob$System[[i]]$ModelStack <- list()
    syhatProb <- stats::predict(Ob$Models[[i]]$ModelStack, new_data = data_test, type = "prob")
    syhatClass <- stats::predict(Ob$Models[[i]]$ModelStack, new_data = data_test, type = "class")
    Ob$System[[i]]$ModelStack$final_fit$.predictions[[1]] <- data.frame(Outcome = Ob$Data[[i]][tempRow]
                                                                        , .pred_class = syhatClass$.pred_class
                                                                        , .pred_0 = syhatProb$.pred_0
                                                                        , .pred_1 = syhatProb$.pred_1
                                                                        , .row = tempRow)
    colnames(Ob$System[[i]]$ModelStack$final_fit$.predictions[[1]])[1] <- i
    
  }
  return(Ob)
}


# 
# mrPredictStack_internal <- function(Ob
#                                     , yModel
#                                     , modelName
#                                     , data_split
#                                     , data_train
#                                     , data_test){
#   Ob$Fits[[yModel]][[modelName]] <- list()                    
#   
# #  mod1_k <- Ob$System[[yModel]][[modelName]]$final_model %>%
# #    workflows::fit(data = data_train)
#   
#   
#   #last_model_fit <- tune::last_fit(Ob$System[[yModel]][[modelName]]$final_model, data = Obs$Data, split = data_split)
#   
#   if (Ob$Methodology$Stacking$stackMode == "classification") {
#     yhatO <- stats::predict(mod1_k
#                             , new_data = Ob$Data
#                             , type = "prob")
#     yhat <- yhatO$.pred_1
#     yhatT <- stats::predict(mod1_k
#                             , new_data = data_test
#                             , type = "class") %>%
#       dplyr::bind_cols(
#         data_test %>%
#           dplyr::select(all_of(yModel))
#       )
#     
#     truth <- as.numeric(as.character(Ob$Data[[yModel]]))
#     
#     deviance <- sapply(
#       seq_along(truth),
#       function(j) {
#         resid <- ifelse(
#           truth[j] == 1,
#           sqrt(-2 * log(yhat[j])),
#           -1 * sqrt(-2 * log(1 - yhat[j]))
#         )
#         return(resid)
#       }
#     )
#     
#     deviance_morans <- deviance
#     deviance_morans[is.infinite(deviance_morans)] <- 2
#   
#  #   last_mod_fit <- tune::last_fit(Ob$System[[yModel]][[modelName]]$final_model
# #                                   , data_split)
#   
#   
#  #   Ob$Models[[yModel]][[modelName]] <- last_mod_fit
#     Ob$Fits[[yModel]][[modelName]]$yhat <- yhat
#     Ob$Fits[[yModel]][[modelName]]$yhatT <- yhatT
#     Ob$Fits[[yModel]][[modelName]]$deviance <- deviance_morans
#   
#   }
#   return(Ob)
# }
# 
# 
# mrAddData <- function(Ob, data, XHeadings, YHeadings, X1Headings){
#   Ob$Data <- data
#   Ob$Methodology$Data$XHeadings = XHeadings
#   Ob$Methodology$Data$YHeadings = YHeadings
#   Ob$Methodology$Data$X1Headings = X1Headings
#   return(Ob)
# }
# 
# 



####---------------------- Take in models to build a stack --------------------
# 
# mrBuildStack <- function(Ob){
#   
#   ctrl_grid <- stacks::control_stack_grid()
#   modStack <- list()
#   tempStack_recipe <- list()
#   tempStack_wflow <- list()
#   tempWorkflow <- list()
#   tempSpec <- list()
#   dStack_split <- list()
#   dStack_train <- list()
#   dStack_test <- list()
#   best_m <- list()
#   mod1_k <- list()
#     
#   if (Ob$Methodology$Stacking$stackMode == "classification"){
#     metSet <- yardstick::metric_set(roc_auc)
#   } else {
#     metSet <- yardstick::metric_set(rmse)
#   }
#   
#   dStack_split<- rsample::initial_split(Ob$Data, prop = Ob$Methodology$Stacking$stackProp)
#   dStack_train <- rsample::training(dStack_split)
#   dStack_test  <- rsample::testing(dStack_split)
#   folds <- rsample::vfold_cv(dStack_train,Ob$Methodology$Stacking$vFolds)
#   
#   
#   
#   ## The following should be made into a separate function for parallel processing.
#   
#   
#   for (i in Ob$Methodology$Data$YHeadings){
#     
#     # print(attributes(Ob$Data[i]))
#     # print(attributes(i))
#     
#     Ob$System[[i]] <- list()
#     Ob$Models[[i]] <- list()
#     modStack[[i]] <- stacks::stacks()
#     
#     
#     
#     tempStack_recipe[[i]] <- recipe(as.formula(paste0(i, "~ .")), data = dStack_train) %>%
#       step_dummy(all_nominal_predictors()) %>% 
#       step_zv(all_predictors())
#     tempStack_wflow[[i]] <- workflow() %>%
#       add_recipe(tempStack_recipe[[i]])
#     
#       for (j in 1:length(Ob$Methodology$Models)){
#         modelName <- Ob$Methodology$Models[[j]]$modelName
#         Ob$System[[i]][[modelName]]<- list()
#         
#         eval(str2expression(paste0(
#           "tempSpec[[i]]"
#           ," <- parsnip::",Ob$Methodology$Models[[modelName]]$modelFunction
#           ,"(", Ob$Methodology$Models[[modelName]]$modelParameters
#           ,")")))
# 
#         tempSpec[[i]] <- parsnip::set_mode(tempSpec[[i]],Ob$Methodology$Models[[modelName]]$modelMode)
#         tempSpec[[i]] <- parsnip::set_engine(tempSpec[[i]],Ob$Methodology$Models[[modelName]]$modelEngine)
#         
#         Ob$System[[i]][[modelName]]$Spec <- tempSpec[[i]]
# 
#         tempWorkflow[[i]] <- tempStack_wflow[[i]] %>%
#           workflows::add_model(tempSpec[[i]])
#       
#         Ob$System[[i]][[modelName]]$Workflow <- tempWorkflow[[i]]
#         Ob$System[[i]][[modelName]]$Recipe <- tune::tune_grid(object = tempWorkflow[[i]]
#                                                     , resamples = folds
#                                                     , grid = Ob$Methodology$Models[[modelName]]$tune_grid_size
#                                                     , control = ctrl_grid                                
#         )
#         if (Ob$Methodology$Models[[j]]$modelMode == "classification") {
#           best_m[[i]] <- Ob$System[[i]][[modelName]]$Recipe %>%
#             tune::select_best(metric = "roc_auc")
#         } else {
#           best_m[[i]] <- Ob$System[[i]][[modelName]]$Recipe %>%
#             tune::select_best(metric = "rmse")
#         }       
#         
#         Ob$Models[[i]][[modelName]] <- tune::finalize_workflow(
#           tempWorkflow[[i]]
#           , best_m[[i]]
#         )
#         
#         mod1_k[[i]] <- Ob$Models[[i]][[modelName]] %>%
#           workflows::fit(data = dStack_train)
#         
#         modStack[[i]] <- add_candidates(modStack[[i]],Ob$System[[i]][[modelName]]$Recipe,name = modelName)
#       }
# 
#     Ob$Models[[i]]$StackedModel <- stacks::blend_predictions(modStack[[i]],metric = metSet) %>%
#       stacks::fit_members()
# 
#   }
#   
# #  Ob <- mrIML_internal_fit_stack(Ob)
#   
#   return(Ob)
# }
# 

