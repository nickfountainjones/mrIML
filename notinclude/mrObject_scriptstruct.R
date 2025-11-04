#' Generates a multi-response predictive model
#'
#' @description
#' This script creates a mrIML object for testing purposes only. It will be in 
#' the form that the mrIMLObject Function will be written.
#'
#' @param Y,X,X1 Data frames containing the response, predictor, and the joint
#' response variables (i.e. the responses that are also to be used as predictors
#' if fitting GN model) respectively. If `X1` is not provided then a standard
#' multi-response model will be fit to the data (e.g. the response models are
#' independant of one another conditional on the predictors supplied in X). See
#' **Details** section below.
#' @param Model Any model from the \pkg{tidymodels} package. See **Examples**.
#' @param balance_data A character string:
#' * "up": up-samples the data to equal class sizes.
#' * "down": down-samples the data to equal class sizes.
#' * "no": leaves the data as is. "no" is the default value.
#' @param dummy A logical value indicating if [recipes::step_dummy()] should be
#' included in the data recipe.
#' @param tune_grid_size A numeric value that sets the grid size for
#' hyperparameter tuning. Larger grid sizes increase computational time. Ignored
#' if `racing = TRUE`.
#' @param racing A logical value. If `TRUE`, `mrIML` performs the grid search
#' using the [finetune::tune_race_anova()] method; otherwise, [tune::tune_grid()]
#' is used. `racing = TRUE` is now the default method of tuning.
#' @param k A numeric value. Sets the number of folds in the cross-validation.
#' 10-fold CV is the default.
#' @param prop A numeric value between 0 and 1. Defines the training-testing
#' data proportion to be used, which defaults to `prop = 0.7`.
#'
#' @details
#' `mrIMLObject` will build an object to be parsed between functions with an all
#' required information required to follow the process and all outcomes. 
#' 
#'
#'
#' @returns (not really a return at this point) A list object with seven slots:
#' * `$Methodology`: A list of processes to occur
#' * `$PreModel` : The data prior to filtering etc
#' * `$Data`: A list of the raw data.
#' * `$Model`: A list of \pkg{tidymodels} objects that was fit.
#' * `$Fits`: A list of the fitted models for each response variable for each 
#' model.
#' * `$Bootstrap`: A list of the predictions
#' * `$SummaryStatistics`: A list of all summary statistics for the models.
#'


### Building the empty structure (likely function going forwards)
library(tibble)
library(parsnip)
library(dplyr)
library(tools)
#library(tidyr)

mrMethod <- list(XHeadings=c("Replace","with","Predictor","Variables")
                , YHeadings = c("Replace","with","Response","Variables")
                , X1Headings = c("Replace","with","Joint","Response","Variables")
                , Models = c("lm","rf")
                , PPFlags = c("Currently Empty")
                , EnStruct = c("Insert","Ensemble","Structure")
                , BootStraping = list(do = FALSE, num = 10)
                , Visualisation = list(PPlot = TRUE
                                       , BSPlot = FALSE
                                       , IntPlot = TRUE))



mrData <- list(X=tibble::tibble(),Y=tibble::tibble(),X1=tibble::tibble())
mrPreMod <- tibble::tibble();
mrModel <- list()
mrFits <- list()
mrBoot <- list()
mrSumStat <- list()
mrFL <- list()
# if (exists("O")){rm("O")}


  
O <- list(Methodology = mrMethod
           , Models = mrModel
           , Data = mrData
           , PreModel = mrPreMod
           , Fits = mrFits
           , Bootstrap = mrBoot
           , SummaryStatistics = mrSumStat
           , Flashlight = mrFL)


#### User Input Data (this would be improved but is what the user would mainly interact with)
## all relevant user settings for the process

dataLocation <- ("Default")
#dataLocation <- ("C:/Users/sriley0/OneDrive - University of Tasmania/RProjects/mrIML/vignettes/articles/gfData.RData")


XHeadings <- c("scale.prop.zos")
YHeadings <- c("Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")
X1Headings <- c("Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")

modelNames <- c("Model_lm","Model_rf","Model_lm2")
desiredModels <- c("logistic_reg","rand_forest","logistic_reg")
desiredEngines <- c("glm","randomForest","glm")
modelModes <- c("classification", "classification", "classification")
trainProp <- c(0.6,0.7,0.7)
balanceData <- c(FALSE,FALSE,FALSE)
dummy <- c(TRUE,TRUE,TRUE)
tune_grid <- c(10,10,10)
k <- c(5,5,5)
racing <- c(FALSE,FALSE,FALSE)


# %% This needs a lot of neatening
model1Parameters <- tibble(blank=TRUE)
model2Parameters <- tibble(trees=100, blank = FALSE)
model3Parameters <- tibble(blank=TRUE)

## Import Data this needs to be visible but shouldn't require modification

O$Methodology$DataLocation <- dataLocation
if (O$Methodology$DataLocation != "Default"){
  if(tools::file_ext(O$Methodology$DataLocation) == "RData"){
    data <- get(load(O$Methodology$DataLocation))
  } else {
    data <- load(O$Methodology$DataLocation)
  }
  
  #data <- load(O$Methodology$DataLocation[])
} else {
  data <- MRFcov::Bird.parasites 
}


#Translating User Input
modelSpecific <-list()
modelSpecific[modelNames[1]] <- list(model1Parameters)
modelSpecific[modelNames[2]] <- list(model2Parameters)
modelSpecific[modelNames[3]] <- list(model3Parameters)


O$Methodology$XHeadings <- XHeadings
O$Methodology$YHeadings <- YHeadings
O$Methodology$X1Headings <- X1Headings

O$Data$X <- select(data,O$Methodology$XHeadings)
O$Data$Y <- select(data,O$Methodology$YHeadings)
O$Data$X1 <- select(data,O$Methodology$X1Headings)


O$Methodology$Models <- tibble(modelNames=modelNames
                               ,ModelFunction=desiredModels
                               ,ModelEngines=desiredEngines
                               ,prop=trainProp
                               ,tune_grid_size=tune_grid
                               ,racing=racing
                               ,mode = modelModes
                               ,dummy = dummy
                               ,modelParams=modelSpecific
                               ,k = k)


# building from User Input


for (i in O$Methodology$Models$modelNames){
  O$Fits[[i]] <- list()
#  O$Boot[[i]] <- list()
  O$SummaryStatistics[[i]] <- list()
}

#%%% The code below is horrible but will work for the demo.

for (j in 1:length(O$Methodology$Models$modelNames)){
  eval(str2expression(paste0(
    "O$Models$",modelNames[[j]]
    ,"<-parsnip::",O$Methodology$Models$ModelFunction[[j]]
    ,"(engine = '",O$Methodology$Models$ModelEngines[j]
    ,"', mode = '",O$Methodology$Models$mode[j] 
    ,"')")))
}





#### Starting the process (user edits should ideally not occur after this point)
## Initial Implementation 
# Data Sorting
O$Data$X <- select(data,O$Methodology$XHeadings)
O$Data$Y <- select(data,O$Methodology$YHeadings)
O$Data$X1 <- select(data,O$Methodology$X1Headings)




####----------------------------- mrIMLPredicts part
## There is likely an opportunity to change data or just pass the object around

# for (i in 1:length(O$Models)){
#   predictData <- mrIMLpredicts(
#     X=O$Data$X
#     , Y=O$Data$Y
#     , X1=O$Data$X1
#     , Model=O$Models[[i]]
#     , prop = O$Methodology$Models$prop[i]
#     , k = O$Methodology$Models$k[i]
#     , racing = O$Methodology$Models$racing[i]
#   )
#   
#   O$Models[[O$Methodology$Models$modelNames[[i]]]] <- predictData$Model
#   O$Fits[[O$Methodology$Models$modelNames[[i]]]] <- predictData$Fits
#   
# }



for (i in 1:length(O$Models)){
  predictData <- mrIMLpredicts(
    X=O$Data$X
    , Y=O$Data$Y
    , X1=O$Data$X1
    , Model=O$Models[[i]]
    , prop = O$Methodology$Models$prop[i]
    , k = O$Methodology$Models$k[i]
    , racing = O$Methodology$Models$racing[i]
    )

  O$Models[[O$Methodology$Models$modelNames[[i]]]] <- predictData$Model
  O$Fits[[O$Methodology$Models$modelNames[[i]]]] <- predictData$Fits
  
  ##%%% This should probably be in another section but that would be a lot of double handling
  ##%%% future version should probably just have the same object passed around.
  tempPref <- mrIMLperformance(predictData)
  O$SummaryStatistics[[O$Methodology$Models$modelNames[[i]]]]$model_performance <- tempPref$model_performance
  O$SummaryStatistics$global_performance_summary[[O$Methodology$Models$modelNames[[i]]]] <- tempPref$global_performance_summary

  tempFL <- mrFlashlight(predictData)
  
  O$Flashlight[[O$Methodology$Models$modelNames[[i]]]] <- tempFL
  
}

####----------------------mrIMLPerformance Section
## 




