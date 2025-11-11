#' Generates a multi-response predictive model
#'
#' @description
#' This is a template to use a mrIML object. By filling out the relevant fields 
#' below an object will be created which can then be used within the mrIML 
#' package.
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
#' @param So many more!
#'
#' @details
#' `mrIMLObject` will build an object to be parsed between functions with an all
#' required information required to follow the process and all outcomes. 
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
library(stacks)
library(tune)
library(nnet)
library(flashlight)
library(ggplot2)
#library(tidyr)


#### User Input Data (this would be improved but is what the user would mainly interact with)
## all relevant user settings for the process

#' Data and basic manipulation. Form should be a data.frame an basic data 
#' cleansing (eg check factors, integers or booleans) 

data <- as_tibble(MRFcov::Bird.parasites)
data$Hzosteropis <- as.factor(data$Hzosteropis)
data$Hkillangoi <- as.factor(data$Hkillangoi)
data$Plas <- as.factor(data$Plas)
data$Microfilaria <- as.factor(data$Microfilaria)


# The following indicate the column names from data. 
XHeadings <- c("scale.prop.zos")
YHeadings <- c("Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")
X1Headings <- c("Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")

# Select model details. Ensure that all setting are done at a length consistent for the model.

modelNames <- c("Model_lm","Model_rf","MLP")
desiredModels <- c("logistic_reg","rand_forest","mlp")
desiredEngines <- c("glm","randomForest","nnet")
modelModes <- c("classification", "classification", "classification")
stacking <- c(TRUE,TRUE,TRUE)
trainProp <- c(0.6,0.7,0.7)
balanceData <- c(FALSE,FALSE,FALSE)
dummy <- c(TRUE,TRUE,TRUE)
tune_grid_size <- c(10,10,10)
k <- c(5,5,5)
racing <- c(FALSE,FALSE,FALSE)
modelParameters <- c(
  ""
  ,"mtry = tune(), min_n = tune(),trees = 500"
  ,"hidden_units = tune(), penalty = tune(), epochs = tune()"
)

Mod1 <- list()
Mod1$modelName <- "Logistic"
Mod1$modelFunction <- "logistic_reg"
Mod1$modelEngine <- "glm"
Mod1$modelMode <- "classification"
Mod1$stacking <- TRUE
Mod1$prop <- 0.7
Mod1$balanceData <- FALSE
Mod1$dummy <- TRUE
Mod1$tune_grid_size <- 10
Mod1$k <- 5
Mod1$racing <- FALSE
Mod1$modelParameters <- ""

Mod2 <- list()
Mod2$modelName <- "RandomForest"
Mod2$modelFunction <- "rand_forest"
Mod2$modelEngine <- "randomForest"
Mod2$modelMode <- "classification"
Mod2$stacking <- TRUE
Mod2$prop <- 0.7
Mod2$balanceData <- FALSE
Mod2$dummy <- TRUE
Mod2$tune_grid_size <- 10
Mod2$k <- 5
Mod2$racing <- FALSE
Mod2$modelParameters <- "mtry = tune(), min_n = tune(),trees = 500"

Mod3 <- list()
Mod3$modelName <- "MLP"
Mod3$modelFunction <- "mlp"
Mod3$modelEngine <- "nnet"
Mod3$modelMode <- "classification"
Mod3$stacking <- TRUE
Mod3$prop <- 0.7
Mod3$balanceData <- FALSE
Mod3$dummy <- TRUE
Mod3$tune_grid_size <- 10
Mod3$k <- 5
Mod3$racing <- FALSE
Mod3$modelParameters <- "hidden_units = tune(), penalty = tune(), epochs = tune()"

stacksOnly <- FALSE
stackMethod <- "control_stack_grid" #control_stack_resamples or control_stack_bayes
vFolds <- 5
stackProp <- 0.7

# Select processing options

bootstrapping = TRUE
bootstrapNumber = 10

# Select visualisation options

partialDependency = TRUE
partialDNumber = 5 #Give a maximum of top 5 per model.


#### Typical end of initial Setup.

S <- new_mrIMLSObject() %>%
  mrAddModel(Settings = Mod1) %>%
  mrAddModel(Settings = Mod1) %>%
  mrAddModel(Settings = Mod2) %>%
  mrAddModel(Settings = Mod3) %>%
  mrUpdateSettings(stacksOnly = stacksOnly
                   , stacksMethod = stacksMethod
                   , bootstrapping = bootstrapping
                   , bootstrapNumber = bootstrapNumber
                   , partialDependency = partialDependency
                   , vFolds = vFolds
                   , stackProp = stackProp
    
  ) %>%
  mrAddData(data=data, XHeadings = XHeadings, YHeadings = YHeadings, X1Headings = X1Headings) %>%  
  mrBuildStack() #%>%
  
  

sTyhat0 <- stats::predict(S$Models$StackedModel, new_data = dStack_test, type = "prob")
sTyhat <- sTyhat0$.pred_1





  # mrAddModels(modelNames = modelNames
  #             , desiredModels = desiredModels
  #             , desiredEngines = desiredEngines
  #             , modelModes = modelModes
  #             , stacking = stacking
  #             , trainProp = trainProp
  #             , balanceData = balanceData
  #             , dummy = dummy
  #             , tune_grid_size = tune_grid_size
  #             , k = k
  #             , racing = racing
  #             , modelParameters = modelParameters
  #             ) %>%











