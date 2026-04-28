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


### ----------------------------------Libraries --------------------------
## Basic Libraries
library(tibble)
library(parsnip)
library(dplyr)
library(tools)
library(tune)
library(flashlight)
library(tidyr)
library(rlang)
library(rsample)
library(recipes)
library(purrr)
library(workflows)
library(yardstick)
library(mrIML)
# stacks also need to be added (will eventually be part of mrIML) current files:
# mrIML_SObject
# mrIMLStackPerform
# mrIMLStackLight

baseDir <- getwd()

source(paste0(baseDir,"/R/mrIMLStackLight.R"), echo = FALSE)
source(paste0(baseDir,"/R/mrIML_SObject.R"), echo = FALSE)
source(paste0(baseDir,"/R/mrIMLStackPerform.R"), echo = FALSE)
source(paste0(baseDir,"/R/mrStackPlot.R"), echo = TRUE)
source(paste0(baseDir,"/R/mrStackVIP.R"), echo = TRUE)


## Graphical Libraries
library(ggplot2)
library(patchwork) #extension to ggplot2

## Model Specific libraries
library(nnet)
library(stacks)

### ----------------------------------Diagnostic Setup ---------------------

tic <- Sys.time()
set.seed(1)
### --------------------------------- User Input Section -------------------

#### User Input Data (this would be improved but is what the user would mainly interact with)
## all relevant user settings for the process

#' Data and basic manipulation. Form should be a data.frame an basic data 
#' cleansing (eg check factors, integers or booleans) 

# XData <- readRDS(paste0(baseDir,"/data/X1000_chaw.rds"))
XData <- readRDS(paste0(baseDir,"/data/X_mriml.rds"))
YData <- readRDS(paste0(baseDir,"/data/Y_mriml.rds"))
# YData <- readRDS(paste0(baseDir,"/data/Y1000_chaw.rds"))

data <- cbind(YData,XData)
data$rs5743618_A <- as.factor(data$rs5743618_A)
data$rs6819274_G <- as.factor(data$rs6819274_G)
data$rs703842_G <- as.factor(data$rs703842_G)

# data <- as_tibble(MRFcov::Bird.parasites)
# data$Hzosteropis <- as.factor(data$Hzosteropis)
# data$Hkillangoi <- as.factor(data$Hkillangoi)
# data$Plas <- as.factor(data$Plas)
# data$Microfilaria <- as.factor(data$Microfilaria)


# # The following indicate the column names from data. 
# XHeadings <- c("scale.prop.zos")
# YHeadings <- c("Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")
# X1Headings <- c("Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")

XHeadings <- c("latitude_S", "longitude_S", "UV_S", "dayT_S", "PC1", "PC2", "PC3", "PC4", "PC5"  )
YHeadings <- c("rs5743618_A", "rs6819274_G", "rs703842_G")
# YHeadings <- c("rs5743618_A", "rs6819274_G")
# YHeadings <- c("rs703842_G")
# YHeadings <- c("rs5743618_A", "rs703842_G")
X1Headings <- YHeadings


# Define models one at a time. Each will be added individually
Mod1 <- list()
Mod1$modelName <- "Logistic"
Mod1$modelFunction <- "logistic_reg"
Mod1$modelEngine <- "glm"
Mod1$modelMode <- "classification"
Mod1$stacking <- TRUE
Mod1$prop <- 0.7
Mod1$balanceData <- "no"
Mod1$dummy <- TRUE
Mod1$tune_grid_size <- 10
Mod1$k <- 5
Mod1$racing <- FALSE
Mod1$modelParameters <- ""
Mod1$modelMetric <- "roc_auc"

Mod2 <- list()
Mod2$modelName <- "RandomForest"
Mod2$modelFunction <- "rand_forest"
Mod2$modelEngine <- "randomForest"
Mod2$modelMode <- "classification"
Mod2$stacking <- TRUE
Mod2$prop <- 0.7
Mod2$balanceData <- "no"
Mod2$dummy <- TRUE
Mod2$tune_grid_size <- 10
Mod2$k <- 5
Mod2$racing <- TRUE
Mod2$modelParameters <- "mtry = tune(), min_n = tune(),trees = 1000"
Mod2$modelMetric <- "roc_auc"

Mod3 <- list()
Mod3$modelName <- "MLP"
Mod3$modelFunction <- "mlp"
Mod3$modelEngine <- "nnet"
Mod3$modelMode <- "classification"
Mod3$stacking <- TRUE
Mod3$prop <- 0.7
Mod3$balanceData <- "no"
Mod3$dummy <- TRUE
Mod3$tune_grid_size <- 10
Mod3$k <- 5
Mod3$racing <- TRUE
Mod3$modelParameters <- "hidden_units = tune(), penalty = tune(), epochs = tune()"
Mod3$modelMetric <- "roc_auc"

StackSet <- list()
StackSet$stacksOnly <- FALSE
StackSet$stackMethod <- "control_stack_grid" #control_stack_resamples or control_stack_bayes
StackSet$vFolds <- 5
StackSet$stackProp <- 0.8
StackSet$stackMode <- "classification"
StackSet$modelMetric <- "roc_auc"
StackSet$penalty <- 0.01
StackSet$positive_only <- FALSE
# Select processing options

ProcessSet <- list()
ProcessSet$bootstrapping = TRUE
ProcessSet$bootstrapNumber = 50
ProcessSet$PDSingle = TRUE
ProcessSet$PDMulti = TRUE




# Select visualisation options

VisSet <- list()
VisSet$partialDependency <- TRUE
VisSet$partialDNumber <- 5 #Give a maximum of top 5 per model.
VisSet$CovPlots <- TRUE
VisSet$DerivPlots <- TRUE
VisSet$Importance <- TRUE

#------------------ Typical end of initial Setup.------------------------------



####------------------------Simple Code Piping --------------------------------

S <- new_mrIMLSObject() %>%
  # mrAddModel(Settings = Mod1) %>%
  mrAddModel(Settings = Mod1) %>%
  mrAddModel(Settings = Mod2) %>%
  mrAddModel(Settings = Mod3) %>%
  mrUpdateSettings(Stack = StackSet) %>%
  mrAddData(data=data, XHeadings = XHeadings, YHeadings = YHeadings, X1Headings = X1Headings) %>% 
  mrUpdateSettings(Visual = VisSet) %>%
  mrUpdateSettings(Process = ProcessSet) %>% 
  mrBuildModels() # %>%
  # mrIMLStackPerform_classification() %>%
  # mrIML_StackLight() %>%
  # mrStackVIP()
# S <- mrIML_StackLight(S)
#  mrPredictStacks()


## Diagnostics
S <- mrIMLStackPerform_classification(S)
S <- mrIML_StackLight(S)
S <- mrStackVIP(S)
# S <- mrPredictStacks(S)


options <- list()
options$PDPPlot <- FALSE
options$DerivativePlots <- FALSE
options$CovPlots <- TRUE
options$ImportancePlots <- FALSE
# 
S <- mrIMLStack_plots(S, options)


# S1 <- mrIMLStack_plots(S1, options)

# S <- mrBuildModels(S)

#### ------------------------- End Diagnostics ---------------------------------

toc <- Sys.time() - tic
