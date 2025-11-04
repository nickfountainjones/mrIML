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

# if (exists("O")){rm("O")}


  
O <- list(Methodology = mrMethod
                       , Models = mrModel
                       , Data = mrData
                       , PreModel = mrPreMod
                       , Fits = mrFits
                       , Bootstrap = mrBoot
                       , SummaryStatistics = mrSumStat)


#### User Input Data (this would be improved but is what the user would mainly interact with)
## all relevant user settings for the process

dataLocation <- ("Default")
#dataLocation <- ("C:/Users/sriley0/OneDrive - University of Tasmania/RProjects/mrIML/vignettes/articles/gfData.RData")


XHeadings <- c("scale.prop.zos")
YHeadings <- c("Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")
X1Headings <- c("Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")

modelNames <- c("Model_lm","Model_rf","Model_lm2")
desiredModels <- c("logistic_reg","rand_forest","linear_reg")
desiredEngines <- c("glm","randomForest","lm")

trainProp <- c(0.6,0.7,0.7)

tune_grid <- c(10,10,10)
k = c(10,10,10)
racing = c("TRUE","TRUE","TRUE")





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

O$Methodology$XHeadings <- XHeadings
O$Methodology$YHeadings <- YHeadings
O$Methodology$X1Headings <- X1Headings

O$Methodology$Models <- matrix(c(modelNames,desiredModels,desiredEngines),ncol=3)


# building from User Input


for (i in O$Methodology$Models){
  O$Fits[[i]] <- list()
  O$Boot[[i]] <- list()
  O$SumStat[[i]] <- list()
}

for (j in 1:length(O$Methodology$Models[,1])){
  O$Models[O$Methodology$Models[j,1]] <-
    eval(str2expression(paste0("parsnip::",O$Methodology$Models[j,2],"()"))) #%>%
      #set_engine(O$Methodology$Models[j,3])  
  
  
}





#### Starting the process (user edits should ideally not occur after this point)
## Initial Implementation 
# Data Sorting
O$Data$X <- select(data,O$mrMethod$XHeadings)
O$Data$Y <- select(data,O$mrMethod$YHeadings)
O$Data$X1 <- select(data,O$mrMethod$X1Headings)




## mrIMLPredicts part






