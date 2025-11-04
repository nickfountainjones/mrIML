library(mrIML)
library(tidymodels)
library(flashlight)
library(ggplot2)
library(patchwork)

library(neuralnet)

# Read in data file with minor allele freqs & env/space variables
#load("gfData.RData")
load("C:/Users/sriley0/OneDrive - University of Tasmania/RProjects/mrIML/vignettes/articles/gfData.RData")
# Get climate & MEM variables for predictors


data_split <- rsample::initial_split(gfData, prop = 0.7)
data_train <- rsample::training(data_split)
data_test <- rsample::testing(data_split)



X_train <- data_train %>%
  select(
    contains("bio_"),
    "elevation",
    contains("MEM.")
  ) %>%
  mutate_if(is.integer, as.numeric)
Y_train <- data_train %>%
  select(contains("GI5")) # GIGANTEA-5 (GI5)

Yatt<-attributes(Y_train)
Xatt<-attributes(X_train)
#dataParse <- data.frame(tibble::as_tibble(X),tibble::as_tibble(Y))
vars <- Yatt$names

Y_test <-data_test %>%
  select(contains("GI5")) # GIGANTEA-5 (GI5)

## test area

testFormula <- as.formula(paste(paste(Yatt$names, collapse = "+"), "~",paste(Xatt$names, collapse = "+")))


#



nnEg <- neuralnet::neuralnet(testFormula
                  , data = data_train
                  , hidden = 10
                  , err.fct = "sse"
                  , linear.output = FALSE
                  , lifesign = "full"
                  , rep = 2
                  , algorithm = "rprop+"
                  , stepmax = 10000
                  )

plot(nnEg, rep =2)

nnOutput <- compute(nnEg, rep = 1, data_test)

