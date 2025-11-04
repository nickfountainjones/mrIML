library(mrIML)
library(tidymodels)
library(flashlight)
library(ggplot2)
library(patchwork)

library(brulee)

set.seed(7007)

# Read in data file with minor allele freqs & env/space variables
#load("gfData.RData")
load("C:/Users/sriley0/OneDrive - University of Tasmania/RProjects/mrIML/vignettes/articles/gfData.RData")
# Get climate & MEM variables for predictors


data_split <- rsample::initial_split(gfData, prop = 0.7)
data_train <- rsample::training(data_split)
data_test <- rsample::testing(data_split)

X <- gfData %>%
  select(
    contains("bio_"),
    "elevation",
    contains("MEM.")
  ) %>%
  mutate_if(is.integer, as.numeric)
Y <- gfData %>%
  select(contains("GI5")) # GIGANTEA-5 (GI5)

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
testFormula <- as.formula(paste(paste(Yatt$names, collapse = "+"), "~",paste(Xatt$names, collapse = "+")))




# 
# nnEg <- neuralnet::neuralnet(testFormula
#                   , data = data_train
#                   , hidden = 10
#                   , err.fct = "sse"
#                   , linear.output = FALSE
#                   , lifesign = "full"
#                   , rep = 2
#                   , algorithm = "rprop+"
#                   , stepmax = 10000
#                   )
# ## test area

#mrIML_nn <- list(Model = nnEg, Data = list(X = as_tibble(X), Y = as_tibble(Y), X1 = as_tibble(Y)))
# 
# mrIML_nn_fail <- mrIMLpredicts(
#   X = X,
#   Y = Y,
#   X1 = Y,
#   Model = nnEg , 
#   balance_data = 'no',
#   prop = 0.6,
#   k = 5,
#   racing = FALSE
# )

model_mlp <- 
  mlp(penalty = 0, epochs = 100) %>% 
  # This model can be used for classification or regression, so set mode
  set_mode("regression") %>% 
  set_engine("brulee")
#model_mlp

mrIML_mlp <- mrIMLpredicts(
  X = X
  ,Y = Y
  ,X1 = Y
  ,Model = model_mlp
  ,prop = 0.7
  ,racing = FALSE
  
)

# 
# model_mlp_fit <- model_mlp %>% fit(testFormula, data = data_train) 
# 
# nnOutput2 <- predict(model_mlp_fit, data_test)
# 
# 
# #
# plot(nnEg, rep =2)
# nnOutput <- compute(nnEg, rep = 1, data_test)
# 
