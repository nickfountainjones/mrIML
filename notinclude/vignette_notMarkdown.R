# ---
#   title: "Example 3: Forest Communities"
# author: "Simon Riley, Ryan Leadbetter, Nick Fountain-Jones"
# date: "2026-04-01"
# output:   
#   html_document:
#   self_contained: true
# editor_options: 
#   markdown: 
#   wrap: 72
# ---
#   
#   ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
# ```
# 
# ## Introduction
# 
# This case study demonstrates *MrIML 2.0* on a larger data set with
# multiple taxonomic groups across a forest successional gradient. We
# analyze 144 plots established across forest age categories:
#   
#   -   **Mature forest**: Unharvested with no fire disturbance for ≥60
# years
# -   **\~45 years post-harvest**
#   -   **\~27 years post-harvest**
#   -   **\~7 years post-harvest**
#   
#   At each plot, we recorded ground-active beetles, plants, and birds,
# along with environmental covariates including coarse wood debris, leaf
# litter depth, and soil variables (pH, conductivity).
# 
# The following is a series of random forests models to demonstrate different ways
# models can be interpreted including use of co-occurrences, interactions and 
# highlighting of variable importance.
# 
# 
# The process within this vignette is:
#   -   Importing Data
# -   Data Preparation
# -   Model Creation
# -   Bootstrapping Analysis
# -   Variable Importance Assessment
# -   Creation of co-occurrence networks
# 
# ```{r libraryLoading, echo=TRUE, include=FALSE}

timings <- list()
timings$knit$start <- Sys.time()

library(rsample, warn.conflicts=F, quietly=TRUE)
library(embed, warn.conflicts=F, quietly=TRUE)
library(dplyr, warn.conflicts=F, quietly=TRUE)
library(randomForest, warn.conflicts=F, quietly=TRUE)
library(tidyverse, warn.conflicts=F, quietly=TRUE)
library(tidymodels, warn.conflicts=F, quietly=TRUE)
library(mrIML, warn.conflicts=F, quietly=TRUE)
library(future, warn.conflicts=F, quietly=TRUE)
library(igraph)

library(patchwork, warn.conflicts=F, quietly=TRUE)
library(ggnetwork)
# Set up parallel processing

n_cores <- parallel::detectCores()
options(future.globals.maxSize = +Inf) # This is dirty, really needs optimisation
plan("multisession", workers = n_cores-2)

# Value for bootstrapping ()
bsNum <- 3
set.seed(1)
# ```
# 
# ## Data Loading
# 
# Data is supplied with this case study in a single rdata file. There are
# 4 data-frames, one containing all the environmental predictors and one for
# each of the observations types (beetles, birds and plants).
# 
# 
# ```{r Data Loading, echo=TRUE}

currentDir <- getwd()
# currentDir <- "C:/Users/Nebula PC/Documents/mrIML_HR"
currentDir <- "C:/Users/sriley0/OneDrive - University of Tasmania/RProjects/mrIML_HR/mrIML_HR/"
sourceFile <- paste0(currentDir,"/R/mrIMLpredicts.R")
source(sourceFile) #This needs to be removed when file is complete
loadFile <- paste0(currentDir,"/data/XYForrestData.rdata")
load(loadFile)

timings <- list()
# 
# ```
# 
# ## Data Preparation
# 
# Derived columns are removed and variables are arranged into X
# (predictor), Y (response) and X1 (co-predictor) for each of beetles,
# birds and plants.Data is converted to presence absence data.
# 
# Response variables are filtered using a mrIML function to remove species that
# are either too common (over 70% prevalence) or too rare (less than 20% 
#                          prevalence) to provide value in the analysis.
# 
# 
# ```{r Data Preparation, echo=TRUE}
timings$data$start <- Sys.time()

X_all <- data_site %>%
  dplyr::select(
    -any_of("Beetles_Mat_Ratio"),
    -any_of("Birds_Mat_Ratio"),
    -any_of("Slope_F"),
    -any_of("Aspect_Cor"),
    -any_of("Vegetation_cover"),
    -contains("Bryo"),
    -contains("Moss"),
    -contains('Plant')    
  )

X_beetles <- data_site %>%
  dplyr::select(
    -any_of("Beetles_Mat_Ratio")#,
    # -any_of("Slope_F"),
    # -any_of("Aspect_Cor")    
  )

X_birds <- data_site %>%
  dplyr::select(
    -any_of("Birds_Mat_Ratio")#,
    # -any_of("Slope_F"),
    # -any_of("Aspect_Cor")    
  )

X_plants <- data_site %>%
  select(
    -contains("Bryo"),
    -contains("Moss"),
    -contains('Plant'),
    -Vegetation_cover)


Y_beetles <- data_beetles %>%
  dplyr::select(-Site, -Distance, -Transect, -Age) %>%
  dplyr::mutate_all(~ ifelse(. > 0, 1, .)) %>%
  mrIML::filterRareCommon(lower = 0.2, higher = 0.7)


Y_birds <- data_birds %>%
  dplyr::select(-Site, -Distance, -Transect, -Age) %>%
  mutate_all(~ ifelse(. > 0, 1, .)) %>%
  mrIML::filterRareCommon(lower = 0.2, higher = 0.7)

Y_plants <- data_plants %>%
  dplyr::select(-Site, -Distance, -Transect, -Age) %>%
  mutate_all(~ ifelse(. > 0, 1, .))%>%
  mrIML::filterRareCommon(lower = 0.2, higher = 0.7)

X1_beetles <- Y_beetles
X1_birds <- Y_birds
X1_plants <- Y_plants

Y_all <- cbind(Y_beetles,Y_birds,Y_plants)

timings$data$finish <- Sys.time()

# ```
# ## Data Summaries {.tabset}
# ### Data
# 
# Each of these tabs contain a summary either the raw or filtered version of the
# data that will be used in the models. 
# 
# ### Beetles Raw
# ```{r Beetles Raw Table}
dim(data_beetles)
summary(data_beetles)
# ```
# ### Beetles Filtered
# ```{r Beetles Filtered Table}
dim(Y_beetles)
summary(Y_beetles)
# ```
# ### Plants Raw
# ```{r Plants Raw Table}
dim(data_plants)
summary(data_plants)
# ```
# ### Plants Filtered
# ```{r Plants Filtered Table}
dim(Y_plants)
summary(Y_plants)
# ```
# ### Birds Raw
# ```{r Birds Raw Table}
dim(data_birds)
summary(data_birds)
# ```
# ### Birds Filtered
# ```{r Birds Filtered Table}
dim(Y_birds)
summary(Y_birds)
# ```
# 
# ## {.unlisted .unnumbered}
# 
# 
# ## Model Building {.tabset}
# 
# ### Model Training
# 
# A common model tidy model specification is created.
# 
# ```{r building models, echo = TRUE, results=TRUE}
timings$modelTemplate$start <- Sys.time()
# Model Spec with Tuning Parameters
rf_spec <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 1000
) %>%
  set_mode("classification") %>%
  set_engine("randomForest")

# ```
# 
# Models are trained for environment without co-occurrence, co-occurrence
# without the environmental variable, a combined model and a combined model
# grouped by site.
# 
# 
# ### Beetles
# 
# The output here is 4 models for prediction of the presence of beetles.
# 
# ```{r Training Beetle, echo= TRUE, results="hide", message= FALSE, warning=FALSE}
timings$beetleTrain$start <- Sys.time()

## Initial models for all response variables, environment, co-occurance and all data.
# Model 1: Environment only

yhats_beetles_env <- mrIMLpredicts(
  X = X_beetles,
  Y = Y_beetles,
  Model = rf_spec,
  balance_data = 'no',
  tune_grid_size = 5,
  prop = 0.6,
  k = 5,
  racing = TRUE
)


# Model 2: Co-occurrence only
yhats_beetles_cooccur <- mrIMLpredicts(
  Y = Y_beetles,
  X1 = X1_beetles,
  Model = rf_spec,
  balance_data = 'no',
  tune_grid_size = 5,
  prop = 0.6,
  k = 5,
  racing = TRUE
)

# Model 3: Combined
yhats_beetles_combined <- mrIMLpredicts(
  X = X_beetles,
  Y = Y_beetles,
  X1 = X1_beetles,
  Model = rf_spec,
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE
)

# Model 4: Combined
yhats_beetles_combined_H <- mrIMLpredicts(
  X = X_beetles,
  Y = Y_beetles,
  X1 = X1_beetles,
  Model = rf_spec,
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE,
  hierarchy = "Site"
)
timings$beetleTrain$finish <- Sys.time()
# 
# ```
# 
# ### Birds
# 
# The output here is 4 models for prediction of the presence of birds
# 
# ```{r Training Birds, echo= TRUE, results="hide", message= FALSE, warning=FALSE}

timings$birdTrain$start <- Sys.time()

yhats_birds_env <- mrIMLpredicts(
  X = X_birds,
  Y = Y_birds,
  Model = rf_spec,
  balance_data = 'no',
  tune_grid_size = 5,
  prop = 0.6,
  k = 5,
  racing = TRUE
)


# Model 2: Co-occurrence only
yhats_birds_cooccur <- mrIMLpredicts(
  Y = Y_birds,
  X1 = X1_birds,
  Model = rf_spec,
  balance_data = 'no',
  tune_grid_size = 5,
  prop = 0.6,
  k = 5,
  racing = TRUE
)

# Model 3: Combined
yhats_birds_combined <- mrIMLpredicts(
  X = X_birds,
  Y = Y_birds,
  X1 = X1_birds,
  Model = rf_spec,
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE
)

# Model 4: Combined
yhats_birds_combined_H <- mrIMLpredicts(
  X = X_birds,
  Y = Y_birds,
  X1 = X1_birds,
  Model = rf_spec,
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE,
  hierarchy = "Site"
)
timings$birdTrain$finish <- Sys.time()
# 
# ```
# 
# ### Plants
# 
# The output here is 4 models for prediction of the presence of plants.
# 
# ```{r Training Plants, echo= TRUE, results="hide", message= FALSE, warning=FALSE}

timings$plantTrain$start <- Sys.time()

yhats_plants_env <- mrIMLpredicts(
  X = X_plants,
  Y = Y_plants,
  Model = rf_spec,
  balance_data = 'no',
  tune_grid_size = 5,
  prop = 0.6,
  k = 5,
  racing = TRUE
)


# Model 2: Co-occurrence only
yhats_plants_cooccur <- mrIMLpredicts(
  Y = Y_plants,
  X1 = X1_plants,
  Model = rf_spec,
  balance_data = 'no',
  tune_grid_size = 5,
  prop = 0.6,
  k = 5,
  racing = TRUE
)

# Model 3: Combined
yhats_plants_combined <- mrIMLpredicts(
  X = X_plants,
  Y = Y_plants,
  X1 = X1_plants,
  Model = rf_spec,
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE
)

# Model 4: Combined
yhats_plants_combined_H <- mrIMLpredicts(
  X = X_plants,
  Y = Y_plants,
  X1 = X1_plants,
  Model = rf_spec,
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE,
  hierarchy = "Site"
)
timings$plantTrain$finish <- Sys.time()

# ```
# ## {.unlisted .unnumbered}
# 
# 
# ## Evaluation of performance {.tabset}
# ### Code
# Model outputs are evaluated.
# 
# ```{r Evaluation of performance, echo=TRUE, warning = FALSE}
timings$Evaluation$start <- Sys.time()
## Evaluate model performance beetles
ModelPerf_beetles_env <- mrIMLperformance(yhats_beetles_env)
ModelPerf_beetles_cooccur <- mrIMLperformance(yhats_beetles_cooccur)
ModelPerf_beetles_combined <- mrIMLperformance(yhats_beetles_combined)
ModelPerf_beetles_combined_H <- mrIMLperformance(yhats_beetles_combined_H)

perf_comparison_beetles_env <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_beetles_env,
  ModelPerf2 = ModelPerf_beetles_combined_H,
  mode = "classification"
)

perf_comparison_beetles_cooccur <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_beetles_cooccur,
  ModelPerf2 = ModelPerf_beetles_combined_H,
  mode = "classification"
)

perf_comparison_beetles_combined <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_beetles_combined,
  ModelPerf2 = ModelPerf_beetles_combined_H,
  mode = "classification"
)
perf_comparison_beetles_combined$performance_plot$data$model_name <- gsub("model1_rand_forest","Combined",perf_comparison_beetles_combined$performance_plot$data$model_name)
perf_comparison_beetles_combined$performance_plot$data$model_name <- gsub("model2_rand_forest","Hierarchy",perf_comparison_beetles_combined$performance_plot$data$model_name)
perf_comparison_beetles_combined$performance_plot$labels$x <- "Models"

# Summary table
all_performance_beetles <- data.frame(
  Model = c("Environment only", "Co-occurrence only", "Combined","Hierarchy"),
  MCC = c(
    ModelPerf_beetles_env[[2]],
    ModelPerf_beetles_cooccur[[2]],
    ModelPerf_beetles_combined[[2]],
    ModelPerf_beetles_combined_H[[2]]
  )
)

## Evaluate model performance birds
ModelPerf_birds_env <- mrIMLperformance(yhats_birds_env)
ModelPerf_birds_cooccur <- mrIMLperformance(yhats_birds_cooccur)
ModelPerf_birds_combined <- mrIMLperformance(yhats_birds_combined)
ModelPerf_birds_combined_H <- mrIMLperformance(yhats_birds_combined_H)

perf_comparison_birds_combined <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_birds_combined,
  ModelPerf2 = ModelPerf_birds_combined_H,
  mode = "classification"
)
perf_comparison_birds_combined$performance_plot$data$model_name <- gsub("model1_rand_forest","Combined",perf_comparison_birds_combined$performance_plot$data$model_name)
perf_comparison_birds_combined$performance_plot$data$model_name <- gsub("model2_rand_forest","Hierarchy",perf_comparison_birds_combined$performance_plot$data$model_name)
perf_comparison_birds_combined$performance_plot$labels$x <- "Models"

# Summary table
all_performance_birds <- data.frame(
  Model = c("Environment only", "Co-occurrence only", "Combined","Hierarchy"),
  MCC = c(
    ModelPerf_birds_env[[2]],
    ModelPerf_birds_cooccur[[2]],
    ModelPerf_birds_combined[[2]],
    ModelPerf_birds_combined_H[[2]]
  )
)

## Evaluate model performance plants
ModelPerf_plants_env <- mrIMLperformance(yhats_plants_env)
ModelPerf_plants_cooccur <- mrIMLperformance(yhats_plants_cooccur)
ModelPerf_plants_combined <- mrIMLperformance(yhats_plants_combined)
ModelPerf_plants_combined_H <- mrIMLperformance(yhats_plants_combined_H)

perf_comparison_plants_combined <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_plants_combined,
  ModelPerf2 = ModelPerf_plants_combined_H,
  mode = "classification"
)
perf_comparison_plants_combined$performance_plot$data$model_name <- gsub("model1_rand_forest","Combined",perf_comparison_plants_combined$performance_plot$data$model_name)
perf_comparison_plants_combined$performance_plot$data$model_name <- gsub("model2_rand_forest","Hierarchy",perf_comparison_plants_combined$performance_plot$data$model_name)
perf_comparison_plants_combined$performance_plot$labels$x <- "Models"
# Summary table
all_performance_plants <- data.frame(
  Model = c("Environment only", "Co-occurrence only", "Combined","Hierarchy"),
  MCC = c(
    ModelPerf_plants_env[[2]],
    ModelPerf_plants_cooccur[[2]],
    ModelPerf_plants_combined[[2]],
    ModelPerf_plants_combined_H[[2]]
  )
)
timings$Evaluation$finish <- Sys.time()
# ```
# 
# ### MCC Tables
# ```{r Table Outputs, echo = TRUE}
all_performance_beetles
all_performance_birds
all_performance_plants
# ```
# 
# ### Beetle Plots
# Initially we look at the performance comparison plots
# 
# ```{r Plots - performance comparision, echo=TRUE, warning= FALSE}

perf_comparison_beetles_combined$performance_plot +
  labs(title = "Community Model Performance")
# ```
# 
# ### Bird Plots
# Initially we look at the performance comparison plots
# 
# ```{r Plots - performance comparision birds, echo=TRUE, warning= FALSE}

perf_comparison_birds_combined$performance_plot +
  labs(title = "Community Model Performance")
# ```
# 
# ### Plant Plots
# ```{r Plots - performance comparision plants, echo=TRUE, warning= FALSE}

perf_comparison_plants_combined$performance_plot +
  labs(title = "Community Model Performance")
# ```
# 
# ## {.unlisted .unnumbered}
# 
# ## Bootstrapping
# 
# Bootstrapping is done to the combined data model and the hierarchical
# data model to get a better estimate of the population properties.
# 
# ```{r Bootstrap, echo=TRUE, results="hide", message= FALSE, warning=FALSE}
timings$bs_beetle$start = Sys.time()

bs_beetles <- mrBootstrap(yhats_beetles_combined, num_bootstrap = bsNum)
plan("sequential") 
bs_beetles_H <- mrBootstrap(yhats_beetles_combined_H, num_bootstrap = bsNum)
plan("multisession", workers = n_cores-2)
timings$bs_beetle$finish = Sys.time()

timings$bs_bird$start = Sys.time()
bs_birds <- mrBootstrap(yhats_birds_combined, num_bootstrap = bsNum)
plan("sequential")
bs_birds_H <- mrBootstrap(yhats_birds_combined_H, num_bootstrap = bsNum)
plan("multisession", workers = n_cores-2)
timings$bs_bird$finish = Sys.time()

timings$bs_plants$start = Sys.time()
bs_plants <- mrBootstrap(yhats_plants_combined, num_bootstrap = bsNum)

plan("sequential")
bs_plants_H <- mrBootstrap(yhats_plants_combined_H, num_bootstrap = bsNum)
plan("multisession", workers = n_cores-2)

timings$bs_plants$finish = Sys.time()
# ```
# 
# ## Checking Variable Importance
# 
# Using the bootstrapping information variables are analysed for predictive power.
# 
# ```{r Variable Importance Checks, echo = TRUE, warning=FALSE}
timings$vip_beetles$start <- Sys.time()
# Variable importance
vi_beetles <- mrVip(
  yhats_beetles_combined,
  mrBootstrap_obj = bs_beetles,
  threshold = 0.5,
  global_top_var = 10
)

# Variable importance
vi_beetles_H <- mrVip(
  yhats_beetles_combined_H,
  mrBootstrap_obj = bs_beetles_H,
  threshold = 0.5,
  global_top_var = 10
)
timings$vip_beetles$finish <- Sys.time()

timings$vip_plants$start <- Sys.time()
vi_plants <- mrVip(
  yhats_plants_combined,
  mrBootstrap_obj = bs_plants,
  threshold = 0.5,
  global_top_var = 10
)

vi_plants_H <- mrVip(
  yhats_plants_combined_H,
  mrBootstrap_obj = bs_plants_H,
  threshold = 0.5,
  global_top_var = 10
)
timings$vip_plants$finish <- Sys.time()

timings$vip_birds$start <- Sys.time()
vi_birds <- mrVip(
  yhats_birds_combined,
  mrBootstrap_obj = bs_birds,
  threshold = 0.5,
  global_top_var = 10
)

vi_birds_H <- mrVip(
  yhats_birds_combined_H,
  mrBootstrap_obj = bs_birds_H,
  threshold = 0.5,
  global_top_var = 10
)
timings$vip_birds$finish <- Sys.time()
# 
# ```
# 
# 
# 
# 
# 
# 
# ## Checking for interactions {.tabset}
# 
# Using the mrInteractions function the strength of interactions for a specific 
# species which have been selected from the variable importance check done in the
# last segement. 
# 
# ### Beetle Processing
# ```{r Checking for beetle interations, echo=TRUE}
timings$interact_beetles$start <- Sys.time()

beetle_feature <- 'Decilaus_lateralis'#'Rhabdotus_reflexus'#

# Analyze interactions in beetle community
int_beetles <- mrInteractions(
  yhats_beetles_combined,
  num_bootstrap = bsNum,
  feature = beetle_feature,
  top_int = 10
)
int_beetles[[1]]$data <- int_beetles[[1]]$data[1:10, , drop = FALSE] 
int_beetles[[2]]$data <- int_beetles[[2]]$data[1:10, , drop = FALSE]
int_beetles[[2]]$labels$title <- "One-way Interactions"
int_beetles[[3]]$data <- int_beetles[[3]]$data[1:10, , drop = FALSE] 
int_beetles[[3]]$labels$title <- "Two-way Interactions"


int_beetles_H <- mrInteractions(
  yhats_beetles_combined_H,
  num_bootstrap = bsNum,
  feature = beetle_feature,
  top_int = 10
)
int_beetles_H[[1]]$data <- int_beetles_H[[1]]$data[1:10, , drop = FALSE] 
int_beetles_H[[2]]$data <- int_beetles_H[[2]]$data[1:10, , drop = FALSE]
int_beetles_H[[2]]$labels$title <- "One-way Interactions Hierarchy" 
int_beetles_H[[3]]$data <- int_beetles_H[[3]]$data[1:10, , drop = FALSE] 
int_beetles_H[[3]]$labels$title <- "Two-way Interactions Hierarchy" 
timings$interact_beetles$finish <- Sys.time()



# ```
# 
# ### Beetle Output
# 
# ```{r Displaying Beetle Interactions}
int_beetles[[1]] + int_beetles_H[[1]]
int_beetles[[2]] + int_beetles_H[[2]]
int_beetles[[3]] + int_beetles_H[[3]]

# ```
# 
# ### Plant Processing
# ```{r checking plant interactions, echo = TRUE}
timings$interact_plants$start <- Sys.time()
plant_feature <- 'Nothcunn'
int_plants <- mrInteractions(
  yhats_plants_combined,
  num_bootstrap = bsNum,
  feature = plant_feature,
  top_int = 10
)

int_plants[[1]]$data <- int_plants[[1]]$data[1:10, , drop = FALSE] 
int_plants[[2]]$data <- int_plants[[2]]$data[1:10, , drop = FALSE] 
int_plants[[2]]$labels$title <- "One-way Interactions" 
int_plants[[3]]$data <- int_plants[[3]]$data[1:10, , drop = FALSE] 
int_plants[[2]]$labels$title <- "Two-way Interactions" 

int_plants_H <- mrInteractions(
  yhats_plants_combined_H,
  num_bootstrap = bsNum,
  feature = plant_feature,
  top_int = 10
)
int_plants_H[[1]]$data <- int_plants_H[[1]]$data[1:10, , drop = FALSE] 
int_plants_H[[2]]$data <- int_plants_H[[2]]$data[1:10, , drop = FALSE]
int_plants_H[[2]]$labels$title <- "One-way Interactions Hierarchy"  
int_plants_H[[3]]$data <- int_plants_H[[3]]$data[1:10, , drop = FALSE] 
int_plants_H[[3]]$labels$title <- "Two-way Interactions Hierarchy"  
timings$interact_plants$finish <- Sys.time()
# ```
# 
# ### Plant Output
# 
# ```{r Displaying Plant Interactions}
int_plants[[1]] + int_plants_H[[1]]
int_plants[[2]] + int_plants_H[[2]]
int_plants[[3]] + int_plants_H[[3]]

# ```
# 
# ### Bird Processing
# ```{r checking Bird interactions, echo = TRUE}
# Analyze interactions in beetle community
timings$interact_birds$start <- Sys.time()
int_birds <- mrInteractions(
  yhats_birds_combined,
  num_bootstrap = bsNum,
  feature = 'CRHON',
  top_int = 10
)
int_birds[[2]]$data <- int_birds[[2]]$data[1:10, , drop = FALSE] 
int_birds[[3]]$data <- int_birds[[3]]$data[1:10, , drop = FALSE] 

int_birds_H <- mrInteractions(
  yhats_birds_combined_H,
  num_bootstrap = bsNum,
  feature = 'CRHON',
  top_int = 10
)
int_birds_H[[2]]$data <- int_birds_H[[2]]$data[1:10, , drop = FALSE] 
int_birds_H[[3]]$data <- int_birds_H[[3]]$data[1:10, , drop = FALSE] 

timings$interact_birds$finish <- Sys.time()
# ```
# 
# ### Bird Output
# 
# ```{r Displaying Bird Interactions}
int_birds[[1]] + int_birds_H[[1]]
int_birds[[2]] + int_birds_H[[2]]
int_birds[[3]] + int_birds_H[[3]]


timings$CN_beetles$start = Sys.time()

# Co-occurrence network
filterStrength <- 0.1

assoc_net_beetles <- mrCoOccurNet(bs_beetles)
assoc_net_beetles_filtered <- assoc_net_beetles %>%
  filter(mean_strength > filterStrength)


assoc_net_beetles_H <- mrCoOccurNet(bs_beetles_H)
assoc_net_beetles_filtered_H <- assoc_net_beetles_H %>%
  filter(mean_strength > filterStrength)

timings$CN_beetles$end = Sys.time()

## Network visualization Beetles
g_beetles <- graph_from_data_frame(assoc_net_beetles_filtered, directed = TRUE)
g_beetles_H <- graph_from_data_frame(assoc_net_beetles_filtered_H, directed = TRUE)
## Network visualization Beetles
if (length(g_beetles) > 0) {
  
  
  E(g_beetles)$Value <- assoc_net_beetles_filtered$mean_strength
  E(g_beetles)$Color <- ifelse(
    assoc_net_beetles_filtered$direction == "negative",
    "blue",
    "red"
  )
  
  # Use circular layout for better visualization
  circle_layout <- layout_in_circle(g_beetles)
  gg_beetles <- ggnetwork(g_beetles, layout = circle_layout)
  
  ggplot(gg_beetles, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
      aes(color = Color, linewidth = Value),
      curvature = 0.1,
      arrow = arrow(length = unit(5, "pt"), type = "closed")
    ) +
    geom_nodes(color = "gray", size = degree(g_beetles, mode = "out") + 2) +
    scale_color_identity() +
    theme_void() +
    geom_nodelabel_repel(
      aes(label = name),
      size = 2,
      colour = "white",
      fill = "grey36"
    ) +
    labs(
      title = "Beetles Co-occurrence Network"
    )
} else {
  cat(paste("No significant beetle associations detected at threshold ",filterStrength))
}

## Network visualization Hierarchy
if (length(g_beetles_H) > 0) {
  
  E(g_beetles_H)$Value <- assoc_net_beetles_filtered_H$mean_strength
  E(g_beetles_H)$Color <- ifelse(
    assoc_net_beetles_filtered_H$direction == "negative",
    "blue",
    "red"
  )
  
  # Use circular layout for better visualization
  circle_layout_H <- layout_in_circle(g_beetles_H)
  gg_beetles_H <- ggnetwork(g_beetles_H, layout = circle_layout_H)
  
  ggplot(gg_beetles_H, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
      aes(color = Color, linewidth = Value),
      curvature = 0.1,
      arrow = arrow(length = unit(5, "pt"), type = "closed")
    ) +
    geom_nodes(color = "gray", size = degree(g_beetles_H, mode = "out") + 2) +
    scale_color_identity() +
    theme_void() +
    geom_nodelabel_repel(
      aes(label = name),
      size = 2,
      colour = "white",
      fill = "grey36"
    ) +
    labs(
      title = "Beetles Co-occurrence Network with Heirarchy"
    )
} else {
  cat(paste("No significant beetle associations detected at threshold ",filterStrength))
}


timings$CN_birds$start = Sys.time()

# Co-occurrence network
# filterStrength <- 0.1

assoc_net_birds <- mrCoOccurNet(bs_birds)
assoc_net_birds_filtered <- assoc_net_birds %>%
  filter(mean_strength > filterStrength)


assoc_net_birds_H <- mrCoOccurNet(bs_birds_H)
assoc_net_birds_filtered_H <- assoc_net_birds_H %>%
  filter(mean_strength > filterStrength)

timings$CN_birds$end = Sys.time()


## Network visualization Beetles
g_birds <- graph_from_data_frame(assoc_net_birds_filtered, directed = TRUE)
g_birds_H <- graph_from_data_frame(assoc_net_birds_filtered_H, directed = TRUE)
## Network visualization Beetles
if (length(g_birds) > 0) {
  
  
  E(g_birds)$Value <- assoc_net_birds_filtered$mean_strength
  E(g_birds)$Color <- ifelse(
    assoc_net_birds_filtered$direction == "negative",
    "blue",
    "red"
  )
  
  # Use circular layout for better visualization
  circle_layout <- layout_in_circle(g_birds)
  gg_birds <- ggnetwork(g_birds, layout = circle_layout)
  
  CoPlot_birds <- ggplot(gg_birds, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
      aes(color = Color, linewidth = Value),
      curvature = 0.1,
      arrow = arrow(length = unit(5, "pt"), type = "closed")
    ) +
    geom_nodes(color = "gray", size = degree(g_birds, mode = "out") + 2) +
    scale_color_identity() +
    theme_void() +
    geom_nodelabel_repel(
      aes(label = name),
      size = 2,
      colour = "white",
      fill = "grey36"
    ) +
    labs(
      title = "Bird Co-occurrence Network"
    )
} else {
  cat(paste("No significant bird associations detected at threshold ",filterStrength," "))
  CoPlot_birds <- plot_spacer()
}

## Network visualization Hierarchy
if (length(g_birds_H) > 0) {
  
  E(g_birds_H)$Value <- assoc_net_birds_filtered_H$mean_strength
  E(g_birds_H)$Color <- ifelse(
    assoc_net_birds_filtered_H$direction == "negative",
    "blue",
    "red"
  )
  
  # Use circular layout for better visualization
  circle_layout_H <- layout_in_circle(g_birds_H)
  gg_birds_H <- ggnetwork(g_birds_H, layout = circle_layout_H)
  
  CoPlot_birds_H <- ggplot(gg_birds_H, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
      aes(color = Color, linewidth = Value),
      curvature = 0.1,
      arrow = arrow(length = unit(5, "pt"), type = "closed")
    ) +
    geom_nodes(color = "gray", size = degree(g_birds_H, mode = "out") + 2) +
    scale_color_identity() +
    theme_void() +
    geom_nodelabel_repel(
      aes(label = name),
      size = 2,
      colour = "white",
      fill = "grey36"
    ) +
    labs(
      title = "Bird Co-occurrence Network with Heirarchy"
    )
} else {
  cat(paste("No significant beetle associations detected at threshold ",filterStrength," "))
  CoPlot_birds_H <- plot_spacer()
}

CoPlot_birds + CoPlot_birds_H



timings$CN_plants$start = Sys.time()

# Co-occurrence network
filterStrength <- 0.1

assoc_net_plants <- mrCoOccurNet(bs_plants)
assoc_net_plants_filtered <- assoc_net_plants %>%
  filter(mean_strength > filterStrength)


assoc_net_plants_H <- mrCoOccurNet(bs_plants_H)
assoc_net_plants_filtered_H <- assoc_net_plants_H %>%
  filter(mean_strength > filterStrength)

timings$CN_plants$end = Sys.time()


## Network visualization Beetles
g_plants <- graph_from_data_frame(assoc_net_plants_filtered, directed = TRUE)
g_plants_H <- graph_from_data_frame(assoc_net_plants_filtered_H, directed = TRUE)
## Network visualization Beetles
if (length(g_plants) > 0) {
  
  
  E(g_plants)$Value <- assoc_net_plants_filtered$mean_strength
  E(g_plants)$Color <- ifelse(
    assoc_net_plants_filtered$direction == "negative",
    "blue",
    "red"
  )
  
  # Use circular layout for better visualization
  circle_layout <- layout_in_circle(g_plants)
  gg_plants <- ggnetwork(g_plants, layout = circle_layout)
  
  CoPlot_plants <- ggplot(gg_plants, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
      aes(color = Color, linewidth = Value),
      curvature = 0.1,
      arrow = arrow(length = unit(5, "pt"), type = "closed")
    ) +
    geom_nodes(color = "gray", size = degree(g_plants, mode = "out") + 2) +
    scale_color_identity() +
    theme_void() +
    geom_nodelabel_repel(
      aes(label = name),
      size = 2,
      colour = "white",
      fill = "grey36"
    ) +
    labs(
      title = "Plant Co-occurrence Network",
      subtitle = "Strong positive associations between epiphytic species"
    )
} else {
  cat(paste("No significant beetle associations detected at threshold ",filterStrength))
  CoPlot_plants <- plot_spacer()
}

## Network visualization Hierarchy
if (length(g_plants_H) > 0) {
  
  E(g_plants_H)$Value <- assoc_net_plants_filtered_H$mean_strength
  E(g_plants_H)$Color <- ifelse(
    assoc_net_plants_filtered_H$direction == "negative",
    "blue",
    "red"
  )
  
  # Use circular layout for better visualization
  circle_layout_H <- layout_in_circle(g_plants_H)
  gg_plants_H <- ggnetwork(g_plants_H, layout = circle_layout_H)
  
  CoPlot_plants_H <- ggplot(gg_plants_H, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
      aes(color = Color, linewidth = Value),
      curvature = 0.1,
      arrow = arrow(length = unit(5, "pt"), type = "closed")
    ) +
    geom_nodes(color = "gray", size = degree(g_plants_H, mode = "out") + 2) +
    scale_color_identity() +
    theme_void() +
    geom_nodelabel_repel(
      aes(label = name),
      size = 2,
      colour = "white",
      fill = "grey36"
    ) +
    labs(
      title = "Plant Co-occurrence Network",
      subtitle = "Strong positive associations between epiphytic species"
    )
} else {
  cat(paste("No significant beetle associations detected at threshold ",filterStrength))
  CoPlot_plants_H <- plot_spacer()
}

CoPlot_plants + CoPlot_plants_H 

vi_beetles[[3]]
vi_beetles_H[[3]]

vi_birds[[3]]
vi_birds_H[[3]]

vi_plants[[3]]
vi_plants_H[[3]]

timings$knit$finish <- Sys.time()

## Processing time
A breakdown of the processing time require for each step is below.
print("Data Load:")
print(timings$data$finish - timings$data$start)

print("Beetle Model:")
print(timings$beetleTrain$finish-timings$beetleTrain$start)
print("Bird Model:")
print(timings$birdTrain$finish-timings$birdTrain$start)
print("Plant Model:")
print(timings$plantTrain$finish-timings$plantTrain$start)

print("Beetle Bootstrapping")
print(timings$bs_beetle$finish-timings$bs_beetle$start)
print("Bird Bootstrapping")
print(timings$bs_bird$finish-timings$bs_bird$start)
print("Plant Bootstrapping")
print(timings$bs_plants$finish-timings$bs_plants$start)

print("Variable Importance Analysis Beetles")
print(timings$vip_beetles$finish-timings$vip_beetles$start)
print("Variable Importance Analysis Beetles")
print(timings$vip_birds$finish-timings$vip_birds$start)
print("Variable Importance Analysis Beetles")
print(timings$vip_plants$finish-timings$vip_plants$start)

print("Interactions Beetles")
print(timings$interact_beetles$finish-timings$interact_beetles$start)
print("Interactions Birds")
print(timings$interact_birds$finish-timings$interact_birds$start)
print("Interactions Plants")
print(timings$interact_plants$finish-timings$interact_plants$start)


print("Total Time")
print(timings$knit$finish - timings$knit$start)



