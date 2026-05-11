library(rsample)
library(embed)
library(dplyr)
library(ranger)
library(tidyverse)
library(tidymodels)
library(mrIML)
library(future)

library(igraph) # possibly remove this one?
library(ggnetwork)

library(patchwork)

baseDir <- getwd()

source(paste0(baseDir,"/R/mrIMLpredicts.r"))

bsNum = 5
# Set up parallel processing
n_cores <- parallel::detectCores()
options(future.globals.maxSize = +Inf) # This is dirty, really needs optimisation
plan("multisession", workers = n_cores-2)
# plan("multisession", workers = n_cores-8)
# options(future.globals.maxSize = 2 * 1e9)  ## 1.0 GB
# on.exit(options(oopts))

currentDir <- getwd()
loadFile <- paste0(currentDir,"/data/XYForrestData.rdata")
load(loadFile)

timings <- list()


### ----- Replicating the X, Y and X1 of the case study

timings$data$start
X_beetles <- data_site  %>%
  dplyr::select(
    # -Beetles_S_Mat_Indval2,
    # -Beetle_S_Total,
    # -dist_regrowth,
    -Beetles_Mat_Ratio,
    # -Birds_S_Mat_EFL,
    # -Birds_S_Total,
    -Birds_Mat_Ratio,
    -Slope_F,
    -Aspect_Cor)


X_birds <- X_beetles

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

Y_all <- cbind(Y_beetles,Y_birds,Y_plants)

FilterList <- c("Dickanta","Grambill","Athemosc","Nemasqua","Monoglau","Hymepelt","Hymeraru","Eucaobli","Rumoadia","Eucrluci","Eucaregn","Anopglan","Nothcunn","Anodbigl","Blecwatt","CRHON","Decilaus_striatus","Nargomorphus_globulus","Oleaargo","GYFAN","Mandalotus_arciferus","Mandalotus_muscivorus","GNROS","Anotylus_TFIC_sp_04","Pomaapet","Phylaspl","Histinci","Anotylus_TFIC_sp_03")

Y_all2 <-  cbind(Y_beetles,Y_birds,Y_plants) %>%
  dplyr::select(any_of(FilterList))

X1_beetles <- Y_beetles
X1_birds <- Y_birds
X1_plants <- Y_plants
# for (i in attributes(Y_beetles)$names){
#   Y_beetles[[i]] <- as.factor(beetle_data[[i]])
# }
beetle_data <- cbind(X_beetles,Y_beetles)

# beetle_data$Acallistuslongus <- as.factor(beetle_data$Acallistuslongus)



timings$data$finish <- Sys.time()
### ------------- Pre Selection of data using Boruta --------------------------


timings$boruta$start <- Sys.time()
# results = list()
resultsB = data.frame()

for (response in colnames(Y_all)){
  data <- cbind(Y_all[[response]], X_all)
  df1 <- data %>%
    rename(!!response := colnames(data)[1])
  formula <- as.formula(paste(response, "~ ."))
  names_col <- data.frame(rep(response, ncol(X_all)))
  boruta_result <- Boruta::Boruta(formula, data = df1)
  resultsB <- rbind(resultsB,unlist(boruta_result$finalDecision))
}
colnames(resultsB)<-colnames(X_all)
rownames(resultsB)<-colnames(Y_all)
predictorRating <- colMeans(resultsB)
selectColumns <- attributes(predictorRating[predictorRating<2.8])$names

X_all2 <- dplyr::select(X_all,any_of(selectColumns))

# note that due to only removing 3 columns the whole data set is used at this point

all_data <- cbind(X_all,Y_all)
X1_all <- Y_all 

timings$boruta$end <- Sys.time()

### ------ Using modified mrIMLpredicts --------------
# Pre function call

# 3. Model Spec with Tuning Parameters
rf_spec <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 1000
) %>%
  set_mode("classification") %>%
  set_engine("randomForest")


### -------------- Model training ------------------------------
timings$modelTrain_start

## Initial models for all response variables, environment, co-occurance and all data.
# Model 1: Environment only
yhats_all_env <- mrIMLpredicts(
  X = X_all,
  Y = Y_all,
  Model = rf_spec,
  balance_data = 'no',
  tune_grid_size = 5,
  prop = 0.6,
  k = 5,
  racing = TRUE
)

# Model 2: Co-occurrence only
yhats_all_cooccur <- mrIMLpredicts(
  Y = Y_all,
  X1 = X1_all,
  Model = rf_spec,
  balance_data = 'no',
  tune_grid_size = 5,
  prop = 0.6,
  k = 5,
  racing = TRUE
)

# Model 3: Combined
yhats_all_combined <- mrIMLpredicts(
  X = X_all,
  Y = Y_all,
  X1 = X1_beetles,
  Model = rf_spec, 
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE
)

# Model 4: Combined
yhats_all_combined_H <- mrIMLpredicts(
  X = X_all,
  Y = Y_all,
  X1 = X1_all,
  Model = rf_spec, 
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE,
  hierarchy = "Site"
)
timings$modelTrain$finish <- Sys.time()

### ----------------------------------- Bootstrap Analysis ---------------------
timings$bs$start = Sys.time()

bs_all <- mrBootstrap(yhats_all_combined, num_bootstrap = bsNum)


plan("sequential")
bs_all_H <- mrBootstrap(yhats_all_combined_H, num_bootstrap = bsNum)
plan("multisession", workers = n_cores-2)

timings$bs$finish = Sys.time()


### ---------------------------------- Evaluation of performance --------------
## Evaluate model performance beetles
ModelPerf_all_env <- mrIMLperformance(yhats_all_env)
ModelPerf_all_cooccur <- mrIMLperformance(yhats_all_cooccur)
ModelPerf_all_combined <- mrIMLperformance(yhats_all_combined)
ModelPerf_all_combined_H <- mrIMLperformance(yhats_all_combined_H)

perf_comparison_all_env <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_all_env,
  ModelPerf2 = ModelPerf_all_combined_H,
  mode = "classification"
)

perf_comparison_all_cooccur <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_all_cooccur,
  ModelPerf2 = ModelPerf_all_combined_H,
  mode = "classification"
)

perf_comparison_all_combined <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_all_combined,
  ModelPerf2 = ModelPerf_all_combined_H,
  mode = "classification"
)

# Summary table
all_performance <- data.frame(
  Model = c("Environment only", "Co-occurrence only", "Combined","Hierarchy"),
  MCC = c(
    ModelPerf_all_env[[2]],
    ModelPerf_all_cooccur[[2]],
    ModelPerf_all_combined[[2]],
    ModelPerf_all_combined_H[[2]]
  )
)






### --------------------------- Variable Importance Checks --------------------
timings$vip$start <- Sys.time()
# Variable importance
vi_all <- mrVip(
  yhats_all_combined,
  mrBootstrap_obj = bs_all,
  threshold = 0.5,
  global_top_var = 10
)


# Variable importance
vi_all_H <- mrVip(
  yhats_all_combined_H,
  mrBootstrap_obj = bs_all_H,
  threshold = 0.5,
  global_top_var = 10
)
timings$vip$end <- Sys.time()

timings$interactions$start <- Sys.time()

int_all <- mrInteractions(
  yhats_all_combined,
  num_bootstrap = bsNum,
  feature = 'Grambill',
  top_int = 10
)

int_all_H <- mrInteractions(
  yhats_all_combined_H,
  num_bootstrap = bsNum,
  feature = 'Grambill',
  top_int = 10
)

timings$interactions$end <- Sys.time()






### ---------------------------- Cooccurance Networks ------------------------

timings$CN$start = Sys.time()

# Co-occurrence network
filterStrength <- 0.05

assoc_net_all <- mrCoOccurNet(bs_all)
assoc_net_all_filtered <- assoc_net_all %>%
  filter(mean_strength > filterStrength)


assoc_net_all_H <- mrCoOccurNet(bs_all_H)
assoc_net_all_filtered_H <- assoc_net_all_H %>%
  filter(mean_strength > filterStrength)



timings$CN$end = Sys.time()
### ---------------------------- Tables ----------------------------------------

cat("Performance:\n")
all_performance
vi_all[[3]]


### ------------------------- Plots --------------------------------------------


perf_comb=list(env = perf_comparison_all_env$performance_plot,
               cooccr = perf_comparison_all_cooccur$performance_plot,
               comb = perf_comparison_all_combined$performance_plot)


patchwork::wrap_plots(perf_comb)

perf_comparison_all_combined$performance_plot +
  labs(title = "Community Model Performance")

## Network visualization Beetles
if (nrow(assoc_net_all_filtered) > 0) {
  g_all <- graph_from_data_frame(
    assoc_net_all_filtered,
    directed = TRUE
  )

  E(g_all)$Value <- assoc_net_all_filtered$mean_strength
  E(g_all)$Color <- ifelse(
    assoc_net_all_filtered$direction == "negative",
    no = "blue",
    yes = "red"
  )

  gg_all <- ggnetwork(g_all)

  ggplot(gg_all, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
      aes(color = Color, linewidth = Value),
      arrow = arrow(length = unit(5, "pt"), type = "closed")
    ) +
    geom_nodes(color = "gray", size = 3) +
    scale_color_identity() +
    theme_void() +
    geom_nodelabel_repel(aes(label = name), size = 2) +
    labs(title = "Co-occurrence Network")
  } else {
    cat(paste("No significant beetle associations detected at threshold ",filterStrength))
}

## Network visualization Hierarchy
if (nrow(assoc_net_all_filtered_H) > 0) {
  g_all_H <- graph_from_data_frame(
    assoc_net_all_filtered_H,
    directed = TRUE
  )
  
  E(g_all_H)$Value <- assoc_net_all_filtered_H$mean_strength
  E(g_all_H)$Color <- ifelse(
    assoc_net_all_filtered_H$direction == "negative",
    no = "blue",
    yes = "red"
  )
  
  gg_all_H <- ggnetwork(g_all_H)
  
  ggplot(gg_all_H, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(
      aes(color = Color, linewidth = Value),
      arrow = arrow(length = unit(5, "pt"), type = "closed")
    ) +
    geom_nodes(color = "gray", size = 3) +
    scale_color_identity() +
    theme_void() +
    geom_nodelabel_repel(aes(label = name), size = 2) +
    labs(title = "Co-occurrence Network")
} else {
  cat(paste("No significant beetle associations detected at threshold ",filterStrength))
}



# Importance plots
vi_all[[3]]

# With hierarchy context
vi_all_H[[3]]


int_all[[1]] /
  int_all[[2]] /
  int_all[[3]] +
  plot_annotation(title = "Community Interactions")


int_all_H[[1]] /
  int_all_H[[2]] /
  int_all_H[[3]] +
  plot_annotation(title = "Community Interactions with Hierarchy")


# # 1. Grouped Resampling (Critical for site effects)
# # This keeps all observations from a site together in either 'train' or 'test'
# eco_folds <- group_vfold_cv(eco_train, group = site)
# 
# # 2. Recipe with site encoding
# eco_rec <- recipe(biomass ~ ., data = eco_train) %>%
#   step_lmer(site, outcome = vars(biomass)) %>%
#   step_zv(all_predictors())
# 
# # 3. Model Spec with Tuning Parameters
# rf_spec <- rand_forest(
#   mtry = tune(),
#   min_n = tune(),
#   trees = 1000
# ) %>%
#   set_mode("regression") %>%
#   set_engine("ranger")
# 
# # 4. Workflow
# eco_wf <- workflow() %>%
#   add_recipe(eco_rec) %>%
#   add_model(rf_spec)
# 
# # 5. Execute Tuning
# # Use a space-filling design to efficiently test parameters
# set.seed(123)
# tune_res <- tune_grid(
#   eco_wf,
#   resamples = eco_folds,
#   grid = 10,
#   control = control_grid(save_pred = TRUE)
# )
