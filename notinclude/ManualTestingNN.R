library(mrIML)
library(tidymodels)
library(flashlight)
library(ggplot2)
library(patchwork)
library(brulee)

set.seed(7007)

data <- MRFcov::Bird.parasites

# Responses
Y <- select(data, "Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")
# Covariates
X <- select(data, "scale.prop.zos")

model_rf <- parsnip::rand_forest(
  trees = 100, # 100 trees are set for brevity. Aim to start with 1000.
  mode = "classification",
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("randomForest")

model_lm <- parsnip::logistic_reg() %>%
  set_engine("glm")

model_mlp <- 
  mlp(penalty = 0, epochs = 100) %>% 
  # This model can be used for classification or regression, so set mode
  set_mode("classification") %>% 
  set_engine("brulee")


mrIML_rf <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = Y,
  Model = model_rf,
  prop = 0.7,
  k = 5,
  racing = TRUE
)

mrIML_lm <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = Y,
  Model = model_lm , 
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE
)

mrIML_mlp <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = Y,
  Model = model_mlp,
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE
)

perf_rf <- mrIML_rf %>%
  mrIMLperformance()
perf_rf$model_performance

perf_lm <- mrIML_lm %>%
  mrIMLperformance()
perf_lm$model_performance

perf_mlp <- mrIML_mlp %>%
  mrIMLperformance()
perf_mlp$model_performance

perf_comp <- mrPerformancePlot(perf_rf, perf_lm)
perf_comp$performance_plot + perf_comp$performance_diff_plot

### Code Additions

mrIML_rf$Performance <- perf_rf$model_performance
mrIML_lm$Performance <- perf_lm$model_performance
mrIML_mlp$Performance <- perf_mlp$model_performance

### end code additions

fl_rf <- mrIML_rf %>%
  mrFlashlight()

fl_rf$Microfilaria %>%
  light_profile(data = data, v = "scale.prop.zos") %>%
  plot() +
  ggtitle("Effect of scale.prop.zos on Microfilaria") +
  theme_bw()

PD_scale.prop.zos_rf <- mrIML_rf %>%
  mrCovar(var = "scale.prop.zos", sdthresh = 0)

PD_scale.prop.zos_rf[[1]] /
  PD_scale.prop.zos_rf[[2]] /
  PD_scale.prop.zos_rf[[3]] +
  plot_layout(axis = "collect")    
    
PD_scale.prop.zos_lm <- mrIML_lm %>%
  mrCovar(var = "scale.prop.zos", sdthresh = 0)

PD_scale.prop.zos_lm[[1]] /
  PD_scale.prop.zos_lm[[2]] /
  PD_scale.prop.zos_lm[[3]] +
  plot_layout(axis = "collect")


PD_scale.prop.zos_mlp <- mrIML_mlp %>%
  mrCovar(var = "scale.prop.zos", sdthresh = 0)

PD_scale.prop.zos_mlp[[1]] /
  PD_scale.prop.zos_mlp[[2]] /
  PD_scale.prop.zos_mlp[[3]] +
  plot_layout(axis = "collect")

## mrVIP section
# random forest first

vip_rf <- mrIML_rf %>%
  mrVip()

vip_rf[[3]]


mrIML_boot_rf <- mrIML_rf %>%
  mrBootstrap()

mrPdPlotBootstrap(
  mrIML_rf,
  mrBootstrap_obj = mrIML_boot_rf,
  target = "Plas",
  global_top_var = 4
)[[2]]

vip_boot_rf <- mrVip(
  mrIMLobj = mrIML_rf,
  mrBootstrap_obj = mrIML_boot_rf
)

vip_boot_rf[[3]]

# mlp next
vip_mlp <- mrIML_mlp %>%
  mrVip()

vip_mlp[[3]]

mrIML_boot_mlp <- mrIML_mlp %>%
  mrBootstrap()

mrPdPlotBootstrap(
  mrIML_mlp,
  mrBootstrap_obj = mrIML_boot_mlp,
  target = "Plas",
  global_top_var = 4
)[[2]]

vip_boot_mlp <- mrVip(
  mrIMLobj = mrIML_rf,
  mrBootstrap_obj = mrIML_boot_mlp
)

vip_boot_mlp[[3]]



