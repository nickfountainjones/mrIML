# Case study: Avian blood parasites

> This case study is adapted from the mrIML 2.0 supplementary material
> authored by Ryan K. Leadbetter and Nicholas M. Fountain-Jones
> ([source](https://rleadbett.github.io/mrIML-paper-supmat/case-studies/01-avian-parasites/dummy.html)).

## Overview

This case study demonstrates mrIML 2.0 on a small dataset of
vector-borne blood parasites from New Caledonian birds. We analyse
presence/absence data for four parasite species:

- *Haemoproteus zosteropis* species complex
- *H. killangoi* species complex
- *Plasmodium* spp.
- Nematode *Microfilaria* species

The dataset covers 449 individual birds. The single environmental
covariate — `scale.prop.zos` — represents the proportion of captures
attributed to *Zosterops* spp. at each site, serving as a proxy for host
relative abundance.

Three model configurations are compared to tease apart the relative
contributions of host abundance and parasite co-occurrence:

1.  **Combined** — co-occurrence predictors (X₁) + host abundance (X)
2.  **Host abundance only** — X only, no co-occurrence
3.  **Co-occurrence only** — X₁ only, no environmental predictors

``` r

library(tidyverse)
library(DT)
library(here)
library(mrIML)
library(tidymodels)
library(future)
library(finetune)
library(flashlight)
library(igraph)
library(ggnetwork)
library(cowplot)
library(patchwork)

n_cores <- parallel::detectCores()
plan("multisession", workers = n_cores - 2)

set.seed(1234)
```

## Data

The bird parasite dataset is available from the `MRFcov` package.

``` r

data <- MRFcov::Bird.parasites
datatable(data)
```

Separate into response (`Y`), environmental predictors (`X`), and
co-occurrence predictors (`X1`).

``` r
# Response: parasite species presence/absence
Y <- data |>
  dplyr::select(-scale.prop.zos) |>
  dplyr::select(sort(names(x = _)))

# Environmental predictor: host abundance proxy
X <- data |>
  dplyr::select(scale.prop.zos)

# Co-occurrence predictors: all other parasites as predictors for each focal species
X1 <- Y |>
  dplyr::select(sort(names(x = _)))
```

## Model definition

All three configurations share the same base random forest model. `mtry`
and `min_n` are tuned by cross-validation.

``` r

model_rf <- rand_forest(
  trees = 1000,
  mode  = "classification",
  mtry  = tune(),
  min_n = tune()
) |>
  set_engine("randomForest")
```

## Model fitting

### Combined model (co-occurrence + host abundance)

``` r

yhats_rf_combined <- mrIMLpredicts(
  X              = X,
  Y              = Y,
  X1             = X1,
  Model          = model_rf,
  balance_data   = "no",
  tune_grid_size = 5,
  prop           = 0.7,
  k              = 5,
  racing         = TRUE
)
```

### Host abundance only

``` r

yhats_rf_nox1 <- mrIMLpredicts(
  X              = X,
  Y              = Y,
  X1             = NULL,
  Model          = model_rf,
  balance_data   = "down",
  tune_grid_size = 5,
  prop           = 0.7,
  k              = 5,
  racing         = TRUE
)
```

### Co-occurrence only

``` r

yhats_rf_nox <- mrIMLpredicts(
  X              = NULL,
  Y              = Y,
  X1             = X1,
  Model          = model_rf,
  balance_data   = "down",
  tune_grid_size = 5,
  prop           = 0.7,
  k              = 5,
  racing         = TRUE
)
```

## Model performance

``` r

ModelPerf_combined <- mrIMLperformance(yhats_rf_combined)
ModelPerf_combined$model_performance
```

Compare mean AUC across the three configurations:

``` r

ModelPerf_noX1 <- mrIMLperformance(yhats_rf_nox1)
ModelPerf_noX  <- mrIMLperformance(yhats_rf_nox)

tibble(
  Model = c("Combined", "Host abundance only", "Co-occurrence only"),
  AUC   = c(
    mean(ModelPerf_combined$model_performance$roc_AUC),
    mean(ModelPerf_noX1$model_performance$roc_AUC),
    mean(ModelPerf_noX$model_performance$roc_AUC)
  ) |> round(3)
)
```

The combined model outperforms either component alone, indicating that
both host abundance and parasite co-occurrence contribute to parasite
distribution — though with a small dataset, cross-validation variance is
appreciable.

## Bootstrap uncertainty estimation

From here we focus on the combined model. Bootstrapping captures
uncertainty in variable importance and partial-dependence estimates.

``` r

bs_avian <- yhats_rf_combined |>
  mrBootstrap(num_bootstrap = 100)
```

``` r

bs_impVI <- mrVip(
  yhats_rf_combined,
  mrBootstrap_obj = bs_avian,
  threshold       = 0.0,
  global_top_var  = 10,
  local_top_var   = 5,
  model_perf      = ModelPerf_combined
)

bs_impVI[[3]]
```

Host abundance (`scale.prop.zos`) is the dominant predictor overall, but
*H. zosteropis* is the strongest predictor for *Microfilaria* —
illustrating how co-occurrence can be locally more important than
environment.

## Partial dependence

Bootstrapped partial-dependence plots for *Plasmodium*:

``` r

pds <- mrPdPlotBootstrap(
  yhats_rf_combined,
  mrBootstrap_obj = bs_avian,
  vi_obj          = bs_impVI,
  target          = "Plas",
  global_top_var  = 5
)

pds[[2]]
```

## Environmental covariate effects

Examine how host abundance relates to parasite occurrence across all
species:

``` r

covar <- mrCovar(
  yhats_rf_combined,
  var      = "scale.prop.zos",
  sdthresh = 0.01
)

covar[[1]] / covar[[2]] / covar[[3]]
```

## Interaction analysis

H-statistic-based interaction analysis for *Plasmodium*:

``` r

int_ <- mrInteractions(
  yhats_rf_combined,
  num_bootstrap = 100,
  feature       = "Plas",
  top_int       = 10
)

int_[[1]] / int_[[2]] / int_[[3]]
```

## SHAP analysis

SHAP (SHapley Additive exPlanations) values provide individual-level
feature attribution for *Plasmodium* predictions:

``` r

shap_results <- mrShapely(
  yhats_rf_combined,
  taxa                  = "Plas",
  plot_feature_effects  = TRUE,
  plot_dependencies     = TRUE,
  plot_2D_dependencies  = TRUE
)
```

``` r

(shap_results$dependencies2D$Plas[[2]][[2]] +
   scale_colour_viridis_c(limits = c(-0.4, 0.8), option = "B") +
   theme_minimal() +
   labs(title = "b)")) +
(shap_results$dependencies2D$Plas[[1]][[2]] +
   scale_colour_viridis_c(limits = c(-0.4, 0.8), option = "B") +
   theme_minimal() +
   labs(title = "c)")) +
patchwork::plot_layout(ncol = 2, guides = "collect")
```

## Co-occurrence network

Build and visualise the marginalised co-occurrence network from
bootstrap results. Blue edges = positive associations; red = negative.

``` r

assoc_net <- mrCoOccurNet(bs_avian)

mrCoOccurNet_plot(
  assoc_net,
  strength_thresh = 0.05,
  network_title   = "Parasite Co-occurrence Network"
)
```
