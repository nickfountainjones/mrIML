# Case study: Ostrich gut microbiome

> This case study is adapted from the mrIML 2.0 supplementary material
> authored by Ryan K. Leadbetter and Nicholas M. Fountain-Jones
> ([source](https://rleadbett.github.io/mrIML-paper-supmat/case-studies/03-ostrich-microbiome/dummy.html)).

## Overview

This case study examines gut microbiome development across three gut
sections — **caecum**, **colon**, and **ileum** — in juvenile ostriches
(*Struthio camelus*). The data span the early post-hatching period when
the microbiome is rapidly assembling, and host characteristics
(primarily age and weight) are expected to be strong drivers alongside
microbial co-occurrence.

Each gut section is modelled independently using three configurations:

1.  **Combined** — host characteristics (X) + microbial co-occurrence
    (X₁)
2.  **Host characteristics only** — X only
3.  **Co-occurrence only** — X₁ only

**Key findings:** A critical developmental window emerges at 20–40 days
post-hatching. Positive associations dominate across all gut sections,
and age and weight are the primary host-level predictors.

``` r

library(here)
library(readxl)
library(tidyverse)
library(DT)
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

# Increase memory allowance for parallel workers (large bootstrap objects)
options(future.globals.maxSize = 10 * 1024^3)
```

## Data preparation

Data are read and pre-processed by a dedicated curation script that
outputs six objects — `Y_caecumF`, `X_caecum`, `Y_colonF`, `X_colon`,
`Y_ileumF`, `X_ileum` — one Y and one X per gut section.

``` r

# Source the data curation script (adjust path to your local directory)
source(
  here("case-studies", "03-ostrich-microbiome", "ostrich_data_curation.r")
)
```

## Model specification

``` r

model_rf <- rand_forest(
  trees = 1000,
  mode  = "classification",
  mtry  = tune(),
  min_n = tune()
) |>
  set_engine("randomForest")
```

------------------------------------------------------------------------

## Caecum

### Model fitting

``` r

X1_caecum <- Y_caecumF   # co-occurrence predictors = all caecum microbes
X_caecum  <- X_caecum    # host characteristics (age, weight, etc.)
Y_caecum  <- Y_caecumF   # response = microbial presence/absence

yhats_rf_caecum_combined <- mrIMLpredicts(
  X = X_caecum, Y = Y_caecum, X1 = X1_caecum,
  Model = model_rf, balance_data = "no",
  tune_grid_size = 5, prop = 0.7, k = 5, racing = FALSE
)

yhats_rf_caecum_noX1 <- mrIMLpredicts(
  X = X_caecum, Y = Y_caecum,
  Model = model_rf, balance_data = "no",
  tune_grid_size = 5, prop = 0.6, k = 5, racing = FALSE
)

yhats_rf_caecum_noX <- mrIMLpredicts(
  X = NULL, Y = Y_caecum, X1 = X1_caecum,
  Model = model_rf, balance_data = "no",
  tune_grid_size = 5, prop = 0.6, k = 5, racing = FALSE
)
```

### Performance

``` r

model_perf_caecum_combined <- mrIMLperformance(yhats_rf_caecum_combined)
model_perf_caecum_noX1     <- mrIMLperformance(yhats_rf_caecum_noX1)
model_perf_caecum_noX      <- mrIMLperformance(yhats_rf_caecum_noX)

data.frame(
  Model = c("Host characteristics", "Co-occurrence only", "Combined"),
  MCC   = c(
    model_perf_caecum_combined[[2]],
    model_perf_caecum_noX1[[2]],
    model_perf_caecum_noX[[2]]
  )
)
```

### Variable importance

``` r

bootstrap_caecum_combined <- mrBootstrap(
  yhats_rf_caecum_combined,
  num_bootstrap = 10
)

vip_caecum_combined <- mrVip(
  yhats_rf_caecum_combined,
  bootstrap_caecum_combined
)

vip_caecum_combined[[3]]
```

### Age effect

Partial-dependence profile for host age across caecum microbiome taxa:

``` r

profilePlot_pd_caecum <- mrCovar(
  yhats_rf_caecum_combined,
  var      = "Age",
  sdthresh = 0.1
)
profilePlot_pd_caecum[[1]]
```

### Co-occurrence network

``` r

assoc_net_caecum <- mrCoOccurNet(bootstrap_caecum_combined)
mrCoOccurNet_plot(
  assoc_net_caecum,
  strength_thresh = 0.1,
  network_title   = "Caecum co-occurrence network",
  nmds_layout     = TRUE,
  degree          = TRUE,
  group_colours   = TRUE
)
```

------------------------------------------------------------------------

## Colon

``` r

X1_colon <- Y_colonF
X_colon  <- X_colon
Y_colon  <- Y_colonF

yhats_rf_colon_combined <- mrIMLpredicts(
  X = X_colon, Y = Y_colon, X1 = X1_colon,
  Model = model_rf, balance_data = "no",
  tune_grid_size = 5, prop = 0.7, k = 5, racing = FALSE
)

yhats_rf_colon_noX1 <- mrIMLpredicts(
  X = X_colon, Y = Y_colon,
  Model = model_rf, balance_data = "no",
  tune_grid_size = 5, prop = 0.6, k = 5, racing = FALSE
)

yhats_rf_colon_noX <- mrIMLpredicts(
  X = NULL, Y = Y_colon, X1 = X1_colon,
  Model = model_rf, balance_data = "no",
  tune_grid_size = 5, prop = 0.6, k = 5, racing = FALSE
)
```

``` r

model_perf_colon_combined <- mrIMLperformance(yhats_rf_colon_combined)
model_perf_colon_noX1     <- mrIMLperformance(yhats_rf_colon_noX1)
model_perf_colon_noX      <- mrIMLperformance(yhats_rf_colon_noX)

data.frame(
  Model = c("Host characteristics", "Co-occurrence only", "Combined"),
  MCC   = c(
    model_perf_colon_combined[[2]],
    model_perf_colon_noX1[[2]],
    model_perf_colon_noX[[2]]
  )
)
```

``` r

bootstrap_colon_combined <- mrBootstrap(
  yhats_rf_colon_combined,
  num_bootstrap = 10
)

vip_colon_combined <- mrVip(
  yhats_rf_colon_combined,
  bootstrap_colon_combined
)

vip_colon_combined[[3]]
```

``` r

profilePlot_pd_colon <- mrCovar(
  yhats_rf_colon_combined,
  var      = "Age",
  sdthresh = 0.1
)
profilePlot_pd_colon[[1]]
```

``` r

assoc_net_colon <- mrCoOccurNet(bootstrap_colon_combined)
mrCoOccurNet_plot(
  assoc_net_colon,
  strength_thresh = 0.1,
  network_title   = "Colon co-occurrence network",
  nmds_layout     = TRUE,
  degree          = TRUE,
  group_colours   = TRUE
)
```

------------------------------------------------------------------------

## Ileum

``` r

X1_ileum <- Y_ileumF
X_ileum  <- X_ileum
Y_ileum  <- Y_ileumF

yhats_rf_ileum_combined <- mrIMLpredicts(
  X = X_ileum, Y = Y_ileum, X1 = X1_ileum,
  Model = model_rf, balance_data = "no",
  tune_grid_size = 5, prop = 0.7, k = 5, racing = FALSE
)

yhats_rf_ileum_noX1 <- mrIMLpredicts(
  X = X_ileum, Y = Y_ileum,
  Model = model_rf, balance_data = "no",
  tune_grid_size = 5, prop = 0.6, k = 5, racing = FALSE
)

yhats_rf_ileum_noX <- mrIMLpredicts(
  X = NULL, Y = Y_ileum, X1 = X1_ileum,
  Model = model_rf, balance_data = "no",
  tune_grid_size = 5, prop = 0.6, k = 5, racing = FALSE
)
```

``` r

model_perf_ileum_combined <- mrIMLperformance(yhats_rf_ileum_combined)
model_perf_ileum_noX1     <- mrIMLperformance(yhats_rf_ileum_noX1)
model_perf_ileum_noX      <- mrIMLperformance(yhats_rf_ileum_noX)

data.frame(
  Model = c("Host characteristics", "Co-occurrence only", "Combined"),
  MCC   = c(
    model_perf_ileum_combined[[2]],
    model_perf_ileum_noX1[[2]],
    model_perf_ileum_noX[[2]]
  )
)
```

``` r

bootstrap_ileum_combined <- mrBootstrap(
  yhats_rf_ileum_combined,
  num_bootstrap = 10
)

vip_ileum_combined <- mrVip(
  yhats_rf_ileum_combined,
  bootstrap_ileum_combined
)

vip_ileum_combined[[3]]
```

``` r

profilePlot_pd_ileum <- mrCovar(
  yhats_rf_ileum_combined,
  var      = "Age",
  sdthresh = 0.1
)
profilePlot_pd_ileum[[1]]
```

``` r

assoc_net_ileum <- mrCoOccurNet(bootstrap_ileum_combined)
mrCoOccurNet_plot(
  assoc_net_ileum,
  strength_thresh = 0.1,
  network_title   = "Ileum co-occurrence network",
  nmds_layout     = TRUE,
  degree          = TRUE,
  group_colours   = TRUE
)
```
