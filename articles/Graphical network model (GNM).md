# Graphical network model (GNM)

This vignette guides you through the new functionality that turns a
`mrIML` multi-response model into a graphical network model (GNM). There
are also new functions in `mrIML (>= 2.0.0)` that can be used in non-GNM
models, such as for single-taxon SDMs, epi models, gene-environment
association (GEA) studies, and landscape genetics. Advances in
[Tidymodels](https://www.tidymodels.org) are at the core of these
functions, as are exciting developments in interpretable machine
learning (e.g., for quickly quantifying interactions through
H-statistics). The capability to capture uncertainty via bootstraps and
spatial patterns are also features of this `mrIML` module. The examples
come from the microbiome world but are, of course, generalizable to any
multi-response problem.

Currently, the models are optimized and tested on presence/absence data.
Models suitable for abundance data will be added in the future.

To demonstrate, first, we need to load some packages and data. The first
example uses coinfection data from New Caledonian *Zosterops* species. A
single continuous covariate is also included (`scale.prop.zos`), which
reflects the relative abundance of *Zosterops* species among different
sample sites.

``` r
library(mrIML)
library(tidymodels)
library(flashlight)
library(ggplot2)
library(patchwork)
library(igraph)
library(ggnetwork)
library(network)

set.seed(7007)
```

``` r
data <- MRFcov::Bird.parasites
Y <-  data %>%
  dplyr::select(-scale.prop.zos) %>%
  dplyr::select(order(everything()))

X <- data %>%
  dplyr::select(
    scale.prop.zos
  )

X1 <- Y
```

Note that the inclusion of `X1` converts `mrIML` into a Joint Species
Distribution Model (JSDM) by directly adding the presence/absence
patterns of the other taxa into the model as predictors (along with
other environmental/host covariates). Note that the order of `X1` and
`Y` needs to match.

## Setting up the models

Built into the `mrIML` architecture (and a big advantage of
`tidymodels`) is the capability to change the underlying model easily.
We are going to set up two models to compare: a random forest model (RF)
and logistic regression (LM). `mrIML` takes advantage of multi-core
processing, so we set it up here to run on 5 cores. These steps are the
same as in `mrIML (< 2.0.0)`.

``` r
future::plan("multisession", workers = 5)
```

``` r
model_rf <-rand_forest(
  trees = 100,
  mode = "classification",
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("randomForest")

model_lm <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
```

## Running the models

Aside from adding JSDM functionality with the `X1` call, we have also
enabled `mrIML` to tune hyperparameters using the very efficient
`racing` option (see Kuhn (2014)). In brief, this racing option takes a
small subsample of parameters and eliminates combinations that do not
improve fit using a repeated-measures ANOVA model with
[`finetune::tune_race_anova()`](https://finetune.tidymodels.org/reference/tune_race_anova.html).
Setting `racing = FALSE` reverts to a grid search if you want to
manually set the tuning grid size (i.e., using
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)).
For logistic regression, there are no parameters to tune, so set
`racing = FALSE`.

``` r
#random forest
yhats_rf <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = Y,
  Model = model_rf,
  prop = 0.7,
  k = 5,
  racing = TRUE
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

#linear model
yhats_lm <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = Y,
  Model = model_lm , 
  balance_data = "no",
  prop = 0.6,
  k = 5,
  racing = FALSE
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
```

## Comparing performance

It’s important to compare whether there are any advantages to using a
random forest approach. Interpretation would be easier overall if
logistic regression gave similar predictive performance.

``` r
ModelPerf_rf <- mrIMLperformance(yhats_rf)

ModelPerf_rf[[1]] #across all parasites
#> # A tibble: 4 × 8
#>   response     model_name roc_AUC   mcc sensitivity   ppv specificity prevalence
#>   <chr>        <chr>        <dbl> <dbl>       <dbl> <dbl>       <dbl>      <dbl>
#> 1 Hzosteropis  rand_fore…   0.931 0.649       0.93  0.894       0.694     0.265 
#> 2 Hkillangoi   rand_fore…   0.780 0.171       0.975 0.893       0.125     0.116 
#> 3 Plas         rand_fore…   0.850 0.555       0.917 0.909       0.630     0.196 
#> 4 Microfilaria rand_fore…   0.921 0.44        0.967 0.928       0.4       0.0980
ModelPerf_rf[[2]] #overall
#> [1] 0.4539035

ModelPerf_lm <- mrIMLperformance(yhats_lm)

ModelPerf_lm[[1]]
#> # A tibble: 4 × 8
#>   response     model_name roc_AUC   mcc sensitivity   ppv specificity prevalence
#>   <chr>        <chr>        <dbl> <dbl>       <dbl> <dbl>       <dbl>      <dbl>
#> 1 Hzosteropis  logistic_…   0.774 0.299       0.886 0.796      0.375      0.265 
#> 2 Hkillangoi   logistic_…   0.807 0           1     0.883      0          0.116 
#> 3 Plas         logistic_…   0.840 0.212       1     0.810      0.0556     0.196 
#> 4 Microfilaria logistic_…   0.919 0.467       0.988 0.930      0.333      0.0980
ModelPerf_lm[[2]]
#> [1] 0.244529

plots <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_lm,
  ModelPerf2 = ModelPerf_rf,
  mode = "classification"
)

plots[[1]] /
plots[[2]]
```

![](Graphical%20network%20model%20(GNM)_files/figure-html/perf-1.png)

``` r
plots[[3]]
#> # A tibble: 4 × 6
#>   response  metric_metric_model1…¹ metric_metric_model2…² outlier_outlier_mode…³
#>   <chr>                      <dbl>                  <dbl> <lgl>                 
#> 1 Hzostero…                  0.299                  0.649 NA                    
#> 2 Hkillang…                  0                      0.171 NA                    
#> 3 Plas                       0.212                  0.555 NA                    
#> 4 Microfil…                  0.467                  0.44  NA                    
#> # ℹ abbreviated names: ¹​metric_metric_model1_logistic_reg,
#> #   ²​metric_metric_model2_rand_forest, ³​outlier_outlier_model1_logistic_reg
#> # ℹ 2 more variables: outlier_outlier_model2_rand_forest <lgl>,
#> #   diff_mod1_2 <dbl>
```

If we just look at the overall AUC values, model performance appears
quite similar (0.87 for the RF and 0.84 for the LM). However, when we
look at the Matthews correlation coefficient (MCC) for each taxon in the
LM model, we see that for *H.killangoi* and Plas (*Plasmodium*), the
values are much lower (an overall MCC of 0.24 for LM is basically just a
guess, compared to 0.45 for the RF). Remember, the classes are
imbalanced, so AUC tends to be an inflated measure. This provides
evidence that non-linear relationships can improve predictive
performance.

Next, we ask whether including putative associations between taxa
improves model performance, or whether the relationship between
parasites and host relative abundance alone is sufficient (i.e.,
assuming independence between the parasites).

``` r
yhats_rf_noAssoc <- mrIMLpredicts(
  X = X,
  Y = Y,
  Model = model_rf,
  prop = 0.7,
  k = 5,
  racing = TRUE
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

ModelPerf_rf_noAssoc <- mrIMLperformance(yhats_rf_noAssoc)

ModelPerf_rf_noAssoc[[1]]
#> # A tibble: 4 × 8
#>   response     model_name roc_AUC   mcc sensitivity   ppv specificity prevalence
#>   <chr>        <chr>        <dbl> <dbl>       <dbl> <dbl>       <dbl>      <dbl>
#> 1 Hzosteropis  rand_fore…   0.764 0.556       0.94  0.855       0.556     0.265 
#> 2 Hkillangoi   rand_fore…   0.545 0           1     0.882       0         0.116 
#> 3 Plas         rand_fore…   0.866 0.630       0.927 0.927       0.704     0.196 
#> 4 Microfilaria rand_fore…   0.856 0           1     0.904       0         0.0980
ModelPerf_rf[[1]] #performance including associations
#> # A tibble: 4 × 8
#>   response     model_name roc_AUC   mcc sensitivity   ppv specificity prevalence
#>   <chr>        <chr>        <dbl> <dbl>       <dbl> <dbl>       <dbl>      <dbl>
#> 1 Hzosteropis  rand_fore…   0.931 0.649       0.93  0.894       0.694     0.265 
#> 2 Hkillangoi   rand_fore…   0.780 0.171       0.975 0.893       0.125     0.116 
#> 3 Plas         rand_fore…   0.850 0.555       0.917 0.909       0.630     0.196 
#> 4 Microfilaria rand_fore…   0.921 0.44        0.967 0.928       0.4       0.0980
```

You can see that overall, including associations improves model
performance, particularly for predicting *H.killangoi* and
*Microfilaria*. Using MCC to compare models is problematic because, in
the association-free model, it’s undefined for these taxa (`NA` values
arise because there are no false negatives for low-prevalence taxa).
Positive predictive value (PPV) is useful in this case and shows that
without associations, we can’t predict the occurrence of these taxa (PPV
= 0 for both). Including associations increases PPV to ~0.2—not great,
as 80% of our positive predictions for these taxa are false.

## Downsampling

Including associations makes a difference, but how can we improve
predictions for our two rarer taxa? Upsampling is possible, but in this
case, we’ll try downsampling to see if correcting class imbalance
improves model fit.

``` r
yhats_rf_downSamp <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = Y,
  Model = model_rf,
  balance_data = "down", #down sampling
  prop = 0.75,
  k = 5,
  racing = TRUE
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%

ModelPerf_rf_downSamp <- mrIMLperformance(yhats_rf_downSamp)
ModelPerf_rf_downSamp[[1]]
#> # A tibble: 4 × 8
#>   response     model_name roc_AUC   mcc sensitivity   ppv specificity prevalence
#>   <chr>        <chr>        <dbl> <dbl>       <dbl> <dbl>       <dbl>      <dbl>
#> 1 Hzosteropis  rand_fore…   0.884 0.657       0.795 0.971       0.933     0.265 
#> 2 Hkillangoi   rand_fore…   0.843 0.327       0.77  0.951       0.692     0.116 
#> 3 Plas         rand_fore…   0.856 0.501       0.747 0.958       0.864     0.196 
#> 4 Microfilaria rand_fore…   0.917 0.434       0.814 0.976       0.818     0.0980
```

Look at those PPV values now—much better. Our false positive rate is
down to \<6% overall. Now that we are happy with the performance of the
model, we can interrogate it further.

## Interpreting the model

In many cases, like this dataset, community or microbiome data tend to
be small in size. Applying stochastic machine learning algorithms to
such data can lead to challenges. For instance, variable importance may
vary substantially when we build multiple models using the same data and
algorithm. To handle this variability and better understand prediction
uncertainty, `mrIML` now includes functionality to capture uncertainty
in the tuned model using bootstrapping. Additionally, this approach
helps estimate how variables affect the response, and these estimates
align with those from traditional linear regression models (see Cook et
al., 2021).

`mrIML` makes it easy to obtain bootstrap estimates for a variety of
interpretable machine learning tools and uses these estimates to
construct marginalized co-occurrence networks. First, let’s perform
bootstrapping and calculate variable importance.

``` r
bs_malaria <- mrBootstrap(
  yhats_rf,
  num_bootstrap = 100,
  downsample = TRUE
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |=======================                                               |  34%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |==============================                                        |  44%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |==========================================================            |  84%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |=================================================================     |  94%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%

bs_impVI <- mrVip(
  mrIMLobj = yhats_rf,
  mrBootstrap_obj = bs_malaria,
  model_perf = ModelPerf_rf
)

bs_impVI[[3]]
```

![](Graphical%20network%20model%20(GNM)_files/figure-html/bootstraps-1.png)

You can see that host abundance is the most important predictor of this
parasite community (followed by *H.zosteropsis*). However, the
sub-figure on the right shows substantial variability. For example,
*H.zosteropsis* is the most important predictor for the occurrence of
*Microfilaria*, and host abundance (shortened to “sc..”) is less
important.

## Bootstrap partial dependence plots

To examine the relationship between each variable and community
structure, `mrIML` has a convenient wrapper to plot bootstrapped partial
dependencies for a taxon of interest.

``` r
pds <- mrPdPlotBootstrap(
  yhats_rf,
  mrBootstrap_obj=bs_malaria,
  vi_obj=bs_impVI,
  target='Plas',
  global_top_var=5
)
pds[[2]]
```

![](Graphical%20network%20model%20(GNM)_files/figure-html/bootstrapped_PDs-1.png)

These plots show that the presence of *Microfilaria* greatly increases
the probability of observing *Plasmodium* (from ~0.47 to 0.70 while
holding all other variables at their mean value). Note that these are
marginal relationships (i.e., isolating the effect of each predictor).
When host abundance is high, the probability of detecting *Plasmodium*
decreases non-linearly (with a threshold around ~0.5). The other
parasites have less effect.

To explore the effect of host abundance overall, we can use the
[`mrCovar()`](https://github.com/nickfountainjones/mrIML/reference/mrCovar.md)
function.

``` r
covar <- mrCovar(
  yhats_rf,
  var = "scale.prop.zos",
  sdthresh = 0.01
)

covar[[1]] /
  covar[[2]] /
  covar[[3]]
```

![](Graphical%20network%20model%20(GNM)_files/figure-html/host_abundance_effect-1.png)

Note that this isn’t bootstrapped—each line represents a taxon. The
second plot shows the community-wide average effect with taxon-specific
effects silhouetted in the background. The third plot shows the
community-wide average change in occurrence probabilities across host
abundance. Note that the occurrence probabilities of all taxa drop at
intermediate levels of host abundance (0.75–1.25).

## Co-occurrence network

We can utilize all the bootstrapped partial dependence estimates (PDs)
to construct a co-occurrence network. The following code converts the
object to an `igraph` object and plots it. This is a directed network,
and edges are scaled by the standard deviation of the marginal change in
prediction (along the PD curves). Red edges are positive associations
(i.e., predicted occurrence increases with the presence of the other
taxon), and blue edges are negative.

``` r
assoc_net<- mrCoOccurNet(bs_malaria)

# Based on simulations (not shown here), we use 
# the following rule of thumb for associations:
# Any association > 0.05 for mean strength is 
# included
assoc_net_filtered <-  assoc_net %>% 
  filter(mean_strength > 0.05)

# Convert mrIML to igraph
g <- graph_from_data_frame(
  assoc_net_filtered,
  directed = TRUE,
  vertices = names(Y)
)

E(g)$Value <- assoc_net_filtered$mean_strength

E(g)$Color <- ifelse(
  assoc_net_filtered$direction == "negative",
  "blue",
  "red"
)

# Convert the igraph to a ggplot with NMDS layout
gg <- ggnetwork(g)

# Plot the graph
gg %>%
  ggplot(
    aes(
      x = x, y = y,
      xend = xend, yend = yend
    )
  ) +
  geom_edges(
    aes(color = Color, linewidth = Value), 
    curvature = 0.2,
    arrow = arrow(
      length = unit(5, "pt"),
      type = "closed"
    )
  ) + 
  geom_nodes(
    color = "gray", 
    size = degree(g, mode = "out")/2
  ) +
  scale_color_identity() +
  theme_void() +
  theme(legend.position = "none")  +
  geom_nodelabel_repel(
    aes(label = name),
    box.padding = unit(0.5, "lines"),
    data = gg,
    size = 3,
    segment.colour = "black",
    colour = "white",
    fill = "grey36"
  ) +
  coord_cartesian(xlim = c(-0.2, 1.2), ylim = c(-0.2, 1.2))
```

![](Graphical%20network%20model%20(GNM)_files/figure-html/network_plot-1.png)

## One-way, two-way, and three-way interactions

Finally, we can quantify the overall importance of interactions in
different taxa models, as well as the importance of one- and two-way
interactions for a specific taxon using the same bootstrapping approach.
To quantify interaction importance, we use H-statistics from the
[`hstats`](https://github.com/mayer79/hstats) package.

``` r
int_ <- mrInteractions(
  yhats_rf,
  num_bootstrap = 100,
  feature = "Plas",
  top_int = 10
)

int_[[1]] / # overall plot
int_[[2]] / # individual plot for feature
int_[[3]]   # two way plot for feature
```

![](Graphical%20network%20model%20(GNM)_files/figure-html/interactions-1.png)

The first plot shows that interactions account for an average of 60%
(with a 90% bootstrap uncertainty interval of 44–81%) of variation in
predictions for *H.killangoi*, and less for the other taxa. The second
plot shows that interactions involving host abundance most affect
predictions of *Plasmodium* (although *H.zosteropis* is also important
and, according to the uncertainty intervals, could be even more
important). This trend is similar community-wide. The last plot shows
that the interaction between *Haemoproteus* presence and host abundance
is the most important two-way interaction for *Plasmodium*, but this
isn’t true community-wide, as host abundance and *H.zosteropis* form the
strongest interaction overall. Taken together, we can see that
interactions between taxa are mediated by host abundance.

Finally, we can explore specific interactions in more detail using 2D
partial dependence plots implemented in the
[`flashlight`](https://mayer79.github.io/flashlight/) package. Below, we
plot how the two-way interaction between host abundance and
*H.zosteropis* impacts the probability of detecting *Plasmodium*.

``` r
fl <- mrFlashlight(
  yhats_rf_downSamp,
  response = "single",
  index = 4 #index=4 selects Plas
)

fl %>%
  light_profile2d(
    c("scale.prop.zos", "Hzosteropis"),
    data
  ) %>%
  plot() +
  theme_bw()
```

![](Graphical%20network%20model%20(GNM)_files/figure-html/two_way_int-1.png)

So, if *H.zosteropsis* is not present and the relative abundance of
*Zosterops* species is low, the probability of observing *Plasmodium* is
high (\>0.7).

### References

Cook et al., 2021: <https://doi.org/10.18651/RWP2021-12> Kuhn (2014):
<https://doi.org/10.48550/arXiv.1405.6974> Fountain-Jones et al(2021):
<https://doi.org/10.1111/1755-0998.13495>
