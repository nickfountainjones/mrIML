# Multi-response (classification)

`mrIML` is an R package that allows users to generate and interpret
multi-response models (i.e., joint species distribution models)
leveraging advances in data science and machine learning. `mrIML`
couples the [`tidymodels`](https://www.tidymodels.org) infrastructure
developed by Max Kuhn and colleagues with model-agnostic interpretable
machine learning tools to gain insights into multiple response data. As
such, `mrIML` is flexible and easily extendable, allowing users to
construct everything from simple linear models to tree-based methods for
each response using the same syntax and comparisons of predictive
performance. In this vignette, we will guide you through how to apply
this package to landscape genetics problems, but keep in mind that any
multiple-response data can be interrogated using this package (e.g.,
species or OTU presence/absence data).

Let’s start by loading the required packages and data. The data we are
going to use is from Fountain-Jones et al. (2017) and consists of single
nucleotide polymorphism (SNP) data from a virus (feline immunodeficiency
virus, FIV) infecting bobcats in Southern California.

``` r
library(mrIML)
#> No methods found in package 'workflows' for request: 'predict.workflow' when loading 'mrIML'
library(tidymodels)
#> ── Attaching packages ────────────────────────────────────── tidymodels 1.4.1 ──
#> ✔ broom        1.0.10     ✔ recipes      1.3.1 
#> ✔ dials        1.4.2      ✔ rsample      1.3.1 
#> ✔ dplyr        1.1.4      ✔ tailor       0.1.0 
#> ✔ ggplot2      4.0.0      ✔ tidyr        1.3.1 
#> ✔ infer        1.0.9      ✔ tune         2.0.1 
#> ✔ modeldata    1.5.1      ✔ workflows    1.3.0 
#> ✔ parsnip      1.3.3      ✔ workflowsets 1.1.1 
#> ✔ purrr        1.2.0      ✔ yardstick    1.3.2
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ purrr::discard() masks scales::discard()
#> ✖ dplyr::filter()  masks stats::filter()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ recipes::step()  masks stats::step()
library(flashlight)

set.seed(7007)
```

``` r
load("Features.RData")
load("Responsedata.RData")

# Print first 6 cols
head(Features)[1:6]
#>      Grassland Shrub.Scrub      Forest   HighlyDev       Urban    Suburban
#> X21 0.07001059   0.5569084 0.010719958 0.000000000 0.000000000 0.003573319
#> X22 0.06766419   0.7673464 0.030587924 0.000000000 0.000000000 0.132680085
#> X23 0.18449304   0.5236581 0.008614977 0.002253148 0.001590457 0.013253810
#> X24 0.09806776   0.7858655 0.000661726 0.000000000 0.000000000 0.001191106
#> X27 0.15776500   0.8416187 0.000616270 0.000000000 0.000000000 0.000000000
#> X31 0.08730321   0.6391331 0.008791658 0.000000000 0.000000000 0.007360458
head(Responsedata)[1:6]
#>     env_1 env_10 env_100 env_104 env_107 env_108
#> X21     0      0       1       0       0       0
#> X22     0      0       0       0       0       0
#> X23     0      0       0       0       1       0
#> X24     0      0       0       1       0       0
#> X27     0      0       0       0       1       0
#> X31     1      0       0       1       0       0
```

When constructing these types of models, SNPs that are common (occur in
\>80% of samples) or rare (\<10% of samples) are difficult to model.
This can be a problem for potentially adaptive loci, which tend to be
rarer. We provide down-sampling or up-sampling strategies for dealing
with unbalanced data (see below), but filtering out common and rare loci
is another option. The more response variables (SNPs) you include, the
longer the computational time, so for this example, we will drop SNPs
occurring in \<40% or \>70% of individuals. This leaves 29 SNPs
remaining. In practice, these filtering steps are stringent, so \<20%
and \>80% may be more appropriate.

``` r
# Define the responses
Y <- filterRareCommon(
  Responsedata,
  lower = 0.4,
  higher = 0.7
) 

# Define the predictors
X <- Features %>%
  select(where(~ n_distinct(.) > 1))
```

If your feature data consists of landscape resistance data generated
from Circuitscape or similar, we provide functionality that can extract
each surface from a folder and generate resistance components that can
be used in these models (using principal coordinate analysis). This data
can be easily merged with other non-matrix data (e.g., age/sex) if
needed. There were no resistance surfaces here, but if there were, we
could load them as follows:

``` r
R <- resist_components(
  filename = "FILE_PATH", p_val = 0.01
)

# Add to predictors
X <- cbind(R, X)
```

Now all the data is loaded and ready to go we can formulate the model
using tidymodel syntax. In this case we have binary data (SNP
presence/absence at each loci) but the data could also be counts or
continuous (the `mode` argument would be `"regression"` instead of
`"classification"`). The user can specify any model from the tidymodel
universe (see <https://www.tidymodels.org/find/> for details). However,
we have done most of our testing on random forests (RF), xgr boost and
glms (generalized linear models). Here we will specify an RF
classification model to fit each response.

``` r
model_rf <- rand_forest(
  trees = 100,
  mode = "classification",
  mtry = tune(),
  min_n = tune()
) %>%
 set_engine("randomForest")
```

Now we can run the model. Hyper parameter tuning (for algorithms that
have hyper parameters) is done automatically by testing how model
performance changes across a random grid and the best performing
combination is kept. The only choice to make is to either down/up sample
data or leave it as is. As our viral data set is small, we will not do
any up- or down-sampling (i.e.,`balance_data = "no"`). We can also
implement parallel processing for larger data sets to speed up the
`mrIML` workflow.

``` r
future::plan("multisession", workers = 4)
```

``` r
yhats_rf <- mrIMLpredicts(
  X = X,
  Y = Y,
  Model = model_rf,
  tune_grid_size = 5,
  k = 5,
  racing = FALSE
)
```

It takes a couple of minutes to run on a laptop and there are warnings
that you can ignore (this data set is small). The above code constructs
tuned random forest models for the features (`Y`) you have selected for
each response (29 viral SNP in this case). We can then check the
performance of each model separately and overall. The performance
metrics we provide for classification models are area under curve
(`roc_AUC`), Mathew’s correlation coefficient (`mcc` - a good metric
when data is imbalanced i.e., unequal numbers of 0/1s in each response),
specificity (proportion correctly classified not having the mutation at
that loci) and sensitivity (proportion correctly classified having the
mutation at that loci). We also provide “prevalence”, which tells you
how common that SNP was in the population sampled.

``` r
ModelPerf_rf <- mrIMLperformance(yhats_rf)
ModelPerf_rf$model_performance
#> # A tibble: 29 × 8
#>    response model_name roc_AUC    mcc sensitivity     ppv specificity prevalence
#>    <chr>    <chr>        <dbl>  <dbl>       <dbl>   <dbl>       <dbl>      <dbl>
#>  1 env_131  rand_fore…   0.625 0            1       0.571       0          0.421
#>  2 env_163  rand_fore…   0.583 0            0     NaN           1          0.632
#>  3 env_164  rand_fore…   0.833 0.167        0.5     0.667       0.667      0.421
#>  4 env_167  rand_fore…   0.5   0            1       0.571       0          0.421
#>  5 env_169  rand_fore…   0.333 0            1       0.571       0          0.421
#>  6 env_212  rand_fore…   0.75  0            0     NaN           1          0.684
#>  7 env_23   rand_fore…   0.583 0            0     NaN           1          0.632
#>  8 env_24   rand_fore…   0.833 0.0913       0.75    0.6         0.333      0.421
#>  9 env_41   rand_fore…   0.5   0.333        0.667   0.667       0.667      0.474
#> 10 env_47   rand_fore…   0.667 0            0.667   0.5         0.333      0.474
#> # ℹ 19 more rows
ModelPerf_rf$global_performance_summary
#> [1] 0.1360853
```

You can compare the predictive performance of random forests with any
other technique simply by altering the following. Lets try a logistic
regression. Note that for logistic regression you may need to
scale/transform the data or create dummy variables if you include
categorical variables. These steps can easily be added to the pipeline
following tidymodel syntax.

``` r
# Define LM
model_glm <- logistic_reg() %>%
  set_engine("glm")

# Fit mrIML model
yhats_glm <- mrIMLpredicts(
  X = X,
  Y = Y,
  Model = model_glm,
  tune_grid_size = 5,
  k = 5,
  racing = FALSE
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==                                                                    |   3%  |                                                                              |=====                                                                 |   7%  |                                                                              |=======                                                               |  10%  |                                                                              |==========                                                            |  14%  |                                                                              |============                                                          |  17%  |                                                                              |==============                                                        |  21%  |                                                                              |=================                                                     |  24%  |                                                                              |===================                                                   |  28%  |                                                                              |======================                                                |  31%  |                                                                              |========================                                              |  34%  |                                                                              |===========================                                           |  38%  |                                                                              |=============================                                         |  41%  |                                                                              |===============================                                       |  45%  |                                                                              |==================================                                    |  48%  |                                                                              |====================================                                  |  52%  |                                                                              |=======================================                               |  55%  |                                                                              |=========================================                             |  59%  |                                                                              |===========================================                           |  62%  |                                                                              |==============================================                        |  66%  |                                                                              |================================================                      |  69%  |                                                                              |===================================================                   |  72%  |                                                                              |=====================================================                 |  76%  |                                                                              |========================================================              |  79%  |                                                                              |==========================================================            |  83%  |                                                                              |============================================================          |  86%  |                                                                              |===============================================================       |  90%  |                                                                              |=================================================================     |  93%  |                                                                              |====================================================================  |  97%  |                                                                              |======================================================================| 100%

# Get performance
ModelPerf_glm <- mrIMLperformance(yhats_glm)
ModelPerf_glm$global_performance_summary
#> [1] 0.2014733
```

You can see that the random forests model outperforms the logistic
regression in this case (0.2014733 versus 0.1360853). Once we are happy
with the underlying model we can then dive in to see how this model
predicts the SNPs. First we can check variable importance.

``` r
glm_impVI <- mrVip(
  yhats_glm,
  threshold = 0.5,
  global_top_var = 10,
  local_top_var = 5,
  model_perf = ModelPerf_glm
)
glm_impVI[[3]]
```

![](Multi-response%20(classification)_files/figure-html/var-imp-1.png)

The output of
[`mrVip()`](https://github.com/nickfountainjones/mrIML/reference/mrVip.md)
contains a figure summarising variable imporance. The main plot on the
left shows the global importance (what features shape genetic change
overall) and the sub plots on the right show the individual models (with
an option to filter by the mcc value using `threshold`). We also provide
functionality to assess outlier SNPs utilizing a PCA on importance
scores.

``` r
glm_impVI_PCA <- glm_impVI %>%
  mrVipPCA()
glm_impVI_PCA[[1]]
```

![](Multi-response%20(classification)_files/figure-html/unnamed-chunk-2-1.png)

We can see only one outlier was detected on PC3 (env24) indicating that
this SNP was bets predicted by a different set of predictors. Looking at
the indiivudal importance plots shows that, unlike the other SNPs,
forest cover was the best predictor for env24. See the regression
vignette for more details.

If your features can be grouped together this can make it easier to
interpret variable importance plots. The order of the groups should
match the order of the columns in the X data, for example:

``` r
groupCov <- c(
  rep("Host_characteristics", 1),
  rep("Urbanisation", 3),
  rep("Vegetation", 2),
  rep("Urbanisation", 1),
  rep("Spatial", 2),
  rep("Host_relatedness", 6),
  rep("Host_characteristics", 1),
  rep("Vegetation", 2),
  rep("Urbanisation",1)
)
```

`mrIML` allows a full suite of model agnostic interpretable machine
learning tools to be applied to individual models or the “global”
throught
[`mrFlashlight()`](https://github.com/nickfountainjones/mrIML/reference/mrFlashlight.md).
We utilize the R package
[`flashlight`](https://mayer79.github.io/flashlight/) which offers an
exciting and ever growing tool set to interpret these kinds of models.
For example, once we create the flashlight objects, we can plot the
predicted values of a feature for each response.

``` r
fl <- mrFlashlight(
  yhats_rf,
  response = "single",
  index = 1
)

fl %>%
  light_scatter(
    v = "Grassland",
    type = "predicted"
  ) %>%
  plot()
```

![](Multi-response%20(classification)_files/figure-html/flashlight-1.png)

`mrIMl` also provides a wraper function for some flashlight
functionality to visualize the marginal (i.e. partial dependencies) or
conditional (accumulated local effects) effect of a feature on the
responses. Partial dependencies take longer to calculate and are more
sensitive to correlated features, therefore the results of the two can
vary. For example, in the two plots bellow generated with
[`mrCovar()`](https://github.com/nickfountainjones/mrIML/reference/mrCovar.md),
the results differ slightly but not significantly. ALE plots are a
better option if your feature set is even moderately impacted by
collinearity (e.g., rho = 0.6). The second plot is the overall smoothed
genetic-turnover function.

``` r
# Partial dependencies
profilePlot_pd <- mrCovar(
  yhats_rf,
  var = "Grassland",
  sdthresh = 0.05
)
profilePlot_pd[[1]] +
  xlim(0, NA)
```

![](Multi-response%20(classification)_files/figure-html/marginal-effects-1.png)

``` r

# Acumulated local effects
profilePlot_ale <- mrCovar(
  yhats_rf,
  var = "Grassland",
  sdthresh = 0.05,
  type = "ale"
)
profilePlot_ale[[1]] +
  xlim(0, NA)
```

![](Multi-response%20(classification)_files/figure-html/marginal-effects-2.png)

Finally, we can assess how features interact overall to shape genetic
change using
[`mrInteractions()`](https://github.com/nickfountainjones/mrIML/reference/mrInteractions.md).
Be warned, this is memory intensive. Future updates to this package will
enable users to visualize these interactions and explore them in more
detail using 2D ALE plots for example.

``` r
interactions <- mrInteractions(
  yhats_rf,
  feature = "pol_105",
  num_bootstrap = 1,
  top_int = 5
)

interactions[[1]] /
  interactions[[2]] /
  interactions[[3]]
```

![](Multi-response%20(classification)_files/figure-html/interactions-1.png)

You could easily compare the plots generated above from our random
forest model with the logistic regression model we fit earlier.

You can visualize detected interactions by converting the `mrIML` object
into a flashlight object and using the
[`flashlight::light_profile2d()`](https://rdrr.io/pkg/flashlight/man/light_profile2d.html)
function. In effect, the resulting heatmap shows the average prediction
of the interacting features across loci/response variables. Both ALEs
and partial dependencies can be used - but we highly recommend using
ALEs for the most robust results (see Molnar (2019) for more detail)

``` r
fl <- mrFlashlight(
  yhats_rf,
  response = "single",
  index = 1
)

fl %>%
  light_profile2d(
    c("Forest", "Grassland"),
    cbind(Y, X)
  ) %>%
  plot() +
  theme_bw()
```

![](Multi-response%20(classification)_files/figure-html/two-way-interaction-1.png)

This is just an example of the tools can be applied to better interpret
multi-response models and in turn gain insights into how landscape
impacts genetic change. We’ve demonstrated that this package is modular
and flexible, allowing future work on ‘tidymodels’ and interpretable
machine learning to easily be incorporated. The predictions from these
models, for example, can be used to identify individual SNPs that may be
adaptive assess gene flow across space (future updates) or to help
better formulate probabilistic models such as generalized dissimilarity
models (GDMs).
