# Multi-response (regression)

`mrIML` is an R package that allows users to generate and interpret
multi-response models (i.e., joint species distribution models)
leveraging advances in data science and machine learning. `mrIML`
couples the [`tidymodels`](https://www.tidymodels.org) infrastructure,
developed by Max Kuhn and colleagues, with model-agnostic interpretable
machine learning tools to gain insights into multiple response data.
`mrIML` is flexible and easily extendable, allowing users to construct
everything from simple linear models to tree-based methods for each
response using the same syntax and compare them under the same
predictive performance criteria.

In this vignette, we will guide you through how to apply this package to
ecological genomics problems using the regression functionality of the
package. The data set we’ll use comes from Fitzpatrick et al. 2014, who
examined adaptive genetic variation in relation to geography and climate
adaptation (current and future) in balsam poplar (Populus balsamifera).
See Ecology Letters, (2014) doi: 10.1111/ele.12376. In this paper, they
used the similar gradient forests routine (see Ellis et al. 2012
Ecology), and we show that `mrIML` can not only provide more flexible
model choice and interpretive capabilities but can derive new insights
into the relationship between climate and genetic variation. Further, we
show that linear models of each loci have slightly greater predictive
performance.

``` r
# Read in data file with minor allele freqs & env/space variables
load("gfData.RData")

# Get climate & MEM variables for predictors
X <- gfData %>%
  select(
    contains("bio_"),
    "elevation",
    contains("MEM.")
  ) %>%
  mutate_if(is.integer, as.numeric)
Y <- gfData %>%
  select(contains("GI5")) # GIGANTEA-5 (GI5)
```

We focus on the adaptive SNP loci from GIGANTEA-5 (GI5) gene that has
known links to stem development, plant circadian clock, and light
perception pathway. The data represents the proportion of individuals in
each population with that SNP loci.

### Parallel processing

`mrIML` uses the flexible
[`future.apply::future_lapply()`](https://future.apply.futureverse.org)
to set up multi-core processing. This can greatly speed up the more
computationally expensive `mrIML` functions. In the example below, we
set up a cluster using 4 cores. If you don’t set up a cluster, the
default settings will be used and the analysis will run sequentially.

``` r
future::plan("multisession", workers = 4)
```

### Building and comparing models

Performing the analysis is very similar to our classification example.
Let’s start by constructing a linear model for this data set. We set
Model 1 to a linear regression. See <https://www.tidymodels.org/find/>
for other regression model options. Note that ‘mode’ must be
‘regression’ and in `mrIML`predicts, model has to be set to
‘regression’.

``` r
model_lm <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

yhats_lm <- mrIMLpredicts(
  X = X,
  Y = Y,
  Model = model_lm,
  prop = 0.7,
  tune_grid_size = 10,
  k = 10,
  racing = FALSE
)
```

Model performance can be examined the same way as in the [classification
example](https://nickfountainjones.github.io/mrIML/articles/Multi-response%20(classification).html),
however the reported performance metrics are different. Running
[`mrIMLperformance()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLperformance.md)
on a regression model provides the root mean square error (rmse) and R².

``` r
ModelPerf_lm <- mrIMLperformance(yhats_lm)
ModelPerf_lm$model_performance
#> # A tibble: 20 × 4
#>    response           model_name   rmse  rsquared
#>    <chr>              <chr>       <dbl>     <dbl>
#>  1 CANDIDATE_GI5_108  linear_reg 0.0440  0.290   
#>  2 CANDIDATE_GI5_198  linear_reg 0.140   0.392   
#>  3 CANDIDATE_GI5_268  linear_reg 0.0847  0.182   
#>  4 CANDIDATE_GI5_92   linear_reg 0.0965  0.116   
#>  5 CANDIDATE_GI5_1950 linear_reg 0.243   0.000340
#>  6 CANDIDATE_GI5_2382 linear_reg 0.137   0.00896 
#>  7 CANDIDATE_GI5_2405 linear_reg 0.145   0.398   
#>  8 CANDIDATE_GI5_2612 linear_reg 0.120   0.585   
#>  9 CANDIDATE_GI5_2641 linear_reg 0.0669  0.120   
#> 10 CANDIDATE_GI5_33   linear_reg 0.167   0.618   
#> 11 CANDIDATE_GI5_3966 linear_reg 0.178   0.0244  
#> 12 CANDIDATE_GI5_5033 linear_reg 0.0725  0.00277 
#> 13 CANDIDATE_GI5_5090 linear_reg 0.136   0.262   
#> 14 CANDIDATE_GI5_5119 linear_reg 0.121   0.0118  
#> 15 CANDIDATE_GI5_8997 linear_reg 0.194   0.233   
#> 16 CANDIDATE_GI5_9287 linear_reg 0.0632  0.0150  
#> 17 CANDIDATE_GI5_9447 linear_reg 0.0816 NA       
#> 18 CANDIDATE_GI5_9551 linear_reg 0.151   0.192   
#> 19 CANDIDATE_GI5_9585 linear_reg 0.177   0.321   
#> 20 CANDIDATE_GI5_9659 linear_reg 0.151   0.637
ModelPerf_lm$global_performance_summary
#> [1] 0.1284043
```

You can see that the overall R² is 0.13 but there is substantial
variation in predictive performance across loci.

Let’s compare the performance of the linear model to that of a random
forest. Random forest is the computational engine in gradient forests.
Notice for random forests we have two hyperparameters to tune: `mtry`
(number of features to randomly include at each split) and `min_n` (the
minimum number of data points in a node that are required for the node
to be split further). The syntax `tune()` acts as a placeholder to tell
`mrIML` to tune those hyperparameters across a grid of values (defined
in `mrIMLpredicts` `tune_grid_size` argument). Different algorithms will
have different hyperparameters; see
<https://www.tidymodels.org/find/parsnip/> for parameter details. Note
that large grid sizes (\>10) for algorithms with lots of hyperparameters
(such as extreme gradient boosting) will be computationally demanding.
In this case we choose a grid size of 5.

``` r
model_rf <- rand_forest(
  trees = 100,
  mode = "regression",
  mtry = tune(),
  min_n = tune()
) %>%
 set_engine("randomForest")

yhats_rf <- mrIMLpredicts(
  X = X,
  Y = Y,
  Model = model_rf,
  tune_grid_size = 5
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |====                                                                  |   5%  |                                                                              |=======                                                               |  10%  |                                                                              |==========                                                            |  15%  |                                                                              |==============                                                        |  20%  |                                                                              |==================                                                    |  25%  |                                                                              |=====================                                                 |  30%  |                                                                              |========================                                              |  35%  |                                                                              |============================                                          |  40%  |                                                                              |================================                                      |  45%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================                                |  55%  |                                                                              |==========================================                            |  60%  |                                                                              |==============================================                        |  65%  |                                                                              |=================================================                     |  70%  |                                                                              |====================================================                  |  75%  |                                                                              |========================================================              |  80%  |                                                                              |============================================================          |  85%  |                                                                              |===============================================================       |  90%  |                                                                              |==================================================================    |  95%  |                                                                              |======================================================================| 100%

ModelPerf_rf <- mrIMLperformance(yhats_rf)

ModelPerf_rf$model_performance
#> # A tibble: 20 × 4
#>    response           model_name    rmse rsquared
#>    <chr>              <chr>        <dbl>    <dbl>
#>  1 CANDIDATE_GI5_108  rand_forest 0.0299 0.101   
#>  2 CANDIDATE_GI5_198  rand_forest 0.0759 0.545   
#>  3 CANDIDATE_GI5_268  rand_forest 0.0812 0.682   
#>  4 CANDIDATE_GI5_92   rand_forest 0.160  0.233   
#>  5 CANDIDATE_GI5_1950 rand_forest 0.142  0.223   
#>  6 CANDIDATE_GI5_2382 rand_forest 0.0718 0.00386 
#>  7 CANDIDATE_GI5_2405 rand_forest 0.107  0.455   
#>  8 CANDIDATE_GI5_2612 rand_forest 0.142  0.106   
#>  9 CANDIDATE_GI5_2641 rand_forest 0.0784 0.0119  
#> 10 CANDIDATE_GI5_33   rand_forest 0.159  0.247   
#> 11 CANDIDATE_GI5_3966 rand_forest 0.112  0.271   
#> 12 CANDIDATE_GI5_5033 rand_forest 0.0420 0.547   
#> 13 CANDIDATE_GI5_5090 rand_forest 0.0946 0.534   
#> 14 CANDIDATE_GI5_5119 rand_forest 0.123  0.0967  
#> 15 CANDIDATE_GI5_8997 rand_forest 0.171  0.502   
#> 16 CANDIDATE_GI5_9287 rand_forest 0.0538 0.0107  
#> 17 CANDIDATE_GI5_9447 rand_forest 0.141  0.000372
#> 18 CANDIDATE_GI5_9551 rand_forest 0.120  0.403   
#> 19 CANDIDATE_GI5_9585 rand_forest 0.137  0.379   
#> 20 CANDIDATE_GI5_9659 rand_forest 0.115  0.487
ModelPerf_rf$global_performance_summary
#> [1] 0.1078119

#easier to see with plots
plots <- mrPerformancePlot(
  ModelPerf1 = ModelPerf_lm,
  ModelPerf2 = ModelPerf_rf,
  mode = "regression"
) 

plots[[1]] /
plots[[2]]
```

![](Multi-response%20(regression)_files/figure-html/fit-rf-1.png)

You can see that predictive performance is actually slightly less using
RF (overall R² = 0.11) but for some loci RF does better than our LM and
sometimes worse. Which to choose? Generally, the simpler the better, the
linear model in this case, but it depends on how important you think
non-linear responses are. In future versions of `mrIML` we will
implement ensemble models that will overcome this issue. However, for
the time being, we’ll need to interrogate the model and make our own
informed decision.

### Interpreting your models

A first step is to look at variable importance. We do this below for the
RF model using
[`mrVip()`](https://github.com/nickfountainjones/mrIML/reference/mrVip.md).

``` r
VI <- mrVip(
  yhats_rf,
  mrBootstrap_obj = NULL,
  threshold = 0.1,
  global_top_var = 10,
  local_top_var = 5,
  taxa = "CANDIDATE_GI5_9585"
) 

VI[[3]]
```

![](Multi-response%20(regression)_files/figure-html/rf-vip-1.png)

In the above figure, the top left plot shows the ranked most important
variables across all models collectively, while the subplots in the top
right show the most important variables in the top few models (the
number of variables and models to visualize can be controlled using the
`global_top_var`, `local_top_var`, and `threshold` arguments of
[`mrVip()`](https://github.com/nickfountainjones/mrIML/reference/mrVip.md),
see `?mrVip()`). The bottom plot shows the most important variables in a
specific response model, which we defined by
`taxa = "CANDIDATE_GI5_9585"`. Generally, `bio_10` (mean summer
temperature), `bio_1` (mean annual temperature), and `bio_18` (summer
precipitation) are the most influential predictor variables, however
this varies quite a lot across the loci. Summer precipitation was not as
important in Fitzpatrick et al. but otherwise these results are similar.

We can also perform PCA of the variable importance scores in the
different models to group loci that behave similarly and to identify
outliers.

``` r
# PCA
VI_PCA <- VI %>%
  mrVipPCA()

VI_PCA$eigenvalues
#>  [1] 3.49956317 1.86919887 1.76923607 1.07149165 0.85216131 0.68184584
#>  [7] 0.64438746 0.38556353 0.13248180 0.05604386 0.03802643
VI_PCA[[1]]
```

![](Multi-response%20(regression)_files/figure-html/rf-vip-PCA-1.png)

The right plot of the first two PCs shows that candidate 5119, 9287,
5033, 92, and 108 are shaped similarly by the features we included and
may, for example, be products of linked selection. The left plot shows
that most of the variability in the variable importance data is captured
by the first three principal components.

Note that you can also supply bootstraps for importance scores in
[`mrVip()`](https://github.com/nickfountainjones/mrIML/reference/mrVip.md),
but this functionality is still under development for regression models.

Next, we can explore the model further by plotting the relationships
between our SNPs and a feature in our set. Let’s choose `bio_1` (mean
annual temperature) and plot the individual and global (average of all
SNPs) partial dependency (PD) plots.

``` r
PD_bio1 <- mrCovar(
  yhats_rf,
  var = "bio_1",
  sdthresh = 0.01
)

(PD_bio1[[1]] + ylim(0, 0.4)) /
  (PD_bio1[[2]] + ylim(0, 0.4)) /
  (PD_bio1[[3]] + ylim(0, NA))
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
```

![](Multi-response%20(regression)_files/figure-html/bio_1-PD-1.png)

The top plot in the above figure shows partial dependency curves for
SNPs that respond to mean annual temperature. What we mean by “respond”
here is that the prediction surface (the line) deviates across the Y
axis of the PD plots. We measure this deviation by calculating the
standard deviation and use that as a threshold (`sdthresh = 0.01` in
this case, and this will differ by data set). The middle plot is the
average partial dependency curve of all SNPs across an annual
temperature gradient. The individual PDs are shown as grey silhouettes
in the background. This is very similar to the pattern observed by
Fitzpatrick et al., except with a slight decline in SNP turnover with
mean annual temperatures \> 0. Combined, you can see here only a few
candidate SNPs are driving this pattern and these may warrant further
interrogation.

The bottom plot in the figure shows the rate of change over the SNP PD
curves. There is a general rise from -80 to -20. This kind of plot can
help to identify general thresholds.

Let’s compare the PDs to accumulated local effect (ALE) plots that are
less sensitive to correlations among features (see Molnar 2019). We
generate ALE plots by supplying `type = "ale"` to
[`mrCovar()`](https://github.com/nickfountainjones/mrIML/reference/mrCovar.md).

``` r
ALE_bio1 <- mrCovar(
  yhats_rf,
  var = "bio_1",
  sdthresh = 0.01,
  type = "ale"
)

(ALE_bio1[[1]] + ylim(0, 0.4)) /
  (ALE_bio1[[2]] + ylim(0, 0.4)) /
  (ALE_bio1[[3]] + ylim(0, NA))
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
```

![](Multi-response%20(regression)_files/figure-html/bio_1-ale-1.png)

The effect of mean annual temperature on SNP turnover is not as distinct
in the global ALE plot. This may mean that correlations between features
may be important for the predictions.

### Other interpretation methods

`mrIML` has easy-to-use functionality that can quantify interactions
between features. Note that this can take a while to compute and will be
the topic of future work.

This is touching only the surface of what is possible in terms of
interrogating this model. Both Flashlight and IML packages have a wide
variety of tools that can offer novel insights into how these models
perform. See
<https://cran.r-project.org/web/packages/flashlight/vignettes/flashlight.html>
and <https://cran.r-project.org/web/packages/iml/vignettes/intro.html>
for other options.
