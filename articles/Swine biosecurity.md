# Swine biosecurity

MrIML is a R package allows users to generate and interpret
multi-response models (i.e., joint species distribution models)
leveraging advances in data science and machine learning. MrIML couples
the tidymodel infrastructure developed by Max Kuhn and colleagues with
model agnostic interpretable machine learning tools to gain insights
into multiple response data such as. As such MrIML is flexible and
easily extendable allowing users to construct everything from simple
linear models to tree-based methods for each response using the same
syntax and same way to compare predictive performance.In this vignette
we will guide you through how to apply the MrIML framework to identify
and benchmark biosecurity practices in veterinary disease.

This working example is built upon a classification framework. More
information about this can be found in the “Classification working
example”. In this case we are predicting one response.

### Lets load that data

This is a synthesized dataset to simulate PRRSV infection, biosecurity
practices and farm demographics on swine farms across the united states.
Please note that the data included in this package is simulated, and is
not a reflection of any real farm, company or state.

``` r
set.seed(130) #set the seed to ensure consistency
load("biosecurity_data.RData")
data <- biosecurity_vignette_data
```

### Defining your model engine

This is a random forest model, as used in the classification example.
However there are more models available within MrIML and can be found
[here](https://www.tidymodels.org/find/).

``` r
#split predictor variables and outcome
Y <- data %>%
  select(Class)
X <- data %>%
  select(-c(1, 44, 45))
model1 <- rand_forest(trees = 1000,
              mtry = tune(),
              min_n = tune(),
              mode = "classification") %>% 
  set_engine("randomForest")
```

### Parallel processing

MrIML provides uses the flexible future apply functionality to set up
multi-core processing. In the example below, we set up a cluster using 4
cores. If you don’t set up a cluster, the default settings will be used
and the analysis will run sequentially.

``` r
future::plan("multisession", workers = 2)
```

### Running the analysis

Now we can train and test our model.

``` r
yhats <- mrIMLpredicts(X=X, #features/predictors 
                       Y=Y, #response data
                       Model=model1, #specify your model
                       racing=T) #turn off racing autotuning for now
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
```

We can then assess our model performance using a number of performance
metrics including area under the curve (AUC), sensitivity, specificity
and Matthews correlation coefficient (MCC).

``` r
ModelPerf <- mrIMLperformance(yhats)

ModelPerf[[1]] #predictive performance for individual responses 
#> # A tibble: 1 × 8
#>   response model_name  roc_AUC    mcc sensitivity   ppv specificity prevalence
#>   <chr>    <chr>         <dbl>  <dbl>       <dbl> <dbl>       <dbl>      <dbl>
#> 1 Class    rand_forest    0.57 0.0667       0.533 0.533       0.533        0.5
ModelPerf[[2]]#overall predictive performance. r2 for regression and MCC for classification
#> [1] 0.06666667
```

### Benchmarking: global importance

Here we can look at the variable importance. This is the dependence
between our outcome and biosecurity variables.

``` r
#calculate variable importance
#still got a bug here to do with the PCA
impVI <- mrVip(
  yhats,
  threshold = 0.1,
  global_top_var = 10,
  local_top_var = 5,
  taxa = NULL,
  model_perf = ModelPerf
)

impVI[[3]] #importance
```

We can explore further and assess the partial dependence of each
variable. Here we isolate the dependence of one variable and visualize
how this dependence changes over different observed values.

``` r
#create a flashlight object
fl <- mrFlashlight(yhats,
                   response="single", 
                   index=1) 
#plot partial dependence profiles
plot(light_profile(fl, v = "Premises_in_3_miles")) +
  theme_bw()
```

![](../../../../../../../tmp/RtmpPDWpTF/articles/reference/figures/pdp-1.png)
\### Benchmarking: predicted risk

The following function allows you to visualize and compare predicted
risk among and within groups

``` r
#apply the trained model to the entire data set to provide risk of predicted outbreak
fit_bio <- extract_workflow(yhats$Fits[[1]]$last_mod_fit)

pred_bio <- fit_bio %>%
  predict(
    new_data = yhats$Fits[[1]]$data,
    type = "prob"
  )

#...
```

``` r
#plot within group comparison of predicted risk
mrBenchmark(data = "data1",
            Y = "Class",
            pred = "Predicted",
            group = "Group",
            label_by = "ID",
            type = "internal") 
```

### Benchmarking: local importance

To investigate and interpret the contribution of variables at an
individual level, we must use a local explanation method. Here we
implement a local breakDown explainer. `mrLocalExplainer` produces both
aggregated and individual results of variable contribution. Variables
with a phi \> 0 contribute to an increase in predicted PRRSV outbreak
risk, while variables with a phi \< 0 contribute to a decrease in
predicted outbreak risk.

``` r
#Use this function to implement the local explainer 
data<-data%>%
  mutate(Class = revalue(Class,
                c("1" = "Positive", "0" = "Negative"))) 
data$Class <- relevel(data$Class, "Positive")
X1 <- X
mrLocalExplainer(X = X1,
                 Model = yhats,
                 Y = data$Class)
```

Individual plots are produced in the list object `LE_indiv_plots`. Each
plot corresponds to a single farm. An example of a positive farm and a
negative farm are shown here.

``` r
LE_indiv_plots[1]
LE_indiv_plots[2]
```

At the positive farm we can see that yards from the public road and
garbage collection are contributing to an increased outbreak risk, while
manure removals and repair visits are contributing to a decreased
outbreak risk. Likewise, at the negative farm yards to the public road
and and the downtime required for manure removal personnel are
contributing to an increased outbreak risk, while feed deliveries and
manure removals are contributing to a decreased outbreak risk.

This session of the MrIML has been funded by Critical Agricultural
Research and Extension 2019-68008-29910 from the USDA National Institute
of Food and Agriculture.
