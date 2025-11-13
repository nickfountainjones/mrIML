# Calculate general performance metrics of a mrIML model

Summarizes the performance of a `mrIML` object created using
[`mrIMLpredicts()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLpredicts.md)
in a way that allows for easy comparison of different models. For
regression models, root mean squared error (RMSE) and R-squared are
reported, while for classification models, area under the ROC curve
(AUC), Matthews correlation coefficient (MCC), positive predictive value
(PPV), specificity, and sensitivity are reported.

## Usage

``` r
mrIMLperformance(mrIMLobj)
```

## Arguments

- mrIMLobj:

  A list object created by
  [`mrIMLpredicts()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLpredicts.md)
  containing multi-response models.

## Value

A list with two slots:

- `$model_performance`: A tibble of commonly used metrics that can be
  used to compare model performance of classification models.
  Performance metrics are based on the test data defined during
  [`mrIMLpredicts()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLpredicts.md).

- `$global_performance_summary`: A global performance metric: the
  average of a performance metric over all response models. MCC is used
  for classification models and RMSE for regression models.

## Examples

``` r
mrIML_rf <- mrIML::mrIML_bird_parasites_RF

perf <- mrIMLperformance(mrIML_rf )
perf[[1]]
#> # A tibble: 4 × 8
#>   response     model_name roc_AUC   mcc sensitivity   ppv specificity prevalence
#>   <chr>        <chr>        <dbl> <dbl>       <dbl> <dbl>       <dbl>      <dbl>
#> 1 Hzosteropis  rand_fore…   0.863 0.520       0.913 0.880      0.581      0.265 
#> 2 Hkillangoi   rand_fore…   0.763 0.166       0.982 0.855      0.0952     0.116 
#> 3 Plas         rand_fore…   0.927 0.640       0.951 0.890      0.636      0.196 
#> 4 Microfilaria rand_fore…   0.761 0.376       1     0.917      0.154      0.0980
perf[[2]]
#> [1] 0.4253448
```
