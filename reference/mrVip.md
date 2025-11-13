# Calculates and helps interpret variable importance for `mrIML` models.

Summarizes variable importance in a `mrIML` model at both a global
(across all the response models) and local (for individual response
models) level. This can be done for a plain `mrIML` model or bootstrap
results obtained from
[`mrBootstrap()`](https://github.com/nickfountainjones/mrIML/reference/mrBootstrap.md).

## Usage

``` r
mrVip(
  mrIMLobj,
  mrBootstrap_obj = NULL,
  threshold = 0.1,
  global_top_var = 10,
  local_top_var = 5,
  taxa = NULL,
  model_perf = NULL
)
```

## Arguments

- mrIMLobj:

  A list object output by
  [`mrIMLpredicts()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLpredicts.md).

- mrBootstrap_obj:

  A list of bootstrap results output by
  [`mrBootstrap()`](https://github.com/nickfountainjones/mrIML/reference/mrBootstrap.md).

- threshold:

  The performance threshold for response models (AUC for classification
  and R2 for regression). Only response models that meet this
  performance criterion are plotted.

- global_top_var:

  The number of top global variables to display (default: 10).

- local_top_var:

  The number of top local variables for each response to display
  (default: 5).

- taxa:

  A character string identifying which response model should be plotted.

- model_perf:

  A list object containing model performance metrics output by
  [`mrIMLperformance()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLperformance.md).
  If not supplied, then
  [`mrIMLperformance()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLperformance.md)
  is run inside `mrvip()` to get performance metrics.

## Value

A list containing:

- `$vi_data`: Variable importance data in its raw form (including
  bootstrap samples if `mrBootstrap_obj` was supplied).

- `$vi_tbl`: Variable importance data point estimates.

- `$vi_plot`: A grouped plot of the most important variables both
  globally and for the individual response models.

## Examples

``` r
# Without bootstrap
mrIML_rf <- mrIML::mrIML_bird_parasites_RF

vip_results <-mrVip(mrIML_rf, taxa = "Plas")

# With bootstrap
# \donttest{
mrIML_rf_boot <- mrIML_rf %>%
  mrBootstrap(num_bootstrap = 5)
#>   |                                                                              |                                                                      |   0%  |                                                                              |====                                                                  |   5%  |                                                                              |=======                                                               |  10%  |                                                                              |==========                                                            |  15%  |                                                                              |==============                                                        |  20%  |                                                                              |==================                                                    |  25%  |                                                                              |=====================                                                 |  30%  |                                                                              |========================                                              |  35%  |                                                                              |============================                                          |  40%  |                                                                              |================================                                      |  45%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================                                |  55%  |                                                                              |==========================================                            |  60%  |                                                                              |==============================================                        |  65%  |                                                                              |=================================================                     |  70%  |                                                                              |====================================================                  |  75%  |                                                                              |========================================================              |  80%  |                                                                              |============================================================          |  85%  |                                                                              |===============================================================       |  90%  |                                                                              |==================================================================    |  95%  |                                                                              |======================================================================| 100%
  
mrIML_rf_vip <- mrVip(
  mrIML_rf,
  mrBootstrap_obj = mrIML_rf_boot
)
# }
```
