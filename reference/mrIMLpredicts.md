# Generates a multi-response predictive model

This function fits separate classification/regression models, specified
in the tidymodels framework, for each response variable in a data set.
This is the core function of `mrIML`.

## Usage

``` r
mrIMLpredicts(
  Model,
  Y,
  X = tibble::tibble(),
  X1 = tibble::tibble(),
  balance_data = "no",
  dummy = FALSE,
  prop = 0.7,
  tune_grid_size = 10,
  k = 10,
  racing = TRUE
)
```

## Arguments

- Model:

  Any model from the tidymodels package. See **Examples**.

- Y, X, X1:

  Data frames containing the response, predictor, and the joint response
  variables (i.e. the responses that are also to be used as predictors
  if fitting GN model) respectively. If `X1` is not provided then a
  standard multi-response model will be fit to the data (e.g. the
  response models are independant of one another conditional on the
  predictors supplied in X). See **Details** section below.

- balance_data:

  A character string:

  - "up": up-samples the data to equal class sizes.

  - "down": down-samples the data to equal class sizes.

  - "no": leaves the data as is. "no" is the default value.

- dummy:

  A logical value indicating if
  [`recipes::step_dummy()`](https://recipes.tidymodels.org/reference/step_dummy.html)
  should be included in the data recipe.

- prop:

  A numeric value between 0 and 1. Defines the training-testing data
  proportion to be used, which defaults to `prop = 0.7`.

- tune_grid_size:

  A numeric value that sets the grid size for hyperparameter tuning.
  Larger grid sizes increase computational time. Ignored if
  `racing = TRUE`.

- k:

  A numeric value. Sets the number of folds in the cross-validation.
  10-fold CV is the default.

- racing:

  A logical value. If `TRUE`, `mrIML` performs the grid search using the
  [`finetune::tune_race_anova()`](https://finetune.tidymodels.org/reference/tune_race_anova.html)
  method; otherwise,
  [`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
  is used. `racing = TRUE` is now the default method of tuning.

## Value

A list object with three slots:

- `$Model`: The tidymodels object that was fit.

- `$Data`: A list of the raw data.

- `$Fits`: A list of the fitted models for each response variable.

## Details

`mrIMLpredicts` fits the supplied tidy model to each response variable
in the data frame `Y`. If only `X` (a data frame of predictors) is
supplied, then independent models are fit, i.e., the other response
variables are not used as predictors. If `X1` (a data frame of all or
select response variables) is supplied, then those response variables
are also used as predictors in the response models. For example,
supplying `X1` means that a co-occurrence model is fit.

If `balance_data = "up"`, then
[`themis::step_rose()`](https://themis.tidymodels.org/reference/step_rose.html)
is used to upsample the dataset; however, we generally recommend using
`balance_data = "no"` in most cases.

## Examples

``` r
data <- MRFcov::Bird.parasites

# Define the response variables of interest
Y <- data %>%
  dplyr::select(-scale.prop.zos) %>%
  dplyr::select(order(everything()))

# Define the predictors
X <- data %>%
  dplyr::select(scale.prop.zos)

# Specify a random forest tidy model
model_lm <- parsnip::logistic_reg()

# Fitting independent multi-response model -----------------------------------
MR_model <- mrIMLpredicts(
  X = X,
  Y = Y,
  Model = model_lm,
  prop = 0.7,
  k = 5,
  racing = FALSE
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> Warning: No tuning parameters have been detected, performance will be evaluated using
#> the resamples with no tuning.
#> Did you want to assign any parameters with a value of `tune()`?
#> Warning: No tuning parameters have been detected, performance will be evaluated using
#> the resamples with no tuning.
#> Did you want to assign any parameters with a value of `tune()`?
#> Warning: No tuning parameters have been detected, performance will be evaluated using
#> the resamples with no tuning.
#> Did you want to assign any parameters with a value of `tune()`?
#> Warning: No tuning parameters have been detected, performance will be evaluated using
#> the resamples with no tuning.
#> Did you want to assign any parameters with a value of `tune()`?

# Fitting a graphical network model -----------------------------------------
# Define the dependent response variables (all in this case)
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
X1 <- Y

GN_model <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = X1,
  Model = model_lm,
  prop = 0.7,
  k = 5,
  racing = FALSE
)
}
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> Warning: No tuning parameters have been detected, performance will be evaluated using
#> the resamples with no tuning.
#> Did you want to assign any parameters with a value of `tune()`?
#> Warning: No tuning parameters have been detected, performance will be evaluated using
#> the resamples with no tuning.
#> Did you want to assign any parameters with a value of `tune()`?
#> → A | warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> There were issues with some computations   A: x1
#> There were issues with some computations   A: x1
#> 
#> Warning: No tuning parameters have been detected, performance will be evaluated using
#> the resamples with no tuning.
#> Did you want to assign any parameters with a value of `tune()`?
#> Warning: No tuning parameters have been detected, performance will be evaluated using
#> the resamples with no tuning.
#> Did you want to assign any parameters with a value of `tune()`?
```
