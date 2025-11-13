# Convert mrIML object into a flashlight object

A wrapper function around
[`flashlight::flashlight()`](https://rdrr.io/pkg/flashlight/man/flashlight.html)
to run multi-response model-agnostic interpretable machine learning
analyses. The output can be interrogated using the core functionality of
flashlight: see
[`vignette("flashlight", package = "flashlight")`](https://cran.rstudio.com/web/packages/flashlight/vignettes/flashlight.html).

## Usage

``` r
mrFlashlight(mrIMLobj, response = "multi", index = 1, predict_function = NULL)
```

## Arguments

- mrIMLobj:

  A list object output by
  [`mrIMLpredicts()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLpredicts.md).

- response:

  A character string indicating the type of response: `"single"` selects
  one response (indicated by `index`) and `"multi"` selects all
  responses.

- index:

  A numeric value used when `response` is `"single"` to select which
  response column in the data to create a flashlight object for.

- predict_function:

  A function specifying a user-defined prediction function (optional).

## Value

A flashlight or multi-flashlight object.

## Examples

``` r
library(flashlight)
library(ggplot2)

mrIML_rf <- mrIML::mrIML_bird_parasites_RF

fl <- mrFlashlight(
  mrIML_rf,
  response = "multi",
  index = 1
)

# Performance comparison
fl %>%
  light_performance(
    metrics = list(`ROC AUC` = MetricsWeighted::AUC)
  ) %>%
  plot() +
  ylim(0, 1)


# Partial dependence curves
fl %>%
  light_profile(data = cbind(mrIML_rf$Data$X, mrIML_rf$Data$Y), "scale.prop.zos") %>%
  plot()


# Two-way partial dependence
fl %>%
  light_profile2d(c("scale.prop.zos", "Plas")) %>%
  plot()
```
