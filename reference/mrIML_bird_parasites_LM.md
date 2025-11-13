# An example mrIML model fit to [MRFcov::Bird.parasites](https://rdrr.io/pkg/MRFcov/man/Bird.parasites.html)

    data <- MRFcov::Bird.parasites
    Y <- data %>%
      dplyr::select(-scale.prop.zos) %>%
      dplyr::select(order(everything()))
    X <- data %>%
      dplyr::select(scale.prop.zos)

## Usage

``` r
mrIML_bird_parasites_LM
```

## Format

An object of class `list` of length 3.

## Details

model_lm \<- logistic_reg() %\>% set_engine("glm")

mrIML_bird_parasites_LM \<- mrIMLpredicts( X = X, Y = Y, X1 = Y, Model =
model_lm, prop = 0.7, k = 2, racing = FALSE )
