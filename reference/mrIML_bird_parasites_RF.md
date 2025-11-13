# An example mrIML model fit to [MRFcov::Bird.parasites](https://rdrr.io/pkg/MRFcov/man/Bird.parasites.html)

    data <- MRFcov::Bird.parasites
    Y <- data %>%
      dplyr::select(-scale.prop.zos) %>%
      dplyr::select(order(everything()))
    X <- data %>%
      dplyr::select(scale.prop.zos)

## Usage

``` r
mrIML_bird_parasites_RF
```

## Format

An object of class `list` of length 3.

## Details

model_rf \<- parsnip::rand_forest( trees = 10, \# 10 trees are set for
brevity. Aim to start with 1000 mode = "classification", mtry =
tune::tune(), min_n = tune::tune(), engine = "randomForest" )

mrIML_bird_parasites \<- mrIMLpredicts( X = X, Y = Y, X1 = Y, Model =
model_rf, prop = 0.7, k = 2, racing = FALSE )
