
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mrIML: multi-response (Multivariate) interpretable machine learning <img src="man/figures/logo.png" align="right" height="120"/></a>

<!-- badges: start -->

![GitHub R package
version](https://img.shields.io/github/r-package/v/nickfountainjones/mrIML?logo=github&logoColor=%2300ff37&style=flat-square)
![GitHub
contributors](https://img.shields.io/github/contributors/nickfountainjones/mrIML?style=flat-square)
![GitHub last
commit](https://img.shields.io/github/last-commit/nickfountainjones/mrIML?style=flat-square)
[![R-CMD-check](https://github.com/nickfountainjones/mrIML/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nickfountainjones/mrIML/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nickfountainjones/mrIML/graph/badge.svg)](https://app.codecov.io/gh/nickfountainjones/mrIML)
<!-- badges: end -->

## Overview

This package aims to enable users to build and interpret multivariate
machine learning models harnessing the tidyverse (tidy model syntax in
particular). This package builds off ideas from Gradient Forests [Ellis
et al
2012](https://esajournals-onlinelibrary-wiley-com.prox.lib.ncsu.edu/doi/full/10.1890/11-0252.1)),
ecological genomic approaches [Fitzpatrick and Keller,
2014](https://onlinelibrary.wiley.com/doi/abs/10.1111/ele.12376) and
multi-response stacking algorithms \[Xing et al 2019\].

This package can be of use for any multi-response machine learning
problem, but was designed to handle data common to community ecology
(site by species data) and ecological genomics (individual or population
by SNP loci).

## How to Install

You can install the development version of `mrIML` using `devtools`:

``` r
devtools::install_github('nickfountainjones/mrIML')
```

## Ursing `mrIML`

To get started, load `mrIML` and `tidymodels`:

``` r
library(mrIML)
library(tidymodels)
#> ── Attaching packages ────────────────────────────────────── tidymodels 1.3.0 ──
#> ✔ broom        1.0.8     ✔ recipes      1.3.0
#> ✔ dials        1.4.0     ✔ rsample      1.3.0
#> ✔ dplyr        1.1.4     ✔ tibble       3.2.1
#> ✔ ggplot2      3.5.2     ✔ tidyr        1.3.1
#> ✔ infer        1.0.8     ✔ tune         1.3.0
#> ✔ modeldata    1.4.0     ✔ workflows    1.2.0
#> ✔ parsnip      1.3.1     ✔ workflowsets 1.1.0
#> ✔ purrr        1.0.4     ✔ yardstick    1.3.2
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ purrr::discard() masks scales::discard()
#> ✖ dplyr::filter()  masks stats::filter()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ recipes::step()  masks stats::step()
```

Many functions in `mrIML` benefit from parallel processing.

``` r
future::plan("multisession", workers = 2)
```

The core function of `mrIML` is `mrIMLpredicts()`, which is a wrapper
around the tidymodels workflow that fits a provided model to each
response variable in a multi-response data set.

``` r
# Load example multi-response data
data <- MRFcov::Bird.parasites
# Split into response and predictor data
Y <- data %>%
  select(-c("scale.prop.zos"))
X <- data %>%
  select(scale.prop.zos)

# Define tidymodel
model <- rand_forest(
  trees = 100,
  mode = "classification",
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("randomForest")

# Fit multi-response model
mrIML_model <- mrIMLpredicts(
  X = X,
  Y = Y,
  Model = model,
  prop = 0.7,
  k = 5
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
```

The object `mrIML_model` can be investigated using:

- `mrIMLperformance()` to get performance metrics for each response
  variable,
- `mrvip()` to get variable importance for each response variable,
- `mrFlashlight()` to get partial dependence plots for each response
  variable,
- `mrCovar()` to get covariate importance for each predictor variable,
  and
- `mrInteractions()` to get interaction importance for each predictor
  variable in the response models.

Two multi-response models can be compared using `mrPerformance()`.

Bootstrapping can be implemented using `mrBootstrap()`, which can then
be used quantify uncertainty around partial dependence plots,
`mrPdPlotBootstrap()`, and variable importance, `mrvipBootstrap()`, as
well as build co-occurrence networks using `mrCoOccurNet()`.

## Recent mrIML publications

1.  Fountain-Jones, N. M., Kozakiewicz, C. P., Forester, B. R.,
    Landguth, E. L., Carver, S., Charleston, M., Gagne, R. B.,
    Greenwell, B., Kraberger, S., Trumbo, D. R., Mayer, M., Clark, N.
    J., & Machado, G. (2021). MrIML: Multi-response interpretable
    machine learning to model genomic landscapes. Molecular Ecology
    Resources, 21, 2766– 2781. <https://doi.org/10.1111/1755-0998.13495>

2.  Sykes, A. L., Silva, G. S., Holtkamp, D. J., Mauch, B. W., Osemeke,
    O., Linhares, D. C.L., & Machado, G. (2021). Interpretable machine
    learning applied to on-farm biosecurity and porcine reproductive and
    respiratory syndrome virus. Transboundary and Emerging Diseases, 00,
    1– 15. <https://doi.org/10.1111/tbed.14369>

## References

Xing, L, Lesperance, ML and Zhang, X (2020). Simultaneous prediction of
multiple outcomes using revised stacking algorithms. Bioinformatics, 36,
65-72. <doi:10.1093/bioinformatics/btz531>.

Fitzpatrick, M.C. & Keller, S.R. (2015) Ecological genomics meets
community-level modelling of biodiversity: mapping the genomic landscape
of current and future environmental adaptation. Ecology Letters 18,
1–16.doi.org/10.1111/ele.12376

Ellis, N., Smith, S.J. and Pitcher, C.R. (2012), Gradient forests:
calculating importance gradients on physical predictors. Ecology, 93:
156-168. <doi:10.1890/11-0252.1>
