# Investigate partial dependencies of a covariate for mrIML JSDMs (Joint Species Distribution Models)

This function is a wrapper around
[`mrFlashlight()`](https://github.com/nickfountainjones/mrIML/reference/mrFlashlight.md)
that plots the covariate partial dependencies for a specified
environmental/host variable. It also filters the taxa based on standard
deviation thresholds.

## Usage

``` r
mrCovar(mrIMLobj, var, sdthresh = 0.05, ...)
```

## Arguments

- mrIMLobj:

  A list object output by
  [`mrIMLpredicts()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLpredicts.md).

- var:

  The variable of interest for calculating the profile.

- sdthresh:

  The standard deviation threshold for filtering taxa (default: 0.05).

- ...:

  Arguments passed to
  [`flashlight::light_profile()`](https://rdrr.io/pkg/flashlight/man/light_profile.html)

## Value

A list of figures:

- `$partial_dep_curves`: The covariate partial dependence profiles for
  those models that meet the `sdthresh` requirement.

- `$partial_dep_avg`: The average partial dependence profile for all
  models. All individual model partial dependence profiles are
  silhouetted in the background.

- `$partial_dep_diff`: The distribution of the rates of change in
  probability for the specified variable (the derivatives of the PD
  curves). Useful to identify key threshold values in the variable.

## Examples

``` r
mrIML_rf <-  mrIML::mrIML_bird_parasites_RF

covar_results <- mrIML_rf %>%
  mrCovar(var = "scale.prop.zos", sdthresh = 0.05)
```
