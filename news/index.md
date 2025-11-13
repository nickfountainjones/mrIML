# Changelog

## mrIML 2.2.0

Initial bug fixes after major release of mrIML 2 to CRAN.

- [`mrIMLpredicts()`](https://github.com/nickfountainjones/mrIML/reference/mrIMLpredicts.md)
  properly handles missing `X` or `X1`.
- [`mrCovar()`](https://github.com/nickfountainjones/mrIML/reference/mrCovar.md)
  and
  [`mrVip()`](https://github.com/nickfountainjones/mrIML/reference/mrVip.md)
  can handle factors without throwing a `cbind` error.
- Stratified test-train splits so that there is always positive and
  negative observation in the test set for small data sets.
- Improved internal behaviour of
  [`predict()`](https://rdrr.io/r/stats/predict.html) from `workflow`
  objects.

## mrIML 2.1.0

CRAN release: 2025-07-28

## mrIML 1.0.1

- Initial release to github
- Initial functions and vignettes

## mrIML 2.0.1

- New functionality to allow MrIML to perform joint species distribution
  models (JSDMs)
- New bootstrap functions and vignette
- Can also harness deep neural network models
