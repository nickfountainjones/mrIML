# mrIML 2.2.0

Initial bug fixes after major release of mrIML 2 to CRAN.

- `mrIMLpredicts()` properly handles missing `X` or `X1`.
- `mrCovar()` and `mrVip()` can handle factors without throwing a `cbind` error.
- Stratified test-train splits so that there is always positive and negative observation in the test set for small data sets.
- Improved internal behaviour of `predict()` from `workflow` objects.

# mrIML 2.1.0

mrIML 1.0.1
=======================================
* Initial release to github
* Initial functions and vignettes

mrIML 2.0.1
=======================================
* New functionality to allow MrIML to perform joint species distribution models (JSDMs)
* New bootstrap functions and vignette 
* Can also harness deep neural network models
