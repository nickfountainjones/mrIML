# Principal Component Analysis of mrIML variable importance

Principal Component Analysis of mrIML variable importance

## Usage

``` r
mrVipPCA(mrVip_obj)
```

## Arguments

- mrVip_obj:

  A list returned by
  [`mrVip()`](https://github.com/nickfountainjones/mrIML/reference/mrVip.md).

## Value

A list of PCA results:

- `$PCA_plot`: Side-by-side plots of the different response models on
  the first two principal components (PCs) and a Scree plot.

- `$PC_outliers`: A list of the models flagged as outliers on at least
  one of the PCs.

- `$eigenvalues`: The eigenvalues associated with the principal
  components.

- `$PC_scores`: The PC scores of each response model.

## Examples

``` r
# Without bootstrap
mrIML_rf <- mrIML::mrIML_bird_parasites_RF

mrIML_rf_vip <- mrVip(mrIML_rf, taxa = "Plas")

vipPCA_results <- mrIML_rf_vip %>%
 mrVipPCA()
```
