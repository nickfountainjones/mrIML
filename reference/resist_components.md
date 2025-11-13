# Calculates resistance components from a list of pairwise resistance surfaces

Calculates resistance components from a list of pairwise resistance
surfaces

## Usage

``` r
resist_components(foldername = foldername, p_val = p_val, cl = NULL)
```

## Arguments

- foldername:

  A `character` this is the location where the resistance surfaces are
  stored.

- p_val:

  A `numeric` this sets the significance threshold for axes in
  explaining variance in the original resistance matrix based on
  redundancy analysis. In effect this filters out axes that don't
  explain variance.

- cl:

  A parallel argument to be passed to
  [`vegan::capscale()`](https://vegandevs.github.io/vegan/reference/dbrda.html)
  if parallel compute is wanted.

## Value

A data frame.

## Details

Outputs a data frame of significant resistance components for each
matrix in the target folder. These data can be combined with
non-pairwise matrix data.

## Examples

``` r
if (FALSE) {
Y <- resist_components(filename = 'FILE_PATH', p_val = 0.01)
}
```
