# Filter rare response variables from the data

Filter rare response variables from the data

## Usage

``` r
filterRareCommon(X, lower = lower, higher = higher)
```

## Arguments

- X:

  is a data.frame with rows as sites or individuals or populations and
  columns as loci or species OTUs.

- lower:

  is the lower threshold value in which response varialkes are removed
  from the data.frame.

- higher:

  is the upper threshold value in which response varialkes are removed
  from the data.frame.

## Value

A filtered tibble.

## Details

This function allows you to remove response units (OTUs or SNPs or
species) from your response data as a preprocessing step. Suitable when
the response is a binary outcome.

## Examples

``` r
if (FALSE) {
X <- filterRareCommon(Responsedata, lower = 0.4, higher = 0.7)
}
```
