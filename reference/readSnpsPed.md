# Conversion to single column per locus from plink file via LEA functionality

Conversion to single column per locus from plink file via LEA
functionality

## Usage

``` r
readSnpsPed(pedfile, mapfile)
```

## Arguments

- pedfile:

  A file location.

- mapfile:

  A file location.

## Value

A tibble.

## Details

Function to import SNP data from a plink format into a format suitable
for MrIML predicts (presence/absence of an alelle for each locus).
Currently if there is missing data (NAs) it either imputes them as the
mode or leaves them. A histogram is also produced of the missing data.

## Examples

``` r
if (FALSE) {
snps <- readSnpsPed("FILE_NAME.plink.ped", "FILE_NAME.plink.map.map")
X <- filterRareCommon(snps, lower = 0.4, higher = 0.7)
}
```
