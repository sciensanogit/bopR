
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bopR

<!-- badges: start -->

[![R-CMD-check](https://github.com/sciensanogit/bopR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sciensanogit/bopR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

bopR is the Belgian open population repository for R

## Installation

You can install the development version of bopR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sciensanogit/bopR")
```

## Population datasets

the `bepop` package contains population files on different geographical
levels (municipalities - districts - provinces - regions - country). The
datasets are named as follows:

-   *BE_POP_MUNTY*: municipalities
-   *BE_POP_ARRD*: districts
-   *BE_POP_PROV*: provinces (+ Brussels Capital Region for simplicity)
-   *BE_POP_RGN*: regions
-   *BE_POP*: country

for more information on the content of each dataset, you can always
consult the help-file by `?BE_POP`. The population structure of Belgium
over the years was as follows:

## Population projection datasets

the `bepop` package also contains population projection files on
different geographical levels (districts - provinces - regions -
country). The datasets are named as follows:

## Data sources

The idea for this package is inspired by the [BelgiumMaps.StatBel
package](https://github.com/bnosac/BelgiumMaps.StatBel), developed by
Jan Wijffels at
[BNOSAC](http://www.bnosac.be/index.php/blog/55-belgiummaps-statbel-r-package-with-administrative-boundaries-of-belgium).

The population files are obtained from the Statbel open data and the
open data from the federal plan bureau (FPB) of Belgium:

-   **<https://statbel.fgov.be/en/themes/population>** includes
    information on the population of Belgium on the 1st of January for
    each year.
-   **<https://www.plan.be/publications/publication-2255-nl-demografische_vooruitzichten_2021_2070_update_sterke_opwaartse_herziening_van_de_bevolkingsgroei_in_2022>**
    includes information on the population projections of Belgium.

To calculate the mid-year population of year `x`, one should average the
population of year `x` and year `x+1` .
