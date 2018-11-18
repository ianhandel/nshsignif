# nshsignif

Tools to produce significance table from study summaries.

## Installation

You can install the released version of nshsignif from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ianhandel/nshsignif")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
library(nshsignif)
parse_pvalue(c("P = 0.05", "P < 0.05"), alpha = 0.05)
```

