![statfunctions](graphs.png)

# qacr

<!-- badges: start -->
<!-- badges: end -->

The goal of qacr is to provide convenient statistical results for data science students
and practitioners.

## Installation

You can install this package with the following code:



``` r
if(!require(devtools)){
   install.packages("devtools")
}
devtools::install_github("rkabacoff/qacr")
```

## Example

This is a basic example which shows you how to solve a common problem- tabulating
the levels of categorical variable.

``` r
library(qacr)
tab(mtcars, cyl)
```

