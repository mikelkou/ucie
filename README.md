
# ucie

<!-- badges: start -->
[![R-CMD-check](https://github.com/mikelkou/ucie/workflows/R-CMD-check/badge.svg)](https://github.com/mikelkou/ucie/actions)
[![Travis build status](https://travis-ci.com/mikelkou/ucie.svg?branch=master)](https://travis-ci.com/mikelkou/ucie)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mikelkou/ucie?branch=master&svg=true)](https://ci.appveyor.com/project/mikelkou/ucie)
[![CircleCI build status](https://circleci.com/gh/mikelkou/ucie.svg?style=svg)](https://circleci.com/gh/mikelkou/ucie)
<!-- badges: end -->

The goal of ucie is to map data into CIELab Color Space.

## Installation

You can install the released version of ucie from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ucie")
or
devtools::install_github("mikelkou/ucie")
```

## Usage

```r
library(ucie)

# returns a data frame with names of data points and colors (hex colors or Lab coordinates)
data2cielab(dataset, WL, Wa, Wb, S, LAB_coordinates)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ucie)
## basic example code
df <- data.frame(V1=runif(100,  0,1), V2=runif(100,  0,5), V3=runif(100,  0,30))
data_with_colors <- data2cielab(df, Wb=1.2, S=1.6)
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)

