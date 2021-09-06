
# ucie3D

<!-- badges: start -->
<!-- badges: end -->

The goal of ucie3D is to translate 3D data into colors, using CIELab color space.

## Installation

You can install the released version of ucie3D from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ucie3D")
or
devtools::install_github("mikelkou/ucie3D")
```

## Usage

```r
library(ucie3D)

# returns a data frame with names of data and colors
ucie3DTransformation(dataset)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ucie3D)
## basic example code
3D_data_with_colors <- ucie3DTransformation(dataset, WL=1, Wa=1, Wb=1.2, S=1.2)
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)

