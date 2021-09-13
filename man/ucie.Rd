\name{data2cielab}
\alias{data2cielab}
\title{Mapping 3D Data into CIELab Color Space}
\usage{
data2cielab(dataset, WL = 1, Wa = 1, Wb = 1, S = 1, LAB_coordinates = FALSE)
}
\arguments{
\item{dataset}{3-column dataset to be translated into colors.}

\item{WL}{Weight of L* axis in optimization function. Default value 1.}

\item{Wa}{Weight of a* axis in optimization function. Default value 1.}

\item{Wb}{Weight of b* axis in optimization function. Default value 1.}

\item{S}{Scaling factor for color mapping. Default value 1.}

\item{LAB_coordinates}{Logical. If FALSE, the function returns a data frame with hex colors. If TRUE, the function returns a data frame with the L*a*b* coordinates. Default value FALSE.}
}
\value{
None
}
\description{
'ucie' returns a data frame with the names of the input entities and hex colors (or CIELab coordinates). Data can be mapped to colors for use in data visualization. 'ucie' optimally maps data points into a polygon that represents the CIELab colour space. Since Euclidean distance approximates relative perceptual differences in CIELab color space, the result is a color encoding that aims to capture much of the structure of the original data.
}
\examples{
\donttest{
  df <- data.frame(V1=runif(10,  0,1), V2=runif(10,  0,5), V3=runif(10,  0,30))
  data_with_colors <- data2cielab(df, Wb=1.2, S=1.6)
  data_with_colors <- data2cielab(df, LAB_coordinates = TRUE)
}
}



