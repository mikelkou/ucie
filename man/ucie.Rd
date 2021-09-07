\name{data2cielab}
\alias{data2cielab}
\title{Mapping 3D Data into CIELab Color Space}
\usage{
data2cielab(dataset, WL = 1, Wa = 1, Wb = 1, S = 1, LAB_coordinates = F)
}
\arguments{
\item{dataset}{3-column dataset to be translated into colors.}

\item{WL}{Weight of L* axis in optimization function. Default value 1.}

\item{Wa}{Weight of a* axis in optimization function. Default value 1.}

\item{Wb}{Weight of b* axis in optimization function. Default value 1.}

\item{S}{Scaling factor for color mapping. Default value 1.}

\item{LAB_coordinates}{Logical. If False, the function returns a data frame with hex colors. If True, the function returns a data frame with the L*a*b* coordinates. Default value F.}
}
\value{
None
}
\description{
3D data can be mapped to colors for use in data visualization.
}
