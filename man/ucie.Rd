\name{data2cielab}
\alias{data2cielab}
\title{Mapping 3D Data into CIELab Color Space}
\usage{
data2cielab(dataset, WL = 1, Wa = 1, Wb = 1, S = 1)
}
\arguments{
\item{dataset}{3-column dataset to be translated into colors.}

\item{WL}{Weight of Lstar axis in optimization function. Default value 1.}

\item{Wa}{Weight of astar axis in optimization function. Default value 1.}

\item{Wb}{Weight of bstar axis in optimization function. Default value 1.}

\item{S}{Scaling factor for color mapping. Default value 1.}
}
\value{
None
}
\description{
3D data can be mapped to colors for use in data visualization.
}
