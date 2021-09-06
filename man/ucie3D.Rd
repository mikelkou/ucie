\name{ucie3DTransformation}
\alias{ucie3DTransformation}
\title{A translation of 3D data into colors, using CIELab color space.}
\usage{
ucie3DTransformation(dataset, WL = 1, Wa = 1, Wb = 1, S = 1)
}
\arguments{
\item{dataset}{3-column dataset that needs to be translated into colors.}

\item{WL}{Weight that shows how shifted towards Lstar axis.}

\item{Wa}{Weight that shows how shifted towards bstar axis.}

\item{Wb}{Weight that shows how shifted towards astar axis.}

\item{S}{Scaling factor.}
}
\value{
None
}
\description{
Translate the 3D data into colors of CIELab color space. CIELab was intended as a perceptually uniform space, where a given numerical change corresponds to similar perceived change in color.
}
