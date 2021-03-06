% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeSpwnPaths.R
\name{writeSpwnPaths}
\alias{writeSpwnPaths}
\title{Spawning Paths}
\usage{
writeSpwnPaths(valid_obs, valid_paths, node_order)
}
\arguments{
\item{valid_obs}{dataframe built by the function \code{assignNodes}.}

\item{valid_paths}{dataframe built by the function \code{getValidPaths}.}

\item{node_order}{dataframe built by the function \code{createNodeOrder}.}
}
\description{
The function builds a data frame of all the observed fish paths in the
observation file based on the parent child table. It works by checking node observations
versus the previous node observation to see if the previous node was a parent or grandparent
great grand-parent..... The function was originally built by Ryan K. (called spawnerPaths). Kevin S. edited it further
}
\examples{
writeSpwnPaths()
}
\author{
Greg Kliewer, Ryan Kinzer, Kevin See
}
