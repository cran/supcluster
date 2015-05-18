\name{supcluster-package}
\alias{supcluster-package}
\docType{package}
\title{
Supervised Cluster Anaysis 
}
\description{
The function clusters features under the assumption that each cluster has a
random effect and there is an outcome variable that is related to the random effects
by a linear regression. In this way the cluster analysis is ``supervised'' by the outcome
variable. An alternate specification is that features in each cluster have the same compound
symetric normal distribution, and the conditional distribution of the outcome given the features has the same coefficient for each feature in a cluster. 
}
\details{
\tabular{ll}{
Package: \tab supcluster\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-24\cr
License: \tab GPL-2\cr
}
The package consists of a function \code{\link{supcluster}} which reads a data frame whose columns include features and an outcome. It then peforms a cluster analysis that is supervised by the outcome as described above. The cluster analysis is performed using a Markoff Chain Monte Carlo algorythm, the output is a matrix where each row is a parameter vector consisting of the parameters of the multivariate normal distribution described above as well as the cluster membership of each of the features.

In addition there is function \code{\link{concordmap}} which produces a array with the posterior probability that each pair of features are in the same cluster and a function \code{\link{compare.chains}} used to compare these arrays for two chains in order to determine whether different chains have converged to the same set of clusters.
}
\author{ David A. Schoenfeld, Jesse Hsu
Maintainer: David A. Schoenfeld <dschoenfeld@mgh.harvard.edu>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }

\seealso{\code{\link{supcluster}},
\code{\link{concordmap}},
\code{\link{compare.chains}},\code{\link{beta.by.gene}}}
