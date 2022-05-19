\name{beta.by.gene}
\alias{beta.by.gene}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Utility to Associate the Value of \eqn{\beta} with the Feature it is Assocated With}
\description{
The model associates the coefficients of the random effects with the cluster number. However the cluster numbers are not unique. This utility associates the coefficient with gene that is in the  cluster, for each cluster number.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{beta.by.gene(supcluster.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{supcluster.list}{ The output of \code{supcluster}
%%     ~~Describe \code{otpt} here~~
}
}

\value{A matrix is returned with dimensions, the number of MCMC iterations by the number of genes/features +1. The first column is the chain number and the remain columns are the beta value for each of the gene/features}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

\references{ Added latter
%% ~put references to the literature/web site here ~
}
\author{David A. Schoenfeld, Jessie Hsu
%%  ~~who you are~~
}
\seealso{\code{\link{supcluster}},,\code{\link{compare.chains}},\code{\link{concordmap}}}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
dat=generate.cluster.data(1)[[1]]
us=supcluster(dat,outcome="outcome",features=1:50,maxclusters=6,nstart=20,n=40)
vs=beta.by.gene(us)
colMeans(vs[,2:7])
}
\keyword{ supervised clustering }

