\name{tab1}
\alias{tab1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulates Supcluster Function}
\description{Produces summary statistics from a simulation of supcluster}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
\usage{
tab1(ratio=4,reps=100,n=1000,start=500,fbeta=FALSE,
               maxclusters=5,chains=1,clusts=c(15,15,20),
               sig=1,gamma=1,npats=80,beta=seq(-5,5,5),
               plot=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ratio}{ The ratio of tau to sigma}
  \item{reps}{The number of runs}
  \item{n}{The number of MCMC iterations}
  \item{start}{The first MCMC iteration used}
  \item{fbeta}{If TRUE the outcome is not used}
  \item{maxclusters}{The maximum number of clusters for the estimation step}
  \item{chains}{The number of chains to run}
  \item{clusts}{A list of the number of genes in each cluster}
  \item{sig,gamma,beta}{The parameters sigma,gamma,beta}
  \item{npats}{The number of experimental units(patients)}
  \item{plot}{Plots the first run}
  
%%     ~~Describe \code{otpt} here~~
}


\value{A data frame is returned with the mean parameter value, it's standard error and the mean of it's standard error calculated from the MCMC}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...


\author{David A. Schoenfeld, Jessie Hsu
%%  ~~who you are~~
}

\seealso{\code{\link{supcluster}},,\code{\link{compare.chains}},\code{\link{concordmap}}}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
#very few iterations done so that this runs in less than 5 seconds.
#You need to change reps=100,start=2000,n=3000 to get enough iterations
tab1(ratio=2,reps=5,n=10,start=1,maxclusters=5)
}
\keyword{cluster}
\concept{machine learning}
\concept{supervised clustering}

