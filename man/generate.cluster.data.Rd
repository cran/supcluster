\name{generate.cluster.data}
\alias{generate.cluster.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to Generate Data According to the Supcluster Model
%%  ~~function to do ... ~~
}
\description{
Generates cluster data according to the used for supervised clustering
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generate.cluster.data(ratio,npats=80,clusts=c(12,8,12,12,6),
                sig=1,gamma=1,beta=c(-5,-2.5,0,2.5,5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ratio}{The ratio \eqn{\tau^2/\sigma^2} of the variance of the random effects to the error variance of the features}
%%     ~~Describe \code{cl1} here~~

  \item{npats}{Number of observations in the data set.
%%     ~~Describe \code{cl2} here~~
}
\item{clusts}{The cluster identity of the features}
\item{sig}{The error variance of the features.}
\item{gamma}{The error variance of the outcome.}
\item{beta}{The value of the regression coefficients}
}
\value{A data frame which is \code{npats} times \code{ngens+1} the last column is the outcome.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{David A. Schoenfeld}
%%  ~~who you are~~
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{ \code{\link{supcluster}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
