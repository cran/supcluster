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
        sig=1,gamma=1,beta=c(-5,-2.5,0,2.5,5),outcomeModel=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ratio}{The ratio \eqn{\tau^2/\sigma^2} of the variance of the\ random effects to the error variance of the features}
%%     ~~Describe \code{cl1} here~~

  \item{npats}{Number of observations in the data set.
%%     ~~Describe \code{cl2} here~~
}
\item{clusts}{The cluster identity of the features}
\item{sig}{The error variance of the features.}
\item{gamma}{The error variance of the outcome.}
\item{beta}{The value of the regression coefficients}
\item{outcomeModel}{A function that returns a data frame with \code{npats} observations and rows that depend on the data object chosen.  Two outcomeModel programs are provided, \code{\link{binaryOutcome}} and \code{\link{survivalOutcome}}, however users can write their own outcome model.
If NULL no data object is returned}
}
\value{A list with one element if outcomeModel=NULL which is a data frame which is \code{npats} times \code{ngens+1} the last column is the outcome. Otherwise a list of two data frames, one being the feature data and the other being the outcome data according to what outcomeModel is used.
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
