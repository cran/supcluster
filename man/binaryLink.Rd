\name{binaryLink}
\alias{binaryLink}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Used with \code{\link{supcluster}} when the outcome data object is binary
%%  ~~function to do ... ~~
}
\description{ Calculates the log-likelihood for a logistic model with log-odds \eqn{\mu+x} where \eqn{x} is a frailty
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binaryLink(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A vector of binary data with values of 0 and 1 or \code{TRUE}, \code{FALSE}
%%     ~~Describe \code{x} here~~
}
}
\value{ A function that given a vector of frialties followed by a value of \eqn{\mu} calculates the log-likelihood
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
David Alan Schoenfeld
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
#Note the number of iterations is small to control run time
generatedData=generate.cluster.data(.25,npats=25,clusts=c(12,8),beta=c(-5,5),
                                    outcomeModel=binaryOutcome(0))
usBinary=supcluster(generatedData[[1]],outcome="outcome",
maxclusters=5,nstart=100,n=200,fbeta=FALSE,
linkLikelihood=binaryLink(generatedData[[2]]))
## The function is currently defined as
function (x) 
{
    m = length(x)
    outfcn = function(parm2) {
        bx = parm2[m + 1] + parm2[1:m]
        loglik = sum(x * bx) - sum(log(1 + exp(bx)))
        return(loglik)
    }
    return(outfcn)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~binary }
\keyword{ ~likelihood }% __ONLY ONE__ keyword per line
