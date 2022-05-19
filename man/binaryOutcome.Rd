\name{binaryOutcome}
\alias{binaryOutcome}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulates a binary model for use with \code{\link{generate.cluster.data}}
%%  ~~function to do ... ~~
}
\description{Given a vector of frailties, say \eqn{x_1,...} it creates a binary variable from a logistic model with log odds ratio \eqn{\mu + x} 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
binaryOutcome(mu)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu}{ Constant term \eqn{\mu }
%%     ~~Describe \code{mu} here~~
}
}
\value{
A vector of binary variables.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
David A. Schoenfeld
%%  ~~who you are~~
}
\examples{
generatedData=generate.cluster.data(.25,npats=25,clusts=c(12,8),beta=c(-5,5),
                                    outcomeModel=binaryOutcome(0))
## The function is currently defined as
function (mu) 
{
    outpt = function(x) {
        m = length(x)
        p = exp(mu + x)/(1 + exp(mu + x))
        return(rbinom(m, 1, p))
    }
    return(outpt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~binary }
