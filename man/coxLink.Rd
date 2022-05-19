\name{coxLink}
\alias{coxLink}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Used with \code{\link{supcluster}} when the outcome data object is a censored survival variable.
%%  ~~function to do ... ~~
}
\description{Calculates the log-partial likelihood for a proportional hazards model with log-hazard \eqn{\mu+\beta x} where \eqn{x} is a frailty
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coxLink(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A two variable data frame where the first variable is the survival time and the second
  variable is a censoring indicator 1-event happened 0-censored
%%     ~~Describe \code{surv} here~~
}
}
\author{
David A. Schoenfeld
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
generatedData=generate.cluster.data(.25,npats=25,clusts=c(12,8),beta=c(-5,5),
                                    outcomeModel=survivalOutcome(0,1,1,1))
usBinary=supcluster(generatedData[[1]],outcome="outcome",
maxclusters=5,nstart=100,n=200,fbeta=FALSE,
linkLikelihood=coxLink(generatedData[[2]]))
}
\keyword{ ~cox }
\keyword{ ~survival }% __ONLY ONE__ keyword per line
