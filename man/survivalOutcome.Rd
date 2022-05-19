\name{survivalOutcome}
\alias{survivalOutcome}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulates a survival model for use with \code{\link{generate.cluster.data}}
%%  ~~function to do ... ~~
}
\description{Given a vector of frailties, say \eqn{x_1,...}, this function generates a censored exponentially distributed random variable with rate equal to \eqn{\mu+\beta x_i}.  The censoring distribution is uniform with from \eqn{f} to 
\eqn{f+a}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
survivalOutcome(mu, beta, accrual, followUp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu}{ The constant term \eqn{\mu}
%%     ~~Describe \code{mu} here~~
}
  \item{beta}{ The frailty effect \eqn{\beta}
%%     ~~Describe \code{beta} here~~
}
  \item{accrual}{ The accrual time \eqn{a}, in a clinical study}
%%     ~~Describe \code{accrual} here~~
  \item{followUp}{ The follow up time \eqn{f} in a clinical study
%%     ~~Describe \code{followUp} here~~
}
}
\value{ A data frame is returned with two columns \code{survival} and \code{censor}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{coxLink}},\code{\link{binaryOutcome}},\code{\link{binaryLink}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
generatedData=generate.cluster.data(.25,npats=25,clusts=c(12,8),beta=c(-5,5),
                                    outcomeModel=survivalOutcome(0,1,1,1))
usBinary=supcluster(generatedData[[1]],outcome="outcome",
maxclusters=5,nstart=100,n=200,fbeta=FALSE,
linkLikelihood=coxLink(generatedData[[2]]))
}
\keyword{ ~survival }
\keyword{ ~censoring }% __ONLY ONE__ keyword per line
