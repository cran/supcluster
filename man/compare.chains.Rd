\name{compare.chains}
\alias{compare.chains}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compare Chains to Test Algorithm Coverage
}
\description{
Suppose say 4 chains are run, then the first two and the last two are combined 
and a concord map of each is calculated, for each pair of genes in the concord map the proportion of times these genes are in the same cluster are calculated for each set of chains.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compare.chains(supcluster.list,chains1,chains2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{supcluster.list}{ The output of \code{supcluster}
%%     ~~Describe \code{otpt} here~~
}
\item{chains1}{The first vector of the chains to be compared}
\item{chains2}{The second vector of chains to be compared}
}

\value{A N(N-1)/2 by 4 matrix is returned. The first two columns are each pair of genes and the next two are the proportion of times that each where in the same cluster in group of chains indicted by chain1 and chain2}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...


\author{David A. Schoenfeld, Jessie Hsu
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{supcluster}},\code{\link{compare.chains}},
\code{\link{beta.by.gene}}}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
#NOTE: only a small number of MCMC iterations are done due to time constraints

dat=generate.cluster.data(.2,npats=40,clusts=c(12,8,5),
                sig=1,gamma=1,beta=c(-5,0,6))
us=supcluster(dat,outcome="outcome",features=1:25,maxclusters=4,nstart=20,n=40,nchains=2)
ts1=compare.chains(us,chains1=1,chains2=2)
#plot of one chain verses another
plot(ts1[,3],ts1[,4])

}
\keyword{ supervised clustering }
