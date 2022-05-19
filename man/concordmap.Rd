\name{concordmap}
\alias{concordmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate the Frequency with which each Pair of Features are in the Same Cluster
%%  ~~function to do ... ~~
}
\description{Label switching is a problem in interpreting the results of a cluster analysis that uses MCMC. Two clusterings may be the same but the labels of the clusters may change. In order to avoid this problem we create a square matrix with length and width equal to the number of features. The i,jth element is the proportion of times feature i and j are in the same cluster. A sorting algorythm puts the genes that are clustered together next to each other.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
concordmap(supcluster.list, chains=1, sort.genes = FALSE,criteria=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{supcluster.list}{ The output of \code{supcluster}}
  \item{chains}{The chains to use in the clustering}
  \item{sort.genes}{If \code{TRUE} Genes that associated are put next to each other}
  \item{criteria}{Two genes are in the same cluster when the probability that they are in the same
  cluster is greater or equal to the criteria.}
%%     ~~Describe \code{otpt} here~~
}
%%     ~~Describe \code{sort.genes} here~~



\value{If sort.genes=TRUE a three element list, the first element is a m x m matrix where m is the number of features and the second element is the ordering created by sorting algorythm that this matrix is in. The final element is the cluster membership for each of the genes.  
If sort genes=FALSE only the m x m matrix is returned.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{David A. Schoenfeld, Jessie Hsu
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{supcluster}},,\code{\link{compare.chains}},\code{\link{beta.by.gene}}}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
#NOTE: only a small number of MCMC iterations are done due to time constraints
dat=generate.cluster.data(.2,npats=40,clusts=c(12,8,5),
                sig=1,gamma=1,beta=c(-5,0,6))[[1]]
us=supcluster(dat,outcome="outcome",features=1:25,maxclusters=4,nstart=20,n=40,nchains=2)
ts1=concordmap(us,chains=1)
#plot of the concord map
image(1:25,1:25,ts1$map)

}
\keyword{cluster}
