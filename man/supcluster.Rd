\name{supcluster}
\alias{supcluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clustering of Features Supervised by an Outcome 
%%  ~~function to do ... ~~
}
\description{We assume that each individual has set of features and an outcome, further we assume that the features are organized in clusters with a random effect for each cluster, and that the outcome is related to the random effects by a linear regression. The function supcluster performs an MCMC to determine the parameters of this model including the cluster membership of each feature. The program can also perform the estimation without considering the outcome.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
supcluster(data,outcome,features,log.transform=TRUE,maxclusters=10,
nstart=100,n=500,shape=1,scale=1,alpha=1,betaP=1,fixj="random",
                     fbeta=FALSE,starting.value=NULL,nchains=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A data frame of the input data }
  \item{outcome}{Either the variable number or the variable name of the outcome variable. If \code{fbeta=TRUE}, no outcome variable is used.}
  \item{features}{A list of features either as variable names or column numbers this can't be mixed}
\item{log.transform}{Log transform the feature data. Generally used when the features are gene expressons}
\item{maxclusters}{The maximum number of clusters used}
\item{nstart}{The first nstart-1 values of each MCMC chain are not reported, that is used as a ``burn in''.
%%     ~~Describe \code{nstart} here~~
}
  \item{n}{The number of MCMC iterations for each chain}
%%     ~~Describe \code{n} here~~
  \item{shape}{The shape parameter for the prior on the variance components
%%     ~~Describe \code{shape} here~~
}
  \item{scale}{The starting scale parmeter for the prior on the variance components
%%     ~~Describe \code{scale} here~~
}
  \item{alpha}{The value to use for the Dirichelet prior parameter}
%%     ~~Describe \code{alpha} here~~
  \item{betaP}{The prior precision of the regression parameters.
%%     ~~Describe \code{betaP} here~~
}
  \item{fixj}{If \code{"random"}, then the starting value for cluster membership is set at random.  If \code{"kmeans"} it uses kmeans to set the starting value.  Otherwise it is matrix of features verses clusters, where a 1 indicates that feature \eqn{i} is in cluser \eqn{j} and the cluster membership is assumed to be known. \code{fixj}
  should be set to \code{"random"} when multiple chains are run.
%%     ~~Describe \code{fixj} here~~
}
  \item{fbeta}{If TRUE then the outcome is not used in the clustering algorithm
%%     ~~Describe \code{fbeta} here~~
}
  \item{starting.value}{Starting value for the MCMC. It should be left as NULL when multiple chains are run, in which case the starting cluster membership is determined by \code{fixj}.
Otherwise it is parameter vector similar to the one described under ``value'' below.  
%%     ~~Describe \code{starting.value} here~~
}
  \item{nchains}{ Number of chains to run
%%     ~~Describe \code{nchains} here~~
}
}

\value{
A compound list is returned. At the first level is the chain number.  At the second level there are two elements
\item{inp}{This has twp values \code{maxclusters} giving the maximum number of clusters and \code{ngenes} giving the maximum number of features}
\item{parms}{This is a \code{n} by \code{3+maxclusters+ngenes} matrix.  Each row is one MCMC iteration. The first three columns are the values of the variance components \eqn{\sigma^2,\tau^2}, and \eqn{\gamma^2} the next \code{maxcluster} values are the regression coefficients for each cluster and the final \code{ngenes} values are the cluster membership of each feature}
}
\references{To be added once the paper is published
%% ~put references to the literature/web site here ~
}
\author{ David A. Schoenfeld, Jessie Hsu
%%  ~~who you are~~
}
\note{
When the feature space is large this program runs slowely. In the example only 20 iterations where used for the burn in and only 80 iterations are run. In general this would not be adequate to fully explore the feature space.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{concordmap}},
\code{\link{compare.chains}},\code{\link{beta.by.gene}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets
##--Note you need to change nstart and n in example to get enough iterations
#run supcluster on trauma data.  Note: nstart and n must be increased to,say, 2000,3000 
#and maxclusters increased to 20
data("trauma_data")
us=supcluster(trauma_data,outcome="outcome",features=1:87,
              maxclusters=5,nstart=5,n=20,fbeta=FALSE)
#creates plot in paper
usm=concordmap(us,chains=1,sort.genes=TRUE)
image(1:87,1:87,usm$map,xlab='Genes',ylab='Genes',
      main="Trauma Data Example",
      col=gray(16:1 / 16))
#Associate genes with clusters
data("gene_names")
betas=colSums(us[[1]]$parms[,3:22])
outpt=data.frame(cluster.number=usm$clusters,beta=betas[usm$clusters],gene_names[usm$order,])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}
\concept{supervised cluster analysis}% __ONLY ONE__ keyword per line
\concept{supervised learning}
\concept{machine learning}