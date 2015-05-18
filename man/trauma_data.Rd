\name{trauma_data}
\alias{trauma_data}
\docType{data}
\title{Trauma Data for Supervised Clustering
%%   ~~ data name/kind ... ~~
}
\description{
This is a genomic data set, saved as an R save file, that loaded with \code{data("trauma_data")} and  \code{data("gene_names")} The data frame \code{trauma_data} has 147 observations on patients with trauma. The first 87 columns are gene expression values and the final column labeled outcome is the multiple organ failure score for the patient.  The data in \code{gene_names} is information on each gene in \code{trauma_data}
}
\usage{data("trauma_data");data("gene_names")}
\format{
A data frame \code{trauma_data} with 147 observations the first 87 columns are gene expression data and the last column labeled \code{outcome} is the maximum organ failure score. A data frame \code{gene_names} with the affymetrix description of the probesets in \code{trauma_data}.

}


\source{ N. Rajicic, Dianne M. Finkelstein, and David A. Schoenfeld.(2007) ``Survival analysis of longitudinal
microarrays.'' \emph{Bioinformatics}, 22(21):2643-2649
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\keyword{datasets}
