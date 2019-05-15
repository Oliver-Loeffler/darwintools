\name{findClusters}
\alias{findClusters}
\title{defect clusters identification}
\description{Function analysis insopection data provided and identifies clusters within the data with given tolerance radius.}
\usage{
findClusters(data, dis=200)
}
\arguments{
	\item{data}{defect list as obtained by from DFX or pattern inspection tool using functions \bold{readDFX} or \bold{readKLA}.}
	\item{dis}{tolerance radius for cluster identification.}

}

\details{
Defects in the incoming data set are analysed with respect to pairwise distance using distance matrix. All defects paires with distance smaller than \bold{dis} are forming 2 defect cluster. Overlapping of such defect paires leads to formation of bigger clusters. 
Result of the analysis is list of defect clusters.\\
Resulting clusters do not need to contain all defects contained in input data. Defects without any other defect in the tolerance radius bold{dis} are excluded from the result.

}
\value{  
	The return value is list contaning data frames - defects forming a cluster. 
}
\author{Pavel Nesladek}
\seealso{getClusters pairCriteriaTable, pairedDFX}
\examples{
### Do not run
# data <- readDFX("DFX_scan1.txt", Dread=0)

### identify clusters
# clusters <- findClusters(data, dis=300)

### count defects in each cluster
# sapply(clusters, dim)[1,]



}
\keyword{misc}
