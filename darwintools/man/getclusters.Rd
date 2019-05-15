\name{getClusters}
\alias{getClusters}
\title{split defect pair criteria matrix into defect clusters}
\description{Splits defect pair criteria table (see function \bold{pairCriteriaTable()}). Each cluster contains candidates for defect pairs from both input data sets in form of matrix.}
\usage{
getClusters(cr)
}
\arguments{
	\item{cr}{defect pair criteria table as estimated using \bold{pairCriteriaTable}. Format and meaning of content see in help for \bold{pairCriteriaTable}}
}

\details{
Defect pair criteria matrix provided as input is split in smaller matrices representing defect clusters. \\
Resulting clusters do not need to contain all defects contained in inoput data. Defects without pair candidate in counterpart data set are omited.
Smallest cluster is  1 cell big containing distance between exactly one defect in data set 1 and 2. Column and row names give information about the indices of these defects in both data sets.\\
In case the relation between defects is not as simple, the cluster contains all defects connected to each other by distance smaller than tolerance radius \bold{dis} provided to function \bold{pairCriteriatable}. 
Function is internally used by \bold{pairedDFX()} function when alg="SORT".
}
\value{  
	The return value is list contaning matrices with dimesions equal to size of each defect cluster. Column names of each cluster matrix represent defect indexes in data set 1, row names represent defect indexes in data set 2.\\
}
\author{Pavel Nesladek}
\seealso{pairCriteriaTable, pairedDFX}
\examples{
### Do not run
# data1  <- readDFX("DFX_scan1.txt", Dread=0)
# data2  <- readDFX("DFX_scan2.txt", Dread=0)

### estimate criteria table
# criteria <- pairCriteriaTable(data1, data2, dis=200, crit="distance")

### split defect criteria table into clusters
#  Clusters  <- getClusters(criteria)

### and identify defect pairs for first cluster by SORT one of available methos
# pairs <- resolveClusterSORT(Clusters[[1]])

}
\keyword{misc}
