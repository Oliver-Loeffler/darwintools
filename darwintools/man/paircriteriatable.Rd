\name{pairCriteriaTable}
\alias{pairCriteriaTable}
\title{estimate distance matrix for 2 defect data sets}
\description{Calculate matrix containing distances between any defect pair for two defect data sets. }
\usage{
pairCriteriaTable(data1, data2, dis=100, crit="distance")
}
\arguments{
	\item{data1}{defect data set 1 containing columns \bold{X}, \bold{Y} and if crit="Size" also column \bold{Size}}
	\item{data2}{defect data set 2 containing columns \bold{X}, \bold{Y} and if crit="Size" also column \bold{Size}}
	\item{dis}{tolence radius in microns}
	\item{crit}{parameter decides if defect distances or defect size will be uded as measure}
}
\details{
Calculate matrix containing distances between any defect pair for two defect data sets. Distances above tolerance limit \bold{dis} are set to NA to distibuish defect pair candidates from distant defects for identification of defect pair candidates. \bold{crit} parameter set to "size" enforces estimation of defect size differences instead of distances.\\
 Selection of candidated based on parameter \bold{dis} remains the same for both cases  - which items in matrix are filled depends on defect placement and parameter \bold{dis} only.\\
Parameter \bold{dis} is tolerance radius in microns, in which defects are considered as pair candidates, above this limit defects are considered to be different defects.\\
Function is internally used by \bold{pairedDFX()} function when alg="SORT".
}
\value{  
	The return value is matrix with dimesions equal to length of input data sets \bold{data1} and \bold{data2} ; data1 in columns and data2 in rows.\\
}
\author{Pavel Nesladek}
\seealso{pairedDFX, subtractDFX}
\examples{
### Do not run
# data1  <- readDFX("DFX_scan1.txt", Dread=0)
# data2  <- readDFX("DFX_scan2.txt", Dread=0)

### estimate criteria table
# criteria <- pairCriteriaTable(data1, data2, dis=200, crit="distance")

}
\keyword{misc}
