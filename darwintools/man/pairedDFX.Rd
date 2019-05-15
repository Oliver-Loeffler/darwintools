\name{pairedDFX}
\alias{pairedDFX}
\title{Estimate coresponding pairs of defects between two DFX scans}
\description{Calculates coresponding defect paires according to settings. Defects in scan data2
 in radius of distance \bold{dis} around defect in scan data1 are treated as candidates for pair 
 partner. Partner are picked according to criteria \bold{crit} eiter as defect with similar
 size \bold{size} or closest defect \bold{distance}.
 The computation time is strongly increasing with increasing defects in DFX scans. For this reason
 it is recommended to apply pairedDFX function on DFX scans containing max. 1000 defects.
 }
\usage{
pairedDFX(data1, data2, dis=100, crit="distance", alg="SORT")
}
\arguments{
	\item{data1}{DFX dataframe (structure see readDFXdata)}
	\item{data2}{DFX dataframe (structure see readDFXdata)}
	\item{dis}{maximum tolerance in defect position between two scans}
	\item{crit}{criteria for pairs estimation, either \bold{size} by defect size (default) or \bold{distance}.}
	\item{alg}{pairing algorithm; currently available algorithm \bold{TSP} or \bold{SORT}}
}
\details{
 Function estimates coresponding pairs of defects between 2 DFX scans.
 Defects within \bold{dis} radius around a defect are treated as pair candidates. In case there
 are several candidates, \bold{crit} criteria set priority. In case \bold{crit} is set to size
 (default), candidate with closest defect size is picked as pair partner.
 In case \bold{crit} is set to distance, the spatially closest candidate is picked without 
 taking into account differences in size of defects. Preferably appropriate value of \bold{dis}
 is set and defect size used as criteria.
 Recommended value of \bold{dis} is about 100 microns, since experimentally estimated mismatch
 in coordinates between two DFX scans is up to 20 microns in X and 80 microns in Y direction. 
 This function is used in \bold{subtractDFX} function when paired option is set TRUE.
 Parameter \bold{alg} declares which of available method will be used for calculation of paires.
 \bold{TSP} method is the previously used one, calculating defect paires as exact as possible. This method is failing in case defects are heavily grouped or the number of defects is very high. The process is then consuming complete available memory of the computer and finally crashes. 
 This issue is solved by alternative method \bold{SORT}, which is in current version default. This method is solving the defect assignment issue by somewhat
 simpler method, consuming significantly less memory and computing power. The \bold{SORT} method is able to deal with higher number of defects and grouping of defects is less critical as well. 
 The costs of this advantage is minor decrease in precision. The sum of distances between defect paires In some cases may be slightly higher compared to paired calculated by \bold{TSP} method. 
}
\value{  
	matrix with indexes of matching defects in data1 and data2. First column coresponds to data1
      defects, second column to data2. Index is not the defect number as shown in column 2 of 
      data frame, since the defect number starts with 0, but the index with 1. It means that e.g.
      index 5 corresponds to defect number 4.
       
}
\author{Pavel Nesladek}
\seealso{subtractDFX, readDFX, viewDFXplot, addDFXplot}
\examples{

## Do not run

# data  <- readDFX("DFX_scan.txt", Dread=0)
# data2  <- readDFX("DFX_scan2.txt", Dread=0)

## estimate paires of defect
# paires <- pairedDFX(data, data2, dis=150, crit="size")

## plot paires only
# viewDFXplot(data1[paires[,1],], pch=19, col="blue")

## plot adders 
# addDFXplot(data2[-paires[,2],], pch=19, cex=0.5, col="red")

## plot removed defects
# addDFXplot(data1[-paires[,1],], pch=19, cex=0.5, col="green")

}
\keyword{misc}
