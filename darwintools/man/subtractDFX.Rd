\name{subtractDFX}
\alias{subtractDFX}
\title{Map to map DFX subtraction}
\description{Subtract on hand of coordinates one DFX scan from other one. The function is quite hungry with respect to computational time. Therefor usage for scans with >1000 defects is not recommended.}
\usage{
subtractDFX(data1, data2, dis=100,  paired="SORT", crit="distance")
}
\arguments{
	\item{data1}{DFX dataframe (structure see readDFXdata)}
	\item{data2}{DFX dataframe (structure see readDFXdata)}
	\item{dis}{maximum accepted distance between same defect in two scans}
	\item{paired}{shall the pairs be estimated by search for pairs between defects- exact
          but more time consuming way - boolean; alternative contains pair algorithm - see pairedDFX help}
	\item{crit}{criteria for pair estimation, either \bold{size} by defect size (default)
       or \bold{distance}.
       }

}
\details{
 Function subtracts map to map data1-data2. Maximum distance between defects location in both scans
 treated as identical is \bold{dis}. Setting option \bold{paired} to FALSE (default setting)
 identifies any defect in \bold{dis} radius as paired, what may lead to mistakes. In case there
 are two defects in \bold{data1} close to one defect in \bold{data2} both of them will be subtracted.
 Setting paired TRUE leads to calculation of defect pairs between scans and result of subtraction 
 is estimated on hand of the defect pairs. This procedure is significatly slower, but lead to 
 exact data. All defects within \bold{dis} radius are treated as pair candidates. In case there
 are several candidates, \bold{crit} criteria set priority. In case \bold{crit} is set to size
 (default), within candidates the most similar defect with respect to defect size is picked as paired.
 In case \bold{crit} is set to distance, the closest candidate is picked without taking into account
 differences in size of defects.
 Recommended value of \bold{dis} is about 100 microns, since experimentally estimated mismatch
 in coordinates between two DFX scans is up to 20 microns in X and 80 microns in Y direction. 
}
\value{  
	DFX data frame
	The return value is data frame containing one observation (row) per defect imported.  The columns of the data frame are following:
	\item{Id}{numeric representation of the defect type 0-5  see help \bold{readDFX}}
	\item{Nr}{Defect number - not used, kept for traceability of defects only}
	\item{Size}{Defect size in microns - diameter?}
	\item{X}{X coordinate of the defect in mm referring to lower left corner of the mask}
	\item{Y}{Y coordinate of the defect in mm referring to lower left corner of the mask}
	\item{belongs}{internaly used vector marking defects for counting and ploting procedures. Manipulation may lead to wrong results.}
}
\author{Pavel Nesladek}
\seealso{pairedDFX, readDFX, viewDFXplot, addDFXplot}
\examples{

## Do not run

# data  <- readDFX("DFX_scan.txt", Dread=0)
# data2  <- readDFX("DFX_scan2.txt", Dread=0)

## plot defects found in first scan
# viewDFXplot(data, pch=19, cex=0.5)

## find adders
# adders <- subtractDFX(data2, data, dis=200, paired=TRUE, crit="size")
# addDFXplot(adders, pch=19, cex=0.5, col="red")

## find removed defects
# removed <- subtractDFX(data,data2)
# addDFXplot(removed, pch=19, cex=0.5, col="blue")

}
\keyword{misc}
