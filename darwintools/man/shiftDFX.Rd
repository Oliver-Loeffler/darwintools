\name{shiftDFX}
\alias{shiftDFX}
\title{shift DFX data }
\description{\bold{shiftDFX} function translates defects in the DFX data by given shift in microns.}
\usage{
shiftDFX(data, shift)
}
\arguments{
	\item{data}{DFX dataframe (structure see readDFXdata)}
	\item{shift}{vector with two components advising shift along X and Y axis in milimeter}
}
\details{
	Function is used for shift of DFX data to match other data set. Main aim is to match defect position between two defect scans performed at different tools or shifted for different possible reasons. Return value is data set shifted by the amount specified in \bold{shift} parameter, with same structure as \bold{data}. 
	When used in conjunction with \bold{getDFXoffset} function, be aware of maximum allowed mismatch of 1 mm. For details see \bold{getDFXoffset} help. \\
	For proper work of \bold{shiftDFX()} function the presence of columns \bold{X} and \bold{Y} in input data is essential.\\
	Unit coordinates of input data needs to be milimeter. This is guaranteed by following functions: \bold{readDFX(), readKLA(), readMD(), readNF()}, but need to be possibly transformed for external data sources.
}
\value{  
	The return value is identical to input data set- for DFX data consult in \bold{readDFXdata} help. The columns of the data frame in case of DFS data are following:
	\item{Id}{numeric representation of the defect type 0-5}
	\item{Nr}{Defect number - not used, kept for traceability of defects only}
	\item{Size}{Defect size in microns}
	\item{X}{X coordinate of the defect in mm referring to lower left corner of the mask}
	\item{Y}{Y coordinate of the defect in mm referring to lower left corner of the mask}
	\item{belongs}{internaly used vector marking defects for counting and ploting procedures. Manipulation may lead to wrong results.}

}
\author{Pavel Nesladek}
\seealso{getDFXoffset, plotDFXoffset}
\examples{
### Do not run
# data1  <- readDFX("DFX_scan1.txt", Dread=0)
# data2  <- readDFX("DFX_scan2.txt", Dread=0)

### plot both data sets together
# viewDFXplot(data1, pch=19, cex=0.3)
# addDFXplot(data2, pch=19, cex=0.2, col="red")

### shift data2 by 0.120 mm in X and 0.056 mm in Y.
# data2r <- shiftDFX(data2, shift=c(0.12, 0.056))

### and check orientation + offset in plot again
# viewDFXplot(data1, pch=19, cex=0.3)
# addDFXplot(data2r, pch=19, cex=0.2, col="red")

## estimate adders
# adders <- subtractDFX(data2r, data1, dis=100)

### plot adders only
# viewDFXplot(adders, pch=19, cex=0.3)
}
\keyword{misc}
