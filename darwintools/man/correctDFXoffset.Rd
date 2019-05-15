\name{correctDFXoffset}
\alias{correctDFXoffset}
\title{correct offset between two DFX data sets}
\description{
Corects tranlation and rotational offset between two DFX data set estimated using \bold{getDFXoffset} function. Offset has to be applied on second data sets - parameter in \bold{getDFXoffset} function. 
}
\usage{
 correctDFXoffset(data, offset)
}
\arguments{
	\item{data}{DFX data set as readed by \bold{readDFX} function this data will be corrected}
	\item{offset}{list created by function \bold{getDFXoffset} containing information about matching defect pairs their offset and average translation and rotation (for details see \bold{getDFXoffset})}
}
\details{
	Function derives new data frame from \bold{data} containing same information but diferent positions of the defects. All defects are shifted according to the distortion given by \bold{offset}. \bold{data} parameter remains untouched.

}
\value{  
	The return value is data frame with structure given by \bold{readDFX} and content almost identical to incoming data. Only X and Y coordinates of each defect are modified. The columns of the data frame are following:
	\item{Id}{numeric representation of the defect type 0-5}
	\item{Nr}{Defect number - not used, kept for traceability of defects only}
	\item{Size}{Defect size in microns}
	\item{X}{X coordinate of the defect in mm referring to lower left corner of the mask}
	\item{Y}{Y coordinate of the defect in mm referring to lower left corner of the mask}
	\item{belongs}{internaly used vector marking defects for counting and ploting procedures. Manipulation may lead to wrong results.}
X- and Y- origin is the lower left corner of the mask.
}
\author{Pavel Nesladek}
\seealso{plotDFXoffset, getDFXoffset}
\examples{
### Do not run
# data1  <- readDFX("DFX_scan1.txt", Dread=0)
# data2  <- readDFX("DFX_scan2.txt", Dread=0)

### estimate and store offset 
# offset <- getDFXoffset(data1, data2)

## view offset
# plotDFXoffset(offset, scale=100, index=TRUE)

### create new data containign corrected data2 set
# data2corrected <- correctDFXoffset(data2, offset)

###plot offset after correction
# plotDFXoffset(offset, scale=100, add=FALSE)
# addDFXoffset(getDFXoffset(data1,data2corrected), scale=1000, col="red")

}
\keyword{misc}
