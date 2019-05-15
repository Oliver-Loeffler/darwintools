\name{flipDFX}
\alias{flipDFX}
\title{flip DFX data set along X or Y axis}
\description{\bold{flipDFX} function flips the DFX data set along X or Y axis according to parameter axis. Main usage is to compensate for scan performed on fliped mask when comparing to mask scanned in normal orinetation.}
\usage{
flipDFX(data, axis="Y", Bsize=152.0)
}
\arguments{
	\item{data}{DFX dataframe (structure see readDFXdata)}
	\item{axis}{axis name along which data are mirrored either X or Y, default value is Y }
	\item{Bsize}{blanksize used in transformation. Default is 152.4}
}
\details{
	Function can be used for mirroring of DFX data set. Main aim is to match orientation between scans performed on mask scanned upside down, for example in order to use the front side laser for back side scan. Data are supposed to be in CATS coordinates / reffering to lower left corner of the mask. If not, the coordinates in resulting data set will be shifted bz the difference between tyeh origin and lower left corner of the mask. 
Return value is mirrored data set with same structure as \bold{data}. Optional parameter \bold{Bsize} represents size of mask blank in milimeter. Default value is 152,4 which seems to be fine for majority of data sets. 
In case function returns obviously shifted values, the root cause is most probably inproper coordinate system or wrongly selected blank size.
	Unit coordinates of input data needs to be milimeter. This is guaranteed by following functions: \bold{readDFX(), readKLA(), readMD(), readNF()}, but need to be possibly transformed for external data sources. In all cases the defect coordiated have to be CATS coordinates
}
\value{  
	The return value is DFX data frame descibed in \bold{readDFXdata} help. The columns of the data frame are following:
	\item{Id}{numeric representation of the defect type 0-5}
	\item{Nr}{Defect number - not used, kept for traceability of defects only}
	\item{Size}{Defect size in microns}
	\item{X}{X coordinate of the defect in mm referring to lower left corner of the mask}
	\item{Y}{Y coordinate of the defect in mm referring to lower left corner of the mask}
	\item{belongs}{internaly used vector marking defects for counting and ploting procedures. Manipulation may lead to wrong results.}

}
\author{Pavel Nesladek}
\seealso{rotateDFX, plotDFX}
\examples{
### Do not run
# data1  <- readDFX("DFX_scan1.txt", Dread=0)

### plot data as scanned along with along Y axis mirrored data. 
# viewDFXplot(data1, pch=19, cex=0.3)
# data2 <- flipDFX(data1, axis="Y")
# addDFXplot(data2, pch=19, cex=0.2, col="red")
}

\keyword{misc}
