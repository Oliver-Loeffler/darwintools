\name{rotateDFX}
\alias{rotateDFX}
\title{rotates DFX data set clockwise}
\description{\bold{rotateDFX} function turns the DFX data by 90,180,270 degree. Main usage is to compensate for scan of mask in different orientation and allow comparison ot such data sets.}
\usage{
rotateDFX(data, angle, Bsize=152.0)
}
\arguments{
	\item{data}{DFX dataframe (structure see readDFXdata)}
	\item{angle}{angle by which the data will be clockwise rotated.Allowed values are 0,90,180 and 270 degree}
	\item{Bsize}{blanksize used in transformation. Default is 152.4}
}
\details{
	Function can be used for rotation of DFX data set. Main aim is to match orientation between scand performed with different orinetation of the mask. Return value is rotated data set with same structure as \bold{data}. Optional parameter \bold{Bsize} represents size of mask blank in milimeter. Default value is 152, which seems to be ok for majority of data sets. In case after rotation your data show obvious shift to expected, use slightly different \bold{Bsize} value. The results depends on real size of blank and can not be adjusted for all cases corectly.
	Be carefull when use in conjunction with \bold{getDFXoffset} function, which does not allow mismatch bigger than about 1 mm. For details see \bold{getDFXoffset} help.\\
	For proper work of \bold{shiftDFX()} function is presence of columns with names \bold{X} and \bold{Y} needed.\\
	Unit coordinates of input data needs to be milimeter. This is guaranteed by following functions: \bold{readDFX(), readKLA(), readMD(), readNF()}, but need to be possibly transformed for external data sources.
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
\seealso{getDFXoffset, plotDFXoffset}
\examples{
### Do not run
# data1  <- readDFX("DFX_scan1.txt", Dread=0)
# data2  <- readDFX("DFX_scan2.txt", Dread=0)

### plot both data sets together
# viewDFXplot(data1, pch=19, cex=0.3)
# addDFXplot(data2, pch=19, cex=0.2, col="red")

### rotate data2 to match data1 orientation
# data2r <- rotateDFX(data2, angle=180)

### and check orinetation + offset in plot again
# viewDFXplot(data1, pch=19, cex=0.3)
# addDFXplot(data2r, pch=19, cex=0.2, col="red")

## estimate adders
# adders <- subtractDFX(data2r, data1, dis=100)

### plto adders only
# viewDFXplot(adders, pch=19, cex=0.3)
}
\keyword{misc}
