\name{readMD}
\alias{readMD}
\title{MD2500 inspection data import}
\description{Read Lasertec MD2500 inspection file.}
\usage{
readMD(Fname)
}
\arguments{
	\item{Fname}{MD2500 inspection report filename placed in working directory or specified by fully qualified file name including path}
}
\details{
	Function reads the defect list from given inspection report. It is not performing file structure analysis and will fail
   in case of applying to inappropriate file format. Function rotates data to CATS coordinates according to parameter listed in input file under parameter name \bold{Lot No.} \\
   Defect map can be plotted using \bold{viewDFXplot()} function. 
   Output can be used in defect map subtraction using  \bold{subtractDFX()} in combination with data frames obtained by functions \bold{readDFX()}, \bold{readKLA()}, \bold{readNF()} or data frame of identical structure. 
Current version of \bold{readMD()} function is looking for data in current folder. In case the file is not found. The procedure is looking for file in the MD folder data at T drive.
}

\value{  
	The return value is data frame containing one data row per defect. Columns of the data frame are following:
	\item{Index}{running index of defect in inspection report}
	\item{X}{X coordinate of the defect in mm referring to lower left mask corner (CATS coordinates). If mask was inspected rotated, the coordinates are rotated correspondingly}
	\item{Y}{Y coordinate of the defect in mm referring to lower left mask corner (CATS coordinates). If mask was inspected rotated, the coordinates are rotated correspondingly}
	\item{SizeX}{Defect size in nanometer - X direction}
	\item{SizeY}{Defect size in nanometer - Y direction}
   \item{Type}{maximum defect size , estimated as maximum of \bold{SizeX}, \bold{SizeY}}
   \item{Kind}{Defect classification, \bold{**} if unclasified}
	\item{ChipX}{Chip column - starting from 1}
	\item{ChipY}{Chip row - starting from 1}
}
\author{Pavel Nesladek}
\seealso{viewDFXplot, readKLA, readNF, readDFX}
\examples{
## Do not run
# data  <- readMD("3540981_K89E528D.txt")
# viewDFXplot(data, pch=19)
## read second data set and plot adders map
# data2  <- readDFX("0A907543.txt")
# addders <- subtractDFX(data2, data, dis=200, paired=TRUE, crit="distance")
# viewDFXplot(adders, add=TRUE, pch=19, cex=0.4, col="blue")

}
\keyword{misc}
