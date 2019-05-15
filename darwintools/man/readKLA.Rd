\name{readKLA}
\alias{readKLA}
\title{KLA inspection data import}
\description{Read KLA Tencor inspection file.}
\usage{
readKLA(Fname, CATS=TRUE)
}
\arguments{
	\item{Fname}{KLA inspection report filename placed in working directory or specified by fullpath}
	\item{CATS}{parameter decides if defect coordinates with be transformed to CATS coordinates (rotated if needed and shifted-default TRUE. If FALSE, alignment will be kept as in data file and defect coordinates will not be rotated. Function is able to handle any angle with direction identified by ending \bold{IU} for CW or \bold{GU} for CCW }
}
\details{
	Function reads the defect list from given inspection report. It is not performing file structure analysis, for which reason any trial to read NuFlare inspection report will fail without error message (or may deliver wrong data)
	 Using \bold{CATS} switch one can decide if raw data (as saved in the inspection report) or transformed back to CATS coordinates and rotated back to 0 degree will be returned. Default CATS is TRUE, what means transformed data so the coordinates can be directly compared to e.g. DFX defect positions. 
Current version of \bold{readKLA()} function is looking for data in current folder. In case the file is not found. The procedure is lookign for file in the KLA folder data at T drive.
}

\value{  
	The return value is data frame containing one data row per defect.  The columns of the data frame are following:
	\item{Index}{defect index in inspection report}
	\item{X}{X coordinate of the defect in mm referring to lower left mask corner (CATS coordinates). If mask was inspected rotated, the coordinates are rotated correspondingly}
	\item{Y}{Y coordinate of the defect in mm referring to lower left mask corner (CATS coordinates). If mask was inspected rotated, the coordinates are rotated correspondingly}
	\item{SizeX}{Defect size in nanometer X direction}
	\item{SizeY}{Defect size in nanometer Y direction}
	\item{Type}{Defect type either defect type as 1A, 1B, etc. or \bold{un} if unclassified}
	\item{Descr}{Defect type decription - extension of the defect classification}
}
\author{Pavel Nesladek}
\seealso{viewDFXplot, subtractDFX, readNF, readDFX, readMD}
\examples{
## Do not run
# data  <- readKLA("608260_282112.txt")
# viewDFXplot(data, pch=19, cex=0.7, col="red")

## add second inspection data set  from Nu Flare tool
# data2  <- readNF("5000026_282112.txt")
# viewDFXplot(data2, add=TRUE, pch=19, cex=0.4, col="blue")

}
\keyword{misc}
