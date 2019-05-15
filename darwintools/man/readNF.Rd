\name{readNF}
\alias{readNF}
\title{Nu Flare inspection data import}
\description{Read Nu Flare inspection file.}
\usage{
readNF(Fname, CATS=TRUE)
}
\arguments{
	\item{Fname}{Nu Flare inspection report filename placed in working directory or specified by full path}
	\item{CATS}{parameter decides if defect coordinates with be transformed to CATS coordinates (rotated if needed and shifted-default. Otherwise alignment will be kept as origin and defect coordinates will be not rotated.
}
}
\details{
	Function reads the defect list from given inspection report. It is not performing file structure analysis, for which reason any trial to read other data types fails without error message (or may deliver wrong data)
	 Using \bold{CATS} switch one can decide if raw data (as saved in the inspection report) or transformed back to CATS coordinates and rotated back to 0 degree will be returned. Default CATS is TRUE, what means transformed data so the coordinates can be directly compared to e.g. DFX defect positions.  Function recognizes 0 and 90 CW degree mask rotation by parameter \bold{Mask 90 degree Turn}
Current version of \bold{readNF()} function is looking for data in current folder. In case the file is not found, the procedure is looking for file in the default NPI 5000 and both NPI7000 folders at T drive.
}

\value{  
	The return value is data frame containing one data row per defect.  The columns of the data frame are following:
	\item{Index}{running index of defect in inspection report}
	\item{X}{X coordinate of the defect in mm referring to lower left mask corner (CATS coordinates). If mask was inspected rotated, the coordinates are rotated correspondingly}
	\item{Y}{Y coordinate of the defect in mm referring to lower left mask corner (CATS coordinates). If mask was inspected rotated, the coordinates are rotated correspondingly}
	\item{SizeX}{Defect size in nanometer X direction}
	\item{SizeY}{Defect size in nanometer Y direction}
	\item{Type}{Defect type either defect type as 1A, 1B, etc. or \bold{un} if unclasified However, this variable is currently kind of unsafe}
	\item{Descr}{Defect type decription - extension of the defect classification}

}
\author{Pavel Nesladek}
\seealso{viewDFXplot, subtractDFX, readKLA, readDFX, readMD}
\examples{
# create overlaz plot of 
## Do not run
# data  <- readNF("5000026_282112.txt")
# viewDFXplot(data, pch=19, cex=0.7, col="red")

## add DFX blank inspection map
# data2  <- readDFX("0A907543.txt")
# viewDFXplot(data2, add=TRUE, pch=19, cex=0.4, col="blue")

}
\keyword{misc}
