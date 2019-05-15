\name{readKLAXML}
\alias{readKLAXML}
\title{KLA inspection data import  from XML output file}
\description{Read KLA Tencor XML inspection file format}
\usage{
readKLAXML(Fname, CATS=TRUE)
}
\arguments{
	\item{Fname}{KLA inspection report filename placed in working directory or specified by fullpath}
	\item{CATS}{parameter decides if defect coordinates will be transformed to CATS coordinates (rotated if needed and shifted - default TRUE. If FALSE, alignment will be kept as in data file and defect coordinates will not be rotated. Function is able to handle any angle with direction identified by ending \bold{IU} for CW or \bold{GU} for CCW }
}
\details{
	Function reads the defect list from given XML inspection report. Trial to read other non XML data or XML data of other origin will fail without error message or return incomplete data)
	 Using \bold{CATS} switch one can decide if raw data (as saved in the inspection report) or transformed back to CATS coordinates and rotated back to 0 degree will be returned. Default CATS is TRUE, what means transformed data, so the coordinates can be directly compared to e.g. DFX defect positions. In case of die-to-database inspection the defect coordinates are transformed using the true position of alignement marks stord in teh insepction file. In case of die-to-die inspection this information is not available and so function is checking if estimated alignment mark is less than 3.5mm away from expected position of JEN cross mark. if so, the known JEN cross mark position [5500,14000] is used. Otherwise the measured alignment mark is used.
Current version of \bold{readKLAXML()} function is looking for data in current folder. 


}

\value{  
	The return value is data frame containing one data row per defect.  The columns of the data frame are following:
	\item{Index}{running defect number in inspection report}
	\item{X}{X coordinate of the defect in mm referring to lower left mask corner (CATS coordinates). If mask was inspected rotated, the coordinates are rotated correspondingly}
	\item{Y}{Y coordinate of the defect in mm referring to lower left mask corner (CATS coordinates). If mask was inspected rotated, the coordinates are rotated correspondingly}
	\item{SizeX}{Defect size in nanometer X direction}
	\item{SizeY}{Defect size in nanometer Y direction}
	\item{Type}{Defect type either defect type as 1A, 1B, etc. or \bold{un} if unclassified}
	\item{Detector}{name of detector triggering defect}
	\item{Note}{inspction tool note, in some cases containing information from Residue2x2 and Residue3x3 parameters}
	\item{Residue2x2}{???}
	\item{Residue3x3}{???}
}
\author{Pavel Nesladek}
\seealso{viewDFXplot, subtractDFX, readNF, readDFX, readMD}
\examples{
## Do not run
# data  <- readKLAXML("711897_E1-3744780-0.xml")
# viewDFXplot(data, pch=19, cex=0.7, col="red")

## add second inspection data set  from Nu Flare tool
# data2  <- readNF("5000026_282112.txt")
# viewDFXplot(data2, add=TRUE, pch=19, cex=0.4, col="blue")

}
\keyword{misc}
