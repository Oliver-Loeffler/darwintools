\name{readDFX}
\alias{readDFX}
\title{DFX data import}
\description{Read Siemens DFX data from txt DFX export file.}
\usage{
readDFX(Fname, Dread=0:5, slim=NULL)
}
\arguments{
	\item{Fname}{DFX filename to read - either in working directory or fully qualified file name}
	\item{Dread}{Identifier of defects type to read. Meaningfull values are 0-5.}
	\item{slim}{lower and upper size limits for defects in microns (x1, x2). defects out of this size range are not returned, default \bold{slim=NULL} do not restrict defect size}
}
\details{
 DFX recongnizes 6 different defect types:
 \itemize{
	\item{0}{- defects on chrome side}
	\item{1}{ - chrome pinholes}
	\item{2}{ - chrome inclusions}
	\item{3}{ - defects on glass side}
	\item{4}{ - glass inclusions}
	\item{5}{ -removed defects}
}

Any combination of the defects can be readed by setting the \bold{Dread} parameter appropriatelly. When more than one defect type has to be imported, \bold{Dread} has to be vector containing all types in any order e.g. Dread=c(0,1,4).
Current version of \bold{readDFX()} function is looking for data in current folder. In case the file is not found. The procedure is looking for file in the DFX folder data at T drive.
}
\value{  
	The return value is data frame containing one observation (row) per defect imported.  The columns of the data frame are following:
	\item{Id}{numeric representation of the defect type 0-5}
	\item{Nr}{Defect number - not used, kept for traceability of defects only}
	\item{Size}{Defect size in microns}
	\item{X}{X coordinate of the defect in mm referring to lower left corner of the mask}
	\item{Y}{Y coordinate of the defect in mm referring to lower left corner of the mask}
	\item{Status}{Scan intensity profile for given defect}
	\item{belongs}{internaly used vector marking defects for counting and ploting procedures. Manipulation may lead to wrong results.}
Default \bold{Dread} setting returns defect on chrome side only. X- and Y- origin is the lower left corner of the mask.
}
\author{Pavel Nesladek}
\seealso{viewDFXplot, addDFXplot, dmap}
\examples{
## Do not run
# data  <- readDFX("DFX_scan.txt", Dread=0)
# viewDFXplot(data)

}
\keyword{misc}
