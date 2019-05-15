\name{readMDinfo}
\alias{readMDinfo}
\title{MD2500 inspection data header reading}
\description{Read Lasertec MD2500 inspection file header.}
\usage{
readMDinfo(Fname)
}
\arguments{
	\item{Fname}{MD2500 inspection report filename placed in working directory or specified by fully qualified file name including path}
}
\details{
	Function reads the header of defect report from given inspection report. As there are no defined needs for itemf of the header, the return value is list containing almost all items from inspection file header. each item has name derived from the item name in inspection report.

   In case of coordinates, those are kept as one line text including units and need to split and converted to numbers for further use.\\
Current version of \bold{readMDinfo()} function is looking for data in current folder. In case the file is not found. The procedure is looking for file in the MD folder data at T drive.
}

\value{  
	The return value is list containing following items:
	\item{Date}{date of file creation}
	\item{Time}{time of file creation}
	\item{Machine}{Machine name (type)}
	\item{Mask}{Mask name}
	\item{Mask}{Mask name}
	\item{Lot}{Lot name}
	\item{Comment}{Comment text}
	\item{Slice}{Slice Name1}
	\item{Speed}{Inspection Speed}
	\item{AUTO}{AUTO COMP}
	\item{ChipDist}{Chip Distance}
	\item{Start}{Start Position}
	\item{Reference}{Reference Position}
	\item{ChipNo}{No. of Chips}
	\item{Scribe}{Scribe Line}
	\item{DefectLim}{Defect Limit}
	\item{LensSep}{Lens Separation}
	\item{Align}{Align on Scan}
	\item{InspChip}{Inspection Chips}
	\item{DefectTot}{Total Defect}
	\item{DefectCheck}{Check Defect}
	\item{DefectPinhole}{Pinhole Defect}
	\item{DefectSpot}{Spot Defect}
}
\author{Pavel Nesladek}
\seealso{readMD}
\examples{
## Do not run
# data  <- readMDinfo("3540981_K89E528D.txt")
}
\keyword{misc}
