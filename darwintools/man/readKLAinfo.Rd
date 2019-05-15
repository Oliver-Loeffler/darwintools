\name{readKLAinfo}
\alias{readKLAinfo}
\title{KLA inspection data header}
\description{Read header info from KLA Tencor inspection file.}
\usage{
readKLAinfo(Fname)
}
\arguments{
	\item{Fname}{KLA inspection report filename placed in working directory or specified by fullpath}
}
\details{
	Function reads information from given inspection report. It is not performing file structure analysis, for which reason any trial to different data structure will fail without error message (or may deliver wrong data)
   Information readed is not complete, but most interesting part of the inpection report as inspection area, reference point coordinates, plate and inspection info and
   defect count overview. Detailed list of defects found can be obtained using readKLA function.
}

\value{  
List of items as follows
	\item{Inspection}{Inspection step, Job and orientation}
	\item{InspType}{Inspection type, e.g. D2D or D2DB}
	\item{Pixel}{Inspection pixel size x and y in microns}
	\item{Plate}{Plate line in inspection report}
	\item{ID}{Inspection ID}
	\item{System}{Inspection system used - tool type}
	\item{Date}{Tiem stamp of data storage}
	\item{Alignment}{Alignment info - DB or simple}
	\item{Area}{Inspected are in square mm}
	\item{Defects}{Dataframe containing chrome user defect summary}
	\item{DefectsClass}{Dataframe containing chrome defect classification summary}
	\item{DBTHiRes1}{DBT_HiRes_1 Detector settings}
	\item{DBTHiRes2}{DBT_HiRes_2 Detector settings}
	\item{DBRHiRes1}{DBR_HiRes_1 Detector settings}
	\item{DBRHiRes2}{DBR_HiRes_2 Detector settings}
	\item{DBTL2O}{DBT_Litho_2_Over Detector settings}
	\item{DBTL2U}{DBT_Litho_2_Under Detector settings}
	\item{DDTHiRes1}{DDT_HiRes_1 Detector settings}
	\item{DDBTHiRes2}{DDT_HiRes_2 Detector settings}
	\item{DDRHiRes1}{DDR_HiRes_1 Detector settings}
	\item{DDRHiRes2}{DDR_HiRes_2 Detector settings}
	\item{DDTL2O}{DDT_Litho_2_Over Detector settings}
	\item{DDTL2U}{DDT_Litho_2_Under Detector settings}

}
\author{Pavel Nesladek}
\seealso{readKLA}
\examples{
## Do not run
# data  <- readKLA("608260_282112.txt")

# readKLAinfo("608260_282112.txt")

}
\keyword{misc}
