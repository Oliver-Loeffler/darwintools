\name{readNFinfo}
\alias{readNFinfo}
\title{NuFlare inspection header import}
\description{Read NuFlare inspection file header}
\usage{
readNFinfo(Fname)
}
\arguments{
	\item{Fname}{Nu Flare inspection report filename placed in working directory or specified by full path}
}
\details{
	Function reads the inspections settings, mask information and details of the inspection process. In this process the defect list is ignored in this function. Function is assuming 
certain structure of the file and structure analysis is not performed - for which reason any trial to read other data types fails without error message (or may deliver wrong data).
Current version of \bold{readNFinfo()} function is looking for data in current folder. In case the file is not found, the procedure is looking for file in the default NPI5000 and both NPI7000 folders at T drive.
}

\value{  
	The return value is list containing following items:
	\item{Serial}{Insepction ID (FMID)}
	\item{Device}{Device name of the inspected mask}
	\item{Layer}{Mask layer name.}
	\item{IStart}{Inspection start date+time in format YYYY/MM/DD HH:MM:SS}
	\item{IEndt}{Inspection end date+time in format YYYY/MM/DD HH:MM:SS}
	\item{Tool}{Inspection tool ID}
	\item{Layout}{Inspection settings: pixel size - inspetion Mode - rotation e.g. P60-PatParFu-0-DNIR }
	\item{Job}{Job number}
	\item{ID}{Inspection Step e.g. E1}
	\item{Mode}{Inspection mode - D-DB (T&R Mode)}
	\item{Resolution}{Pixel size in nanometer}
	\item{Idata}{Inspection data name e.g. 471127-1_1_NPI_VSB121}
	\item{MaskAbs}{Mask absorber material - e.g. OMOG}
	\item{Mask90D}{Mask rotation 90deg}
	\item{Alignment}{Coordinates of reference point - typically first alignment mark}
	\item{InspTact}{Inspection time in format 02:01:37}
	\item{TotalStrip}{Number of inspection stripes}
	\item{StartStrip}{Starting stripe number}
	\item{EndStrip}{End stripe number}
	\item{IStatus}{Inspection status}
	\item{IAreaCount}{Inspection area count}
	\item{NIAreaCount}{Count of non-inspection areas }
	\item{IArea}{data frame, containing position and size of each inspections area}
	\item{NIArea}{data frame, containing position and size of non-inspections area}
	\item{PriorIArea}{not used}
	\item{SumDim}{Inspection area in mm^3}
	\item{Align1}{Coordinates of 1st alignment mark / referring to Alignment point}
	\item{Align2}{Coordinates of 2nd alignment mark / referring to Alignment point}
	\item{Align3}{Coordinates of 3rd alignment mark / referring to Alignment point}
	\item{Align4}{Coordinates of 4th alignment mark / referring to Alignment point}
	\item{RigNr}{Number of RIG points}
	\item{Rig}{data frame containing Id, Name, X and Y coordinates of each RIG point}
	\item{Stagespeed}{Stage speed in um/s}
	\item{LightCal}{data frame containing calibration marks}
}
\author{Pavel Nesladek}
\seealso{readNF, readKLA}
\examples{

## Do not run
# data  <- readNFinfo("5000026_282112.txt")

}
\keyword{misc}
