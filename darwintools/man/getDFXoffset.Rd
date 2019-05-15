\name{getDFXoffset}
\alias{getDFXoffset}
\title{estimate offset between DFX data sets}
\description{Calculate translation and rotation between two DFX data sets}
\usage{
getDFXoffset(data1, data2)
}
\arguments{
	\item{data1}{DFX dataframe (structure see readDFXdata)}
	\item{data2}{DFX dataframe (structure see readDFXdata)}
}
\details{
	Function calculates translation and rotation between two DFX data sets or other dataframes containing columns \bold{X}, and \bold{Y}. For estimation isolated defects are used. All areas of 1 squre milimeter containing single defect in both scans are selected. This defects are assumed to be non movable and use for offset estimation. The procedure fails if the offset is bigger than 1 mm - the coresponding defecs do not get into same tile of 1x1 mm. During the procedure the correction is estimated for each defect used in order to be identify inconsistency due to movable defects.
	The difference is calculated as \bold{data2} - \bold{data1} so the corerction has to be applied on the second argument to correct the offset. Use \bold{plotDFXoffset} to visualize the offset and \bold{correctDFXoffset} applied to second data set to get matching data.
}
\value{  
	The return value is list containing 6 items:
	\item{X}{X coordinates of each defect taken into offset calculation - originates from data set 1, origin is lower left corner of the mask, unit is mm}
	\item{Y}{Y coordinates of each defect taken into offset calculation - originates from data set 1, origin is lower left corner of the mask, unit is mm}
	\item{Xd}{X offset \bold{data2} against \bold{data1} at given defect in microns}
	\item{Yd}{Y offset \bold{data2} against \bold{data1} at given defect in microns}
	\item{offset}{vector of 2 items containing mean X and Y translation in microns}
	\item{rotation}{CW rotation of the second data set with respect to first one in degree around the mask center.}
}
\author{Pavel Nesladek}
\seealso{plotDFXoffset, correctDFXoffset}
\examples{
### Do not run
# data1  <- readDFX("DFX_scan1.txt", Dread=0)
# data2  <- readDFX("DFX_scan2.txt", Dread=0)

### estimate and store offset 
# offset <- getDFXoffset(data1, data2)

## view offset
# plotDFXoffset(offset, scale=100)

### create new data containign corrected data2 set
# data2corrected <- correctDFXoffset(data2, offset)

###plot offset after correction
# plotDFXoffset(getDFXoffset(data1,data2corrected), scale=1000)

}
\keyword{misc}
