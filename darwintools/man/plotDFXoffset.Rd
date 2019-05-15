\name{plotDFXoffset}
\alias{plotDFXoffset}
\alias{addDFXoffset}
\title{plot DFX offset}
\description{plot offset between 2 DFX data sets calculated by \bold{getDFXoffset}. Mean translation and rotation are shown as well as shift of each isolated defect.}
\usage{
plotDFXoffset(offset, scale=100, col="blue", add=FALSE, index=FALSE)
addDFXoffset(offset, scale=100, col="red", index=FALSE)

}
\arguments{
	\item{offset}{list created by function \bold{getDFXoffset}  containing information about matching defect pairs their offset and average translation and rotation between both sets (for detailed structure see \bold{getDFXoffset})}
	\item{scale}{scaling factor used to make the arrows visible independently from their size, default is 100}
	\item{col}{color of the arrows, usefull for co-plot of several offsets, default is blue}
	\item{add}{if TRUE plot will be added to previous one, otherwise new plot is created - default FALSE}
	\item{index}{shall index the defects according to their position
 in offset variable, default is FALSE}
}
\details{
	Function creates new plot window and plots calculated offset between pairs of defects, assuming \bold{add} is FALSE.  If \bold{add} is true, arrows in teh new plot are added to  existing plot. One can use different \bold{scale} parameter for each plot.The origin - starting point for each arrow is position of the defect in the first data set. 
	Function is not designed for use with hand collected data. 
	Alternativelly you can use \bold{addDFXoffset} when adding offset plot. 

}
\value{  
	None
}
\author{Pavel Nesladek}
\seealso{getDFXoffset, correctDFXoffset}
\examples{
### Do not run
# data1  <- readDFX("DFX_scan1.txt", Dread=0)
# data2  <- readDFX("DFX_scan2.txt", Dread=0)

### estimate and store offset 
# offset <- getDFXoffset(data1, data2)

## view offset
# plotDFXoffset(offset, scale=100, index=TRUE)

### create new data containign corrected data2 set
# data2corrected <- correctDFXoffset(data2, offset)

###plot offset after correction
# plotDFXoffset(offset, scale=100, add=FALSE)
# addDFXoffset(getDFXoffset(data1,data2corrected), scale=1000, col="red")

}
\keyword{misc}
