\name{Defect data plotting}
\alias{viewDFXplot}
\alias{addDFXplot}
\title{plot DFX and inspection data}
\description{Plot spatial distribution of DFX, inspection(KLA, NF and MD2500) defects data}
\usage{
viewDFXplot(data, title="", xlim=NULL, ylim=NULL, pch=19,
 cex=0.7, col=NULL, add=FALSE, CATS=TRUE, border="mask")

addDFXplot(data, title="", xlim=NULL, ylim=NULL, pch=19,
 cex=0.3, col=NULL, add=TRUE, CATS=TRUE, border="mask")
}
\arguments{
	\item{data}{ dataframe having structure described at readDFXdata. In most cases  the data readed by readDFXdata or subset of it.}
	\item{title}{mean title of the plot}
	\item{xlim}{ extension on the x axis of the plot window. The coordinates are referring to the center of the mask}
	\item{ylim}{ extension on the y axis of the plot window. The coordinates are referring to the center of the mask}
	\item{pch}{symbol used for plotting .. see plot.default for details}
	\item{cex}{scaling factor for plotting symbol ... see par for details}
	\item{col}{color used for symbols}
	\item{add}{add points to previous plot by default or create new one if FALSE}
	\item{CATS}{uses CATS cordinates system instead of mask center based. If FALSE, incoming X and Y coordinates are decreased by 76 mm.}
	\item{border}{border to be drawn; \bold{mask} draws mask outline; \bold{ins} draws red frame at about biggest DFX inspecion area of 70x70mm}
}
\details{
viewDFXplot overwrites previous plot or creates new devide (window) if none open and plots defect map of given data parameter. If add=TRUE defect map is added instead.\\
addDFXplot by default adds points to previous plot rather than creates new one. This behavior can be overruled by add=FALSE.
Both function do not distinguish between defect types. If needed, you habe to implement it by e.g. multiple \bold{pch} or \bold{col} parameter. Default plotting color for viewDFXplot is given by belongs flag, black for non marked defects(belongs=1), red for marked defects (belongs=2) unless specified by col.
The ploted area is kept isotropic - the aspect ratio is 1 at any circumstances. In order to fulfill this criteria the axis extension is changed in case of differences in resolution in X and Y.
Origin of the plot is mask center, - shifted by 76 mm in both X and Y directions toward higher values compared to incoming data frame. This can be overruled by CATS=TRUE. 
X and Y coordinates units are mm.
Parameter \bold{border} can be used to draw or supress drawing of mask outline and maximum inspection area of DFX tools. Default setting is to draw both.
}
\value{  
	No return value.
}
\author{Pavel Nesladek}
\seealso{readDFX, readKLA, readNF, readMD, dmap}
\examples{

## Do not run
# data  <- readDFX("DFX_scan.txt", Dread=0)

## plot all defects from 2 inspections in area 20x20mm  
# viewDFXplot(data, xlim=c(-10,10), ylim=c(-10,10), pch=19, cex=0.7, col="blue", border="mask")
# viewDFXplot(data2, pch=19, col="red", cex=0.4, add=TRUE)

## replot big defects size>1 micron in red
# addplot(data[which(data$Size >1),],  pch=19, cex=0.7, col="red" )

## plot defect map with symbol size representing defect size
# viewDFXplot(data, pch=19, cex=data$Size)


}
\keyword{misc}
