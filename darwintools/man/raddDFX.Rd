\name{raddDFX}
\alias{raddDFX}
\title{radial defect density DFX}
\description{Calculate radial defect density from DFX raw data.}
\usage{
raddDFX(data, bw=0.2)
}
\arguments{
	\item{data}{DFX dataframe (structure see \bold{readDFXdata})}
	\item{bw}{bandwidth of Gausian used in density distribution estimation}
}
\details{
 \bold{raddDFX} calculates radial density ditribution function from raw DFX data. 
 Default bandwidth is 0.2 mm. Result contains dataframe with \bold{x} and \bold{y} columns.
 \bold{x} contains information of radius in range 0 to 100mm. \bold{y} contains corresponding
 defect density in number of defects per square milimeter. 
 Density calculated from radius of each defect is divided by circumference of circe with radius
 \bold{x} for x smaller and equal 70, and 4x length of arc covering the DFX range of view for
radii bigger than 70 mm. 

}
\value{  
	The return value is dataframe containing 2 columns \bold{x} and \bold{y}.
	\item{x}{radius from mask center 0-100 mm}
	\item{y}{defect density at corresponding radius 0-100 mm }
}
\author{Pavel Nesladek}
\seealso{readDFX, radialdDFX}
\examples{
## Do not run
# data  <- readDFX("DFX_scan.txt", Dread=0)
# plot(raddDFX(data), xlab="r [mm]", ylab=expression(bold(paste("Density [",n/mm^2,"]"))), main=title, 
#    xlim=c(0,100))

}
\keyword{misc}
