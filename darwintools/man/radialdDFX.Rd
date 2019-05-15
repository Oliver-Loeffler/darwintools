\name{radialdDFX}
\alias{radialdDFX}
\title{plot radial defect density DFX}
\description{Calculate and plot radial defect density from DFX raw data.}
\usage{
radialdDFX(data, title="", bw=0.2)
}
\arguments{
	\item{data}{DFX dataframe (structure see \bold{readDFXdata})}
	\item{title}{main title for radial defect density plot}
	\item{bw}{bandwidth of Gausian used in density distribution estimation}
}
\details{
 \bold{radialdDFX} calculates and plot radial density ditribution function from raw DFX data. 
 Default bandwidth is 0.2 mm. \bold{radialdDFX} uses internally the \bold{raddDFX} function. 

}
\value{  
	None
}
\author{Pavel Nesladek}
\seealso{raddDFX, readDFX}
\examples{
## Do not run
# data  <- readDFX("DFX_scan.txt", Dread=0)
# radialdDFX(data, title="", bw=0.5)

}
\keyword{misc}
