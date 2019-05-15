\name{pre3.rad}
\alias{pre3.rad}
\title{Calculate and plot radial PRE3}
\description{Plot radial distribution of particle removal efficiency PRE3 calculated from provided
 DFX scans}
\usage{
pre3.rad(data1, data2, data3, title="")
}
\arguments{
	\item{data1}{pre DFX dataframe (structure see readDFXdata)}
	\item{data2}{coat clean DFX data frame}
	\item{data3}{post clean DFX data frame}
	\item{title}{mean title of the plot}
}
\details{
 \bold{pre3.rad} plots PRE3 using pre, coat and post DFX scans provided as parameters.
}
\value{  
none
}
\author{Pavel Nesladek}
\seealso{readDFX}
\examples{

## Do not run
# data1  <- readDFX("DFX_pre_scan.txt", Dread=0)
# data2  <- readDFX("DFX_coat_scan.txt", Dread=0)
# data3  <- readDFX("DFX_post_scan.txt", Dread=0)

# pre3.rad(data1, data2, data3)

}
\keyword{misc}
