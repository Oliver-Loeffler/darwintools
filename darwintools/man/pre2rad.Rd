\name{pre2.rad}
\alias{pre2.rad}
\title{Calculate and plot radial PRE2}
\description{Plot radial distribution of particle removal efficiency PRE2 calculated from provided using \bold{dmap} or \bold{dsmooth} functions}
\usage{
pre2.rad(data1, data2, title="")
}
\arguments{
	\item{data1}{coat DFX dataframe (structure see readDFXdata)}
	\item{data2}{post clean DFX data frame}
	\item{title}{mean title of the plot}
}
\details{
 \bold{pre2.rad} plots PRE2 using coat and post DFX scans provided as parameters.
}
\value{  
none
}
\author{Pavel Nesladek}
\seealso{readDFX}
\examples{

## Do not run
# data1  <- readDFX("DFX_coat_scan.txt", Dread=0)
# data2  <- readDFX("DFX_post_scan.txt", Dread=0)

# pre2.rad(data1, data2)

}
\keyword{misc}
