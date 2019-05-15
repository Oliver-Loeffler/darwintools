\name{readNKmap}
\alias{readNKmap}
\title{Import N and K map file}
\description{Import N and K map file /identify placement of measurement points on the substrate.}
\usage{
readNKmap(folder)
}
\arguments{
	\item{folder}{Directory in which the map file is stored - typically the output folder of a measurement. }
}
\details{
 Functions is used for identification of placemnt of each particular reflectivty and transmission measurement. Depending on the recipe setup, the origin of the coordinates may differ. It is either the center or lower left corner of the mask subtrate. The units are centimeter.
}

\value{
	The return value is data frame containing X nad Y coordinates of each measurement position.  wavelength observation per row. The columns of the data frame are following:
	\item{X}{X coordinates of measurement points}
	\item{Y}{Y coordinates of measurement points}
}
\author{Pavel Nesladek}
\seealso{readNKaverage, readNK}
\examples{
## Do not run
# data  <- readNKmap("/tmp")


}
\keyword{misc}
