\name{readNKaverage}
\alias{readNKaverage}
\title{N and K spectra average}
\description{Read averaged R and T spectra for given folder.}
\usage{
readNKaverage(folder, fe=NULL)
}
\arguments{
	\item{folder}{folder containing \bold{output} files created by the N and K reflectometer}
	\item{fe}{indexes of output files to average. Default is \bold{NULL} -average all output files contained}
}
\details{
 Function reads all (or specified) measurement points data and estimates mean, minimum, maximum for reflectivity and transmission based on \bold{Tsexp} and \bold{Rsexp}. All data are stored as function of wavelength \bold{WL}
}

\value{
	The return value is data frame containing one wavelength observation per row. The columns of the data frame are following:
	\item{WL}{wavelength in nanometer}
	\item{Rmean}{mean reflectivity Rs as function of wavelength}
	\item{Rmin}{minimum reflectivity Rs as function of wavelength}
	\item{Rmax}{maximum reflectivity Rs as function of wavelength}
	\item{Tmean}{mean transmission Rs as function of wavelength}
	\item{Tmin}{minimum transmission Rs as function of wavelength}
	\item{Tmean}{maximum transmission Rs as function of wavelength}
}
\author{Pavel Nesladek}
\seealso{readNKspectra, readNKmap, readNK}
\examples{
## Do not run
# data  <- readNKaverage("/tmp/262032")
}
\keyword{misc}
