\name{readNK}
\alias{readNK}
\alias{readNK2}
\title{Read N and K data}
\description{Read N and K spectra, and map file  from specified folder}
\usage{
readNK(FDir)
readNK2(FDir)
}
\arguments{
	\item{FDir}{folder containing \bold{output.X} and \bold{mapping.map} files readed }
}
\details{
Function reads map and all spectral data acquired by N and K reflectometer for one measurement. \bold{readNK} uses as source the ASCII data stored in all output.x files, each contaning data for one measurment points. These files contain fitted data in items columns which were not measured. For this reason you need to distiguish between real measured data and fitted data stored in e.g. \bold{Rpexp} column. The data are restricted with respect to wavelength to the range set in recipe as fit interval, even if broader data range is measured.
\bold{readNK2} reads all binary raw data stored for each measured point in files named e.g. \bold{rsdata.1}, \bold{rpdata.1}, \bold{tsdata.1}, \bold{tpdata.1}. \\
Properties not set in the recipe are not measured and therefore corresponding files are not existing. The binary files read using \bold{readNKraw()} function contain the whole wavelength range 190 - 1000nm.
Not measured data are in resulting data frame represented with NA values.

}
\value{  
List of data containing following items
	\item{WL}{wavelength vector for all spectral data used}
	\item{Rs}{reflectivity experimental spectral data for s polarization, wavelength varying across rows, each column contains one measurement point}
	\item{Ts}{transmission experimental spectral data for s polarization, wavelength varying across rows, each column contains one measurement point}
	\item{Rp}{reflectivity experimental spectral data for p polarization, wavelength varying across rows, each column contains one measurement point}
	\item{Tp}{transmission experimental spectral data for p polarization, wavelength varying across rows, each column contains one measurement point}
	\item{map}{data frame containing measurement map in centimeter. Reference is lower left corner.}

}
\author{Pavel Nesladek}
\seealso{readNKmap, readNKspectra}
\examples{
## Do not run
# A <- readNK("/tmp/356741/wafer.1")
## plot Rp spectra for first measurement point
# plot(A$WL, A$Rp[,1])

## plot map of points acquired
# plot(A$map$X,A$map$Y, asp=1, xlab="X [cm]", ylab="Y [cm]")
# text(A$map$X,A$map$Y, 1:length(A$map$X), pos=3)

# A <- readNK2("/tmp/356741/wafer.1")
## plot Rp spectra for first measurement point
# plot(A$WL, A$Rp[,1])

## plot map of points acquired
# plot(A$map$X,A$map$Y, asp=1, xlab="X [cm]", ylab="Y [cm]")
# text(A$map$X,A$map$Y, 1:length(A$map$X), pos=3)



}
\keyword{misc}
