\name{WL}
\alias{WL}
\title{Convert Verity spectrometer channel to wavelength}
\description{
Converts channel number of Verity spectrometer to wavelength.}
\usage{
WL(channel=100)
}
\arguments{
  \item{channel}{channel number 1-1201}
}
\details{
This function converts channel number or vector consisting of channel numbers to wavelength of the Verity spectrometer. Channel number is in range 1-1201 to stay within the spectra. Use for description of x axis when plotting spectra.
}
\value{  
  \item{WL}{Wavelength.}
}
\author{Pavel Nesladek}
\seealso{spectra, CH, header}
\examples{

### Do not run
# SP  <- spectra("A_Plasmastep.svd")
# plot(WL(1:1201), SP[40,], type="l", xlab="wavelength", ylab="intensity counts")

}
\keyword{misc}
