\name{CH}
\alias{CH}
\title{Conversion wavelength to channel - Verity spectrometer}
\description{
Converts wavelength to channel number at Verity spectrometer.}
\usage{
CH(lambda=520)
}
\arguments{
  \item{lambda}{wavelength in nm}
}
\details{
This function converts wavelength or vector consisting of wavelengths to channel number of the Verity spectrometer. Can be used for e.g. picking particullar wavelength from spectra readed from .svd file using spectra function.
}
\value{  
  \item{CH}{channel number.}
}
\author{Pavel Nesladek}
\seealso{spectra, WL}
\examples{


 CH(c(256,520,777))

}
\keyword{misc}
