\name{header}
\alias{header}
\title{Read header from .svd Verity file}
\description{
Read header information from .svd data file and returns list consisting of header entries. 

}
\usage{
header(FName)
}
\arguments{
  \item{FName}{file name of svd file to read}
}
\details{
Function is not recommended to use anymore. The information needed is provided by \bold{read_spectra}
\bold{header} reads header information of the svd spectra file written by the EyeD Verity software used for data acquisition from the Verity spectrometer. The header contains following information.
\bold{date}, \bold{offset}, \bold{frames}, \bold{size}, \bold{svcRecipe}, \bold{process}. Content see below.

}
\value{  
  \item{date}{time stamp of the first spectra saved in data file}
  \item{offset}{offset in byte from the file begin to first spectra}
  \item{frames}{vactor of 2 integer containing total number of spectra and number of spectra per channel. If equal the numbers represent number of spectra from one source. If different, the first number is number of spectra per source; second one total number of spectra contained in .svd file. So far only one or two sources are contained.}
  \item{sources}{number of sources in data file - either 1 or 2}
  \item{size}{data size in bytes (number of spectra contained times frame size}
  \item{svcRecipe}{EyeD recipe-setting file used for data acquisition}
  \item{process}{Recipe name used at etch tool during data acquisition}
  \item{tstep}{time step between spectra}
  \item{tfirst}{time of the first spectra taken counted from beginning of the process step}
}
\author{Pavel Nesladek}
\seealso{read_spectra, plot_spectra, spectra, CH, WL}
\examples{

### Do not run
# SP  <- spectra("A_Plasmastep.svd")
# H <- header("A_Plasmastep.svd")
# plot(WL(1:1201), SP[40,], type="l", xlab="wavelength", ylab="intensity counts", main=paste("recipe:",H$process))

}
\keyword{misc}
