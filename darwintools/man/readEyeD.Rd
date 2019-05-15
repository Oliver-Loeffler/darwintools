\name{readEyeD}
\alias{readEyeD}
\title{robust function for .svd Verity file import}
\description{
the function reads spectra and logistic information as integrating time, time stamps, wavelength spectra count sources count from .svd file and returns list of data. Depending on the source spectrometer setup the file contains data from one or two sources. If more than one data source were used the items in value have addtional dimesion covering source. 
It replaces completelly the functions \bold{spectra} and \bold{header} which are not recommended to use anymore. and are kept for back compatibity only.
}
\usage{
readEyeD(FName)
}
\arguments{
  \item{FName}{file name of svd file to read}
}
\details{
This function read blockwise the svd spectra file written by the EyeD Verity software used for data acquisition from the Verity spectrometer. The maximum spectrometer wavelength range is 200-800nm covered by 1201 channels, however these can be restricted by svc setup. Up to version 0.1.5 avaialble function \bold{spectra} was removed due to improper data reading in case of wavelength restriction.
}
\value{
List of items as follows
  \item{WL}{wavelength vector for each channel one mainly 1201 entries}
  \item{t}{time vector or array for each source one column. for each spectra readed the time elapsed from data acquisition start is stored here}
  \item{SP}{spectra table having 2 or 3 dimensions, dimension one is time - coresponding to the \bold{t} item, second dimension is channel corresponding to \bold{WL} item. last dimension corresponds to source }
  \item{TF}{frame ID - index of each spectra acquired. In some cases the counter is resetted when changing to next step, but not consequently. What is teh trigeer remains unclear yet.}
  \item{SpectraCount}{total number of spectra acquired over all sources}
  \item{Tstamp}{Time stamp of teh first spectra - string}
  \item{SourceCount}{number of sources contained}
  \item{IT}{starting integration time in miliseconds}
  \item{AGC}{integration time in miliseconds after AGC, if not used AGC is 0}

}
\author{Pavel Nesladek}
\seealso{plot.spectra}
\examples{

#### Do not run
# SP  <- readEyeD("A_Plasmastep.svd")
# plot_spectra(SP, time=c(20,32), col=c("red","blue"))

}
\keyword{misc}
