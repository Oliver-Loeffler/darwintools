\name{readSVD}
\alias{readSVD}
\title{function for comprehensive .svd Verity file import}
\description{
The function reads spectra and exteded logistic information as e.g. initial as well as through AGC adjusted integrating time , time stamps, wavelength spectra count sources count from .svd file and returns list of data. Depending on the source spectrometer setup the file contains data from one or two sources. If more than one data source were used the items in value have addtional dimesion covering source. 
It replaces completelly the functions \bold{spectra} and \bold{header} which are not recommended to use anymore. and are kept for back compatibity only.
}
\usage{
readSVD(FName)
}
\arguments{
  \item{FName}{file name of svd file to read}
}
\details{
This function read blockwise the svd spectra file written by the EyeD Verity software used for data acquisition from the Verity spectrometer. The maximum spectrometer wavelength range is 200-800nm covered by 1201 channels, however these can be restricted by svc setup. Up to version 0.1.5 avaialble function \bold{spectra} was removed due to improper data reading in case of wavelength restriction.
Function provides detailed information for the header section as recipe name, svc configuration, fully qualified data file name (with folders separed by dubble back slash), lot name, casette name, etc., assuming this data is stored in the file, which is manily driven by the recipe download procedure. Each data file is marked by the serial number of the process chamber, identifying each chamber at each tool.
Logistics section of the SVD file is provided in detailed manner as data frame, provided each entry in the section. Endpoint time corresponding to the call od \bold{ENDPOINT} step in the Sequence is listed separatelly. In case several \bold{ENDPOINT} steps were placed, all their times are returned as vector.
Fucntion has experimental status, adjustments and optimization of the source code and corrections in format are expected. 
}
\value{
List of items as follows
  \item{WL}{wavelength vector for each channel one mainly 1201 entries}
  \item{t}{time vector or array for each source one column. for each spectra readed the time elapsed from data acquisition start is stored here}
  \item{SP}{spectra table having 2 or 3 dimensions, dimension one is time - coresponding to the \bold{t} item, second dimension is channel corresponding to \bold{WL} item. last dimension corresponds to source }
  \item{TF}{frame ID - index of each spectra acquired. In some cases the counter is resetted when changing to next step, but not consequently. What is teh trigeer remains unclear yet.}
  \item{SpectraCount}{total number of spectra acquired over all sources}
  \item{Tstamp}{Time stamp of the first spectra - string}
  \item{IT}{initial integration time as set in the setup section of the svc}
  \item{AGC}{integration time as adjusted by AGC procedure, if present, otherwise NA}
  \item{SourceCount}{number of data sources in data file}
  \item{SourceCount}{number of data sources in data file}
  \item{EP}{Endpoint time, depending on the nuimber of steps, either NA, numer or vector with endpoint times in seconds}
  \item{Config}{Name of the svc endpoin configuration file used for endpoint detection}
  \item{File}{FQ file name of the destionation of resulting svd file}
  \item{Recipe}{Etch recipe as stored in header section, assuming the data is provided by etch tool to Verity endpoint system}
  \item{Wafer}{Wafer name as stored in header section, mainly PodA 1 - assuming the data is provided by etch tool to Verity endpoint system}
  \item{Lot}{Lot name as stored in header section - represented Job number if procesed through the recipe download.}
  \item{Casette}{Casette name as stored in header section - mostly useless for amsk etch tool}
  \item{Logistics}{Logistic information stoerd duting the process stored as data frame, one row each message}
  \item{ChamberID}{Unique etch chamber identifier, stored as sevel digits number}


}
\author{Pavel Nesladek}
\seealso{plot.spectra}
\examples{

#### Do not run
# SP  <- readSVD("A_Plasmastep.svd")
# plot_spectra(SP$SP, time=c(20,32), col=c("red","blue"))

}
\keyword{misc}
