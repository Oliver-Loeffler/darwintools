\name{plot_spectra}
\alias{plot_spectra}
\title{Plotting svd data}
\description{
Data read by read.spectra function can be plotted by this function. Intensity as function of time, wavelength and both is supported. One cannot use this function for plotting date readed by \bold{spectra} function. There is no such function avaialble for the older data format. 
}
\usage{
plot_spectra(SP, lambda=NULL, time=NULL, sp=1, col="black", xlab = NULL, ylab = NULL,
   xlim = NULL, ylim=NULL, zlim=NULL, tlim=NULL, llim=NULL, add=FALSE)
}
\arguments{
  \item{SP}{spectral data readed using function read.spectra}
  \item{lambda}{wavelength at which the time-intensity plot will be created, has to match perfectly, lambda are stored with step of 0.5 nm in WL item}
  \item{time}{process times at which the wavelenth-intensity plot will be created. Do not need to match perfectly the time stamp. In case of mismatch closest spectra is taken}

  \item{sp}{spectra source to plot, default 1 to plot first spectra source this parameter is limited by sources stored in SP data. The source count is stored in SourceCount item}
  \item{col}{color used for plotting of scatterplot diagrams time-intensity or wavelength-intensity. has no influence on surface plot wavelength-time-intensity, colors are reused, one can specify more colors to distiguish between parameters}
  \item{xlab}{x axis label}
  \item{ylab}{yaxis label}
  \item{xlim}{extension of x axis - if not specified as llim or tlim}
  \item{ylim}{extension of y axis}
  \item{zlim}{extension of zaxis , valid for surface plot only, changes the extension of the color axis at surface plot}
  \item{tlim}{x axis extension for time-intesity plot and surface plot time-wavelength-intesity. Overrules xlim.}
  \item{llim}{x axis extension for wavelength-intesity plot. Overrules xlim, y axis extension for surface plot time-wavelength-intesity. Overrules then ylim.}
  \item{add}{logic switch. if TRUE, plot is added to previously drawn viewgraph, if FALSE, new viewgraph is created. default is FALSE}
}
\details{
The function can be used for easy to do plotting of spectra data. Overview plot for picking time frame or wawelength is created if labda and time parameters are not set.
If lambda is set, wavelength-intensity plot is created, if time is set spectra at given tiem frame is plotted. Multiple wawelength / time frames are allowed. Tehd several lines will be plotted in the viewgraph.
Use llim /tlim for setting of axis length. these parameters are not dependent on plot type and so axis orientation. If intesity is drawn on Y axis ylim has to be used.
Wavelength parameter has to match data. Wavelength used are saved in 0.5 nm step e.g. 200nm, 200.5 nm, etc The wavelength are stored in WL item of the data.

}
\value{  
 None
}
\author{Pavel Nesladek}
\seealso{read.spectra}
\examples{

#### Do not run
# SP  <- read_spectra("A_Plasmastep.svd")
####plot overview 
# plot_spectra(SP)

#### plot spectra acquired at about 20 and 32 seconds first red second blue
# plot_spectra(SP, time=c(20,32), col=c("red","blue"))

#### plot intensity as function of time at wavelengths 520 and 777 nm first red second blue
#### time axis is limited to 50s
# plot_spectra(SP, lambda=c(520,777), tlim=c(0,50), col=c("red","blue"))

}
\keyword{misc}
