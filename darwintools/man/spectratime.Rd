\name{spectra.time}
\alias{spectra.time}
\title{Read timing information from .svd Verity file}
\description{
Do not use! The information is provided on reliable way by function \bold{read.spectra} ------------------------
The function reads from each spectra saved in .svd file information about the time elapsed 
 from beginning of the data acquisition. The only parameter is the file name of the .svd file. 
  file to read. Return value is data frame containing two columns \bold{time} and \bold{frame} containing: 
	\itemize{
	\item{time}{time from begin of the data acquisition in seconds}
	\item{frame}{id of the spectra in process step - this starts over from 1 when next process step is reached}
   }
}
\usage{
spectra.time(FName)
}
\arguments{
  \item{FName}{file name of svd file to read}
}
\details{
\bold{spectra.time} reads time counter for each spectra contained in the .svd file.
The only parameter is the file name of the .svd file. 
  file to read. 

}
\value{  

 Return value is data frame containing two columns \bold{time} and \bold{frame} containing: 
  \item{time}{time from beginning of the data acquisition in seconds}
  \item{frame}{id of the spectra in process step - this starts over from 1 when next process
   step is reached}Return Value is data frame 

}
\author{Pavel Nesladek}
\seealso{read_spectra, spectra, CH, WL}
\examples{

### Do not run
## plot time intensity at 550nm as function of process time
# SP  <- spectra("A_Plasmastep.svd")
# TT <- spectra.time("A_Plasmastep.svd")
# plot(TT$time, SP[,CH(550)], type="l", xlab="time[s]", ylab="intensity counts", 
#    main=paste("recipe:",H$process))

## add markers at begining of each step - first spectra
# abline(v=TT$frame[which(TT$frame==0)], col="red")
}
\keyword{misc}
