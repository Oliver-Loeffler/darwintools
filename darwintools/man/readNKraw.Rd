\name{readNKraw}
\alias{readNKraw}
\title{Read N and K spectra raw data }
\description{Function reads raw data from N and K raw data file, typically called like \bold{rsdata.1}, \bold{rpdata.1}, \bold{tsdata.1} and \bold{tpdata.1}. 
}
\usage{
readNKraw(FName)
}
\arguments{
	\item{FName}{file name to read as e.g. \bold{rpdata.1}}
}
\details{
Fuction reads single binary file given by its file name. This data files are binary data with text header. Header content is read as well and provided as text strings line by line. 
These files are created only if corresponding measurement was switche don in recipe. At least one file exists in any case. In contrary to the \bold{output.1} file, this fiels contains full wavelength acquired, independent on teh wavelength range fitted. Data stored in \bold{output.1} file are containing columns for all types Rs, Rp, Ts and Tp independently from what is measured. Not measured data are replaced by some fit data - misleading and wrong in description.
}
\value{  
List containing 2 items 
	\item{header}{text header of file represented as string vector}
	\item{data}{data frame with two columns \bold{Wl} containing wavelength information and \bold{R} containing reflectivity or transmission data read}
}
\author{Pavel Nesladek}
\seealso{readNK, readNKmap, readNKspectra}
\examples{
## Do not run
# A <- readNKraw("/tmp/rpdata.1")
}
\keyword{misc}
