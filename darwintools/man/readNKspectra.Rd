\name{readNKspectra}
\alias{readNKspectra}
\title{Read N and K spectra from file}
\description{Read N and K spectra from output.X file specified by file name.}
\usage{
readNKspectra(FName)
}
\arguments{
	\item{FName}{file name of \bold{output.X} file to read}
}
\details{
Fuction reads single output file containing one spectrum acquired by N and K reflectometer.
}
\value{  
data frame containing following columns. 
}
\author{Pavel Nesladek}
\seealso{readNK, readNKmap}
\examples{
## Do not run
# A <- readNKspectra("/tmp/3516759/output.1")
}
\keyword{misc}
