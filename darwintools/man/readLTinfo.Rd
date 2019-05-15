\name{readLTinfo}
\alias{readLTinfo}
\title{LaserTec inspection data header}
\description{Read header info from Magics LT inspection file.}
\usage{
readLTinfo(Fname)
}
\arguments{
	\item{Fname}{LT inspection report filename placed in working directory or specified by fullpath}
}
\details{
	Function reads information from given inspection report. It is not performing file structure analysis, for which reason any trial to different data structure may fail without error message (or may deliver wrong data)
   Information read is not complete, but file header under \@Common and the settings for front side and back side inspection (\@Front, \@Back) as well as defect count overview are returned. Detailed list of defects found can be obtained using readLT function.
}

\value{  
List of items as follows
	\item{Job#}{numeric}
	\item{Pod#}{numeric}
	\item{BlankSerial#}{character}	
	\item{...}{around ten other settings and parameters}
	\item{}{}
	\item{Front}{Table of front side settings and parameters, around ten.}
  \item{Back}{Table of back side settings and parameters}

}
\author{Pavel Nesladek, Jonas Schmidt}
\seealso{readLT}
\examples{
## Do not run
# readLTinfo("9A000091.csv"")

# data  <- readLT("9A000091.csv)
# myJob <- data$Job
# inspectionStart <- data$Front$InspectionStartTime
# data$DefectCount

}
\keyword{misc}
