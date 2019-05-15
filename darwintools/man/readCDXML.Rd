\name{readCDXML}
\alias{readCDXML}
\title{CD measurement data import  from XML output file}
\description{Read CD measurement data obtained from different CDSEM tools exported to XML file format}
\usage{
readCDXML(Fname)
}
\arguments{
	\item{Fname}{filename placed in working directory or specified by fully qualified file name}
}
\details{
	Function reads the CD data, including site index and subindex, target measurement coordinates, target name, orientation, and feature type. Function is not testing file consistency and so trial to read other fiel format might fail with or without error message, possibly with wrong return value. \bold{readKLAXML()} function provides CD data ready for FDL evaluation or CDU plot etc. 
}

\value{  
	The return value is data frame containing one data row per defect. The columns of the data frame are following:
	\item{Index}{running number of measurement site}
	\item{SubIndex}{measurement site sub Index - different from 1 when more values at one site estimated e.g. LER/LWR}
	\item{X}{target X coordinate of the measurement site in microns. This value is the target value, not the postion found by pattern recognition.}
	\item{Y}{target X coordinate of the measurement site in microns.  This value is the target value, not the postion found by pattern recognition.}
	\item{Bottom}{CD value, LER or LWR value in nm, depending on recipe.}
	\item{Target}{Target name as called in recipe.}
	\item{Type}{Feature type measured}
	\item{Orientation}{Orientation of measurement - at lines / spaces most frequently perpendicular to line orientation}
	\item{XDie}{Die column ID, when recipe defined as multidie; otherwise 0}
	\item{YDie}{Die row ID, when recipe defined as multidie; otherwise 0}
}
\author{Pavel Nesladek}
\seealso{plotCDU}
\examples{
## Do not run
# data  <- readCDXML("4062999_ANTARES_CLEANMASK-CDU-AB_1_CHCD_20131216104830.xml")


## plot CDU
# plotCDU(data$X, data$Y, data$Bottom)

}
\keyword{misc}
