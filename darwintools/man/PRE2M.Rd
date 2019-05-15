\name{Particle Removal Efficiency maps}
\alias{PRE2M}
\alias{PRE3M}

\title{Particle Removal Efficiency (PRE) spatial distribution map calculation}
\description{Functions \bold{PRE2M} and \bold{PRE3M}calculate particle removal efficiency (PRE) map based on 2 - PRE2 or 3 - PRE3 defect data sets.\\
}

\usage{
PRE2M(coat, post, smooth=TRUE)
PRE3M(pre, coat, post, smooth=TRUE)
}

\arguments{
	\item{pre}{data set taken before particle application - structure see details}
	\item{coat}{data set taken after particle application - structure see details}
	\item{post}{data set taken post test clean - structure see details}
	\item{smooth}{switch whitch decides about smoothing of the final data}
}
\details{
Let \bold{pre} be defects data before particle application, \bold{coat} defect data after particle application and \bold{post} defect data post clean.
The removal efficiency of the clean process \bold{PRE2} is estimated on hand of \bold{coat} and \bold{post} as :\\
\bold{PRE2 = (coat-post) / coat} \\
Here all defects present in pre scan are assumed to be removable).\\
The removal efficiency \bold{PRE3} is estimated on hand of all three data sets as: \\
\bold{PRE3 = (coat-post) / (coat-pre)} \\
Here the defects identified in \bold{pre} are assumed to be non removable, but at high enough defect count in \bold{coat} both methods are assymptotically comparable. \\
 The incoming data sets have to have to be DFX raw data sets read in by \bold{readDFX} function.

}

\value{  
Return value is of identical structure as the dmap function - list containing 3 components:
	\item{xords}{x locations of grid lines at which the density is estimated - vector of equidistant positions in ascending order}
	\item{yords}{y locations of grid lines at which the density is estimated - vector of equidistant positions in ascending order}
	\item{zden}{rectangular matrix, containing the PRE values in percent at given location on the mask}
}

\author{Pavel Nesladek}
\seealso{readDFX, dmap, dsmooth}
\examples{

## Do not run
# pre  <- readDFX("DFX_pre_scan.txt", Dread=0)
# coat <- readDFX("DFX_coat_scan.txt", Dread=0)
# post <- readDFX("DFX_post_scan.txt", Dread=0)

## calculate and view PRE2 without smoothing
# P2map <- PRE2M(coat, post, smooth=FALSE)
# viewmap(P2map)

## calculate PRE3 for 
# P3map <- PRE3M(pre, coat, post, smooth=TRUE)
# viewmap(P3map)


}
\keyword{misc}
