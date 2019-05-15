\name{Particle Removal Efficiency}
\alias{PRE2}
\alias{PRE3}

\title{Particle Removal Efficiency (PRE) estimation}
\description{Functions \bold{PRE2} and \bold{PRE3}calculate particle removal efficiency (PRE) based on 2 - PRE2 or 3 - PRE3 defect data sets.\\
}

\usage{
PRE2(coat, post, slim=NULL)
PRE3(pre, coat, post, slim=NULL)
}

\arguments{
	\item{pre}{data set taken before particle application - structure see details}
	\item{coat}{data set taken after particle application - structure see details}
	\item{post}{data set taken post test clean - structure see details}
	\item{slim}{size limit for evaluation - vector with 2 items for minimum and maximum defect size, size limit is taken into account only in caseA - DFX raw data}
}
\details{
Let \bold{pre} be defects data before particle application, \bold{coat} defect data after particle application and \bold{post} defect data post clean.
The removal efficiency of the clean process \bold{PRE2} is estimated on hand of \bold{coat} and \bold{post} as :\\
\bold{PRE2 = (coat-post) / coat} \\
Here negligible number of non removable defects in \bold{pre} is assumed (or all pre defects removable).\\

The removal efficiency \bold{PRE3} is estimated on hand of all three data sets as: \\
\bold{PRE3 = (coat-post) / (coat-pre)} \\
Here the defects identified in \bold{pre} are assumed to be non removable at high enough defect count in \bold{coat} both methods are assymptotically comparable. \\
 The incoming data sets have to have identical structure. Following incoming data types are accepted by both \bold{PRE2} and \bold{PRE3} functions:\\ 
\itemize{
	\item{A}{raw DFX data sets as obtained by reading in using \bold{readDFX()}. The data has to contain columns \bold{X} and \bold{Y} (data.frame and matrix or array with named columns are accepted}
	\item{B}{defect density maps as obtained by \bold{dmap{}} or in combination with \bold{dsmooth()}. This is list containing three items \bold{xords} , \bold{yords} and \bold{zden}. First two are vectors with N equidistant positions on the mask, last item is matrix with N-1 x N-1 values - defect densities}
	\item{C}{quadratic matrix with defect densities}
   }
Be aware, that size limit can be applied only in case \bold{A} - raw DFX data. IN remaining two cases the defect size information is not available and can not be estimated.
}
\value{  

Return value differs dependant on the parameter type and is
	\item{PRE}{number for PRE % estimated on hand of the DFX raw data provided, \\ list with same structure as incoming data with PRE % estimated for each cell \\ or matrix with same dimensions as incoming data.
  }
}
\author{Pavel Nesladek}
\seealso{readDFX, dmap, dsmooth}
\examples{

## Do not run
# pre  <- readDFX("DFX_pre_scan.txt", Dread=0)
# coat <- readDFX("DFX_coat_scan.txt", Dread=0)
# post <- readDFX("DFX_post_scan.txt", Dread=0)

## calculate PRE2 for all defect sizes
# PRE2(coat, post)

## calculate PRE3 for defect sizes 0.1 - 1000nm 
# PRE3(pre, coat, post, slim=c(0.1, 1000))

## calculate PRE3 map - defect sizes are ignored here, since no information about 
## defect sizes are available
# premap <- dsmooth(dmap(pre))
# caotmap <- dsmooth(dmap(coat))
# postmap <- dsmooth(dmap(post))

# PRE3(premap, coatmap, postmap)


}
\keyword{misc}
