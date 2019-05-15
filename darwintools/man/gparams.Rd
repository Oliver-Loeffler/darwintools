\name{gparams}
\alias{gparams}
\title{Set nice graphics parameters}
\description{Set graphics parameters in order to create publication like viewgraphs.}
\usage{
gparams()
}
\arguments{
None
}
\details{
 Function sets following parameter using par function
	\itemize{
	\item{omi}{ all outer margins = 0}
	\item{mai}{viewgraph margins = 0.5,0.5,0.25,0.05}
	\item{font.lab}{label font type = 2 - bold}
	\item{font.axis}{axis description font type = 2 - bold}
	\item{cex.axis}{scaling axis description = 0.8}
	\item{mgp}{The margin line (in mex units) for the axis title, axis labels 
and axis line = 1.3, 0.5, 0}
	\item{lab}{approximate number of tickmarks on the x and y axes = 7,7,2 }
	}
}
\value{  
none
}
\author{Pavel Nesladek}
\seealso{viewDFXplot, viewmap}
\keyword{misc}
