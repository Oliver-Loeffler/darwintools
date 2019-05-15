\name{plotCDU}
\alias{plotCDU}
\title{CDU bubble plot}
\description{Create bubble plot viewgraph, similar to Excel, with some additional options.}
\usage{
plotCDU(x,y,CD, col=FALSE, scale=1, nwhite=TRUE, CATS=NA, xlim=NULL, ylim=NULL)

}
\arguments{
	\item{x}{vector of X coordinates (horizontal) of measurement sites}
	\item{y}{vector of Y coordinates (vertical) of measurement sites}
	\item{CD}{vector of CD sizes for each measurement sites}
	\item{col}{provides color name for bubbles. In case one color is specified, this color is used for all bubles. For negative deviations the buble color can be overruled by  parameter \bold{nwhite}.} 
	\item{scale}{scaling factor used to change the default selected bubble size.}
	\item{nwhite}{boolean parameter which decides, whether negative deviations from mean will be plotted by white (default) or color given by \bold{col} parameter.}
	\item{CATS}{are coordinates of measurement sites referring to lower left corner of the mask (TRUE) or mask center (FALSE). By default \bold{NA} - the function makes guess on hand of coordinates distribution.}
	\item{xlim}{extension on the x axis of the plot window}
	\item{ylim}{extension on the y axis of the plot window}
}
\details{
	Function creates new plot window and creates bubble plot similar to Excel. By default the axis scaling and origin of measurement sites are estimated by the fuction. This can be overruled by \bold{xlim},\bold{ylim} and \bold{CATS} parameters. By default the positive deviation from mean are drawn in light blue and negative deviations in white.
Color of each point in the plot can be adjusted independently by providing vector with colors (names or numbers) in \bold{col}. Unless \bold{nwhite} parameter is set to FALSE, all negative deviations will be plotted in white. 
Function can not be used for CD data in matrix. in such case apply \bold{as.vector()} function first

}
\value{  
	None
}
\author{Pavel Nesladek}

\seealso{readCDXML}

\examples{
X <- rep(seq(-50, 50, 10), 11)
Y <- rep(seq(-50, 50, 10), each=11)
R <- sqrt(X^2 + Y^2)
CDX <- (200 + X*0.01 - Y*0.015 + R^2*1E-3 - R^4*1.4E-9)+ rnorm(121)
plotCDU(X,Y,CDX)

# alternatively with slightly bigger bubbles in red 
plotCDU(X,Y,CDX, col="red", scale=1.3)


}
\keyword{misc}
