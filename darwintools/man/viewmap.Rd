\name{viewmap}
\alias{viewmap}
\title{Plot binomial defect density distribution calculated from DFX data}
\description{Plot defect density calculated using \bold{dmap} or \bold{dsmooth} functions}
\usage{
viewmap(data, title="", xlim=NULL, ylim=NULL, zlim=NULL)
}
\arguments{
	\item{data}{DFX dataframe (structure see readDFXdata)}
	\item{title}{mean title of the plot}
	\item{xlim}{ extension on the x axis of the plot window. The coordinates are referring to the center of the mask}
	\item{ylim}{ extension on the y axis of the plot window. The coordinates are referring to the center of the mask}
	\item{zlim}{ extension on the z axis - the color scale of the plot window. Data exceeding teh selected range will be plotted white}
}
\details{
 The viewmap function is plotting the defect density map calculated by \bold{dmap} / \bold{dsmooth} functions. The coordinates of the resulting density map are related to mask center. Output of the function can be ploted using \bold{viewmap} function or directly using \bold{surface} function (library fields).
The ploted area is kept isotropic as in case of \bold{viewDFXplot} function.
}
\value{  
none
}
\author{Pavel Nesladek}
\seealso{dmap, dsmooth}
\examples{


## Do not run
# data  <- readDFX("DFX_scan.txt", Dread=0)

# map <- dmap(data)
# viewmap(map)

## for comparison plot smooth map 
# viewmap(dsmooth(map))

}
\keyword{misc}
