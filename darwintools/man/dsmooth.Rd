\name{dsmooth}
\alias{dsmooth}
\title{Smoothing of the defect density map}
\description{This function can be applied on the result of the \bold{dmap} function only. The smoothing is performed on rectangular area 3x3 mm. The smoothing areas are overlaping and are quadratic. Output format is equal to input - defect density per 1x1mm in 1 mm grid for in total 152x152 positions on the mask.}
\usage{
dsmooth(data)
}
\arguments{
	\item{data}{density map list obtained by dmap function}
	list contains 3 components

	-xords -153 x locations of grid lines at which the density is estimated. Vector of equidistant positions in ascending order 

	-yords -153 y locations of grid lines at which the density is estimated. Vector of equidistant positions in ascending order

	- zden -152x152 matrix containing the particle density values at given location on the mask.
}
\details{
 The density smoothing is calculated in grid of 1x1 mm, over area 152x152 mm.  The smoothing area is 3x3mm. The coordinates of the resulting density map are related to mask center. output of the function can be ploted using \bold{viewmap} or \bold{surface} functions as well as the of \bold{dmap}.
}
\value{  
	list containing 3 components
	\item{xords}{153 x locations of grid lines at which the density is estimated. Vector of equidistant positions in ascending order}
	\item{yords}{153 y locations of grid lines at which the density is estimated. Vector of equidistant positions in ascending order}
	\item{zden}{152x152 matrix containing the particle density values at given location on the mask}
}
\author{Pavel Nesladek}
\seealso{readDFX, dmap, viewmap}
\examples{

## Do not run
# data  <- readDFX("DFX_scan.txt", Dread=0)

# map <- dmap(data)
# viewmap(map)

# for comparison plot smooth map 
# viewmap(dsmooth(map))

}
\keyword{misc}
