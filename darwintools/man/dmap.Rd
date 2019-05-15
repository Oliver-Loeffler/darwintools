\name{dmap}
\alias{dmap}
\title{Calculate bivariate defect density distribution from DFX data}
\description{Calculate defect density at 1mmx1mm area in 1 mm grid for in total 152x152 positions on the mask.}
\usage{
dmap(data, grid=1)
}
\arguments{
	\item{data}{DFX dataframe (structure see \bold{readDFXdata})}
	\item{grid}{grid of resulting density map}
}
\details{
 The density is calculated in grid of \bold{grid} parameter, by default 1x1 mm. Grid can be freely changed. In case of high scattering, smoothing can be used by aplying \bold{dsmooth} function on result of \bold{dmap} function. The coordinates of the resulting density map are related to same reference point as input data. Output of the function can be ploted using \bold{viewmap()} function or directly using \bold{surface} function (library fields).
}
\value{  
	list containing 3 components
	\item{xords}{x locations of grid lines at which the density is estimated - vector of equidistant positions in ascending order}
	\item{yords}{y locations of grid lines at which the density is estimated - vector of equidistant positions in ascending order}
	\item{zden}{rectangular matrix, containing the particle density values at given location on the mask}
}
\author{Pavel Nesladek}
\seealso{readDFX, dsmooth, viewmap}
\examples{

### Do not run
# data  <- readDFX("DFX_scan.txt", Dread=0)

# map <- dmap(data, grid=3)
# viewmap(map)

# for comparison plot smooth map 
# viewmap(dsmooth(map))

}
\keyword{misc}
