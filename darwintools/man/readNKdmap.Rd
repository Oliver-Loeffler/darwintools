\name{readNKdmap}
\alias{readNKdmap}
\title{Import and merge N and K measurement}
\description{Import \bold{mapping.tmp} and \bold{distri.dat} ascii files containing measurement map and evaluation of N and K, film thicknesses, goodness of fit,phase angle etc.}
\usage{
readNKdmap(FDir)
}
\arguments{
	\item{FDir}{Directory in which both files \bold{distri.dat} and \bold{mapping.tmp} are located - typically the output folder of a N and K measurement. }
}
\details{
 Functions is internally using \bold{readNKmap} and \bold{readNKdistri} functions. Providing convenient access to merge of evaluated N and K results merged with measurement map coordinates. 
Number of columns is sum of both data sets.
Return value is columnwise bind of both data frame (map columns \bold{X} and \bold{Y} are leftmost columns. Resulting data frame rows are sorted along increasing \bold{X} and \bold{Y} map coordinates
formating as \bold{distri.dat} file. Column names are extracted from the header of the distri.dat file and spaces in column names replaced with underscore. The return value is containing as many columns as the sum of \bold{distri.dat} and \bold{mapping.tmp} files and as many rows as 
measurement points.
}

\value{
	The return value is data frame containing same columns as return value of \bold{readNKmap} and \bold{readNKdistri} functions. The typical columns are e.g. as follows:
	\item{X}{X coordinates of the measurement point in cm, origin either in mask center or lower left corner, depending of map setup}
	\item{Y}{Y coordinates of the measurement point in cm, origin either in mask center or lower left corner, depending of map setup}
	\item{Point_Index_}{running number of the measurement point}
	\item{Thickness_substrate_(mm)}{Subtrate thickness}
	\item{Thickness_of_1st_layer}{thickness of bottom-most layer in Angstrom, repeated for each subsequent layer}
	\item{Thickness_of_1st_layer}{Goodness of fit (0-1)}
	\item{n@450}{refreaction index at 450nm wavelength.}
	\item{k@450}{absorption index at 450nm wavelength.}
	\item{T_exp@450}{measured transmission at 450nm wavelength.}
	\item{T_cal@450}{calculated transmission at 450nm wavelength.}
	\item{Rs_exp@450}{measured reflectivity at 450nm wavelength.}
	\item{Rs_cal@450}{calculated reflectivity at 450nm wavelength.}
	\item{Phaseshift_@450}{calcualted phase shift for 450nm wavelength.}
	\item{Eg_(eV)}{optical badgap width in electronvolts}
	\item{Patern_Match_Score}{qualityof pattern match - if used (0-1)}
}
\author{Pavel Nesladek}
\seealso{readNKmap, readNKdistri}
\examples{
## Do not run
# data  <- readNKdmap("/tmp")


}
\keyword{misc}
