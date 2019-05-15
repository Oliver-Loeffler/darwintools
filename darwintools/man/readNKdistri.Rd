\name{readNKdistri}
\alias{readNKdistri}
\title{Import N and K result file distri.dat}
\description{Import distri.dat ascii file containing evaluation of N and K, film thicknesses, goodness of fit,phase angle etc.}
\usage{
readNKdistri(FDir)
}
\arguments{
	\item{FDir}{Directory in which the \bold{distri.dat} file is located - typically the output folder of a N and K measurement. }
}
\details{
 Functions is providing convenient access to evaluated N and K tool measurement data. Results are returned as data frame containing same 
formating as \bold{distri.dat} file. Column names are extracted from the header of the file, which is omited in the output, spaces in the 
column names ar replaced by underscore. The return value is containing as many columns as the distri.dat file and as many rows as 
measurement points in the \bold{mapping.tmp} file. The order is the same as in teh map file.
}

\value{
	The return value is data frame containing as many columns as the distri.dat file and as many rows as measuremen point in
    the map.  wavelength observation per row. The typical columns are e.g. as follows:
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
\seealso{readNKmap, readNK}
\examples{
## Do not run
# data  <- readNKdistri("/tmp")


}
\keyword{misc}
