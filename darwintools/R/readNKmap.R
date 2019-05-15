#
#   Copyright (C) 2019  Pavel Nesladek
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program; if not, write to the Free Software Foundation, Inc.,
#   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#

#-------------------------------------------------------------------
# read mapping.tmp file containing n&k measurement point placement
#-------------------------------------------------------------------
readNKmap <- function(folder)
{
   styles <- data.frame(style=c(" IRR"," SQUARE"," XY", " Polar"), offset=c(0, 7, 7, 7))
   if(substr(folder, nchar(folder), nchar(folder))=="/") Fname <- paste(folder,"mapping.tmp", sep="") else Fname <- paste(folder,"/mapping.tmp", sep="")
   Inp <- file(Fname, "r")
   tmp <- readLines(Inp, n=30)
   close(Inp)
   style <- grep("Style",tmp[])[1]
   style <- unlist(strsplit(tmp[style], ":"))[2]
   stripNr <- grep("(x0,y0,a0,b0,x1,y1,x2,y2)",tmp[])[1]
   if (style %in% styles$style) strip <- stripNr+styles$offset[which(styles$style==style)] else stop("maping file style not known, can not read in map !")
#   if(style==" IRR") strip <- stripNr
#   if(style==" SQUARE") strip <- stripNr+7
#   if(style==" XY") strip <- stripNr+7
   data <- read.table(Fname ,  sep=",", skip=strip, fill=TRUE)
   names(data) <- c("X", "Y")
   return(data)
}

