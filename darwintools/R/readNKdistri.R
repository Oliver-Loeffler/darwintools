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

#----------------------------------------------------------------
# reads distri.dat file in the given directory, assigns propper column names and returns
# data formated as data.frame
#----------------------------------------------------------------

readNKdistri <- function(FDir)
{

# find fully qualified file name 
   Fname <- file.path(FDir, "distri.dat")
   Inp <- file(Fname, "r")
   tmp <- readLines(Inp)
   close(Inp)
   nskip <- which(substr(tmp[],1,14) == "Next-text-line")
   skip <- nskip + as.numeric(unlist(strsplit(tmp[nskip],"="))[2])
# read columns names
   cnames <- sapply(strsplit(tmp[(nskip+1):(skip-1)], " ="), "[",1)
   cnames <- sapply(cnames, function(x){gsub(" ", "_", x)} )
   attributes(cnames) <- NULL
# read values
   data <- read.table(Fname, header=FALSE, sep=" ", skip=skip)
   data <- data[,1:length(cnames)]
   names(data) <- cnames
#   data <- data[,-which(is.na(names(data)))]
   return(data)
}



