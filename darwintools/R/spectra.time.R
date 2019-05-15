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

# returns time vector readed from svd file
spectra.time <- function(FName)
{
   width <- 2530
   channels <- 1201
   input <- file(FName,"rb")
   fill <- readBin(input, integer(),1)
   data.size <- readBin(input, integer(),signed=FALSE,1)
   header.offset <- readBin(input, integer(),2)
   header.frames <- readBin(input, integer(),2)
   header.frames[2] <- (data.size-header.offset[1])/width
   spectra <- header.frames[2]%/%header.frames[1]
   close (input)
# pull information about time step
   time <- c()
   tframe <- c()
   input <- file(FName,"rb")
   fill <- readBin(input, raw(), header.offset[1])
   for (i in 1:header.frames[2])
   {
      fill <- readBin(input, raw(), 40)
      time[i] <- readBin(input, integer(),1)
      tframe[i] <- readBin(input, integer(),1)
      fill <- readBin(input, raw(), width-48)
    }
   close (input)
   out <- data.frame(time/1000,tframe)
   names(out) <- c("time","frame")
   return(out)
}

