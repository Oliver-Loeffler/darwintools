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

# returns header information about the svd file provided
# provides time stamp of teh first spectra also
header <- function(FName)
{
   width <- 2530
   channels <- 1201
   input <- file(FName,"rb")
   fill <- readBin(input, integer(),1)
   data.size <- readBin(input, integer(),signed=FALSE,1)
   header.offset <- readBin(input, integer(),2)
   header.frames <- readBin(input, integer(),2)
   header.frames[2] <- (data.size-header.offset[1])/width
   sources <- header.frames[2]%/% header.frames[1]
   fill <- readBin(input, integer(),1, size=1)
   header.3 <- readChar(input, fill)
   fill <- readBin(input, integer(),1, size=1)
   header.svc_rcp <- readChar(input, fill)
   header.6 <- readBin(input, integer(),1, size=2)
   header.7 <- readBin(input, character(),1)
   fill <- readBin(input, raw(),115)
   header.proces_rcp <- readBin(input, character(),1)
   close (input)
#   input <- file(FName,"rb")
#   fill <- readBin(input, raw(), header.offset[1])
#   header.date <-readChar(input, 21)
#   close (input)

# pull information about time step
   time <- c()
   tframe <- c()
   input <- file(FName,"rb")
   fill <- readBin(input, raw(), header.offset[1])
   for (i in 1:4)
   {
      header.date <-readChar(input, 21)
      fill <- readBin(input, raw(), 19)
      time[i] <- readBin(input, integer(),1)
      tframe[i] <- readBin(input, integer(),1)
      fill <- readBin(input, raw(), width-48)
    }
   close(input)
   if(sources==1) {header.tstep <- (lm(time~tframe)$co[2])/1000 }else {
      header.tstep <- (lm(time[c(1,3)]~tframe[c(1,3)])$co[2])/1000}
   header.tfirst <- time[1]/1000 

   return(list(date=header.date, offset=header.offset, frames=header.frames, sources=sources, 
      size=data.size, svcRecipe=header.svc_rcp, process=header.proces_rcp, tstep=header.tstep,
      tfirst=header.tfirst))
}
