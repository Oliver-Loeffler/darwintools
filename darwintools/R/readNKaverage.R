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

#-------------------------------------------
# reads in output.x files from given folder and calculates average, min, max for
# reflectivity and tranmission
#-------------------------------------------
readNKaverage <- function(folder, fe=NULL)
{
   Files <- list.files(folder, pattern="output.", full.names=TRUE)
   Nr <- length(Files)
   if(!length(Files)==0) 
   {
      Files <- paste(folder, ,"/output.", fe, sep="")
      Nr <- length(Files)
      if(Nr>0)
      {
#      setwd(folder)
         WL <- c()
         R <- c()
         T <- c()
         for(i in Files)
         {
#           print(i)
            A <- readNKspectra(i)
            WL <- cbind(WL,A$WL)
            R <- cbind(R,A$R)
            T <- cbind(T,A$T)
         }
         SL <- length(R[,1])
         Rmean <- unlist(sapply(1:SL, function(x) {mean(R[x,])} ))
         Rmin <- unlist(sapply(1:SL, function(x) {min(R[x,])} ))
         Rmax <- unlist(sapply(1:SL, function(x) {max(R[x,])} ))
         Tmean <- unlist(sapply(1:SL, function(x) {mean(T[x,])} ))
         Tmin <- unlist(sapply(1:SL, function(x) {min(T[x,])} ))
         Tmax <- unlist(sapply(1:SL, function(x) {max(T[x,])} ))
         data <- data.frame(cbind(WL[,1], Rmean, Rmin, Rmax, Tmean, Tmin, Tmax))
         names(data) <- c("WL", "Rmean", "Rmin", "Rmax", "Tmean", "Tmin", "Tmax")
      }
      else{
         print("Something went wrong... there are no files here...")
         data <- c()
      }
   }
   return(data)
} # end of nk.average function 
