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

###########################################
# calculates x-y map for median particle size as as e.g. dmap does for density of particles
# Darwin internal function
smap <- function(data)
{ 
   print("calculation particle size map ...")
   map <- matrix(data=0, nrow=28, ncol=28, byrow=TRUE)

   xver <- floor((data[,4]-1)/5)
   yver <- floor((data[,5]-1)/5)
   xords <- seq(from=-68, to=68, by=5)
   yords <- seq(from=-68, to=68, by=5)
   for(i in 1:28)
   {
      for(j in 1:28)
      {
         map[i,j]<- median(data$Size[which(xver==i & yver==j)])
      }
   } 
   resu <- list(xords, yords, map)
   names(resu) <- c("xords", "yords", "zden")
   print("done")
   return(resu)   
}
