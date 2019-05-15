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

############################################
# gives back part of the data corresponding to particles within areas(mm^2) with density 
# lower than low and bigger than high limits
# Darwin internal function
sieveDFXdata <- function(data, low=0, high=1000)
{
# calculate density map - do not rely on maps available
   map1 <- dmap(data)
# estimate fields with one particle only for both maps
   ID1 <- which(map1$zden>= low & map1$zden<=high)

# estimate X,Y coordinates of field and merge to CO data
   X <- (ID1%% 152)
   Y <- ((ID1 %/% 152) + 1)
   CO <- cbind(X, Y)
# estimate particles in given fields
   DT1 <- trunc(data[,4:5])+1
#   ID1 <- rep(NA, length(CO[,1]))

# wrong approach - adds crossed areas as well
#   ID2 <- which(DT1$X %in% CO[,1] & DT1$Y %in% CO[,2])
# may be better this one, but slower   
   ID2 <- c()
   for(i in 1:length(CO[,1]))
   {
   ID2 <- c(ID2, which(DT1$X ==CO[i,1] & DT1$Y == CO[i,2]))
   }
# get data for particles in DT1 and DT2 respectively, DTdiff contains offset
   ret <- data[ID2,]
   return(ret)
}
