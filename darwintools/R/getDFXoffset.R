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
# calculates x-y offset between two data sets (data2-data1) in mm
# calculation based on map fields (1 mm^2) containing 1 particle only
# returns data in DFX coordinates - X,Y range 0-152mm
#----------------------------------------------------------------
getDFXoffset <-  function (data1, data2)
{
# calculate maps - do not rely on maps available
   map1 <- dmap(data1)
   map2 <- dmap(data2)
# estimate fields with one particle only for both maps
   ID1 <- which(map1$zden==1)
   ID2 <- which(map2$zden==1)
# which fiels are identical in both maps
   ID12 <- ID1[which(ID1 %in% ID2 == TRUE)]
   ID21 <- ID2[which(ID2 %in% ID1 == TRUE)]
   if(length(ID12)<1) stop("No matching defects found. Offset estimation not possible.") 
   if(length(ID12)<10) warning ("Warning: number of usefull data points is ",length(ID12))

# estimate X,Y coordinates of field and merge to CO data
   X <- (ID12%% 153)
   Y <- ((ID12 %/% 153) + 1)
   CO <- cbind(X, Y)
# estimate particles in given fields
   DT1 <- trunc(data.frame(X=data1$X, Y=data1$Y)) + 1
   DT2 <- trunc(data.frame(X=data2$X, Y=data2$Y)) + 1
   ID1 <- rep(NA, length(CO[,1]))
   ID2 <- rep(NA, length(CO[,1]))
   for(i in 1:length(CO[,1]))
   {
# here ID1 and ID2 get indexes of particles in data sets
      ID1[i] <- which( DT1$X==CO[i,1] & DT1$Y==CO[i,2])
      ID2[i] <- which( DT2$X==CO[i,1] & DT2$Y==CO[i,2])
   }
# get data for particles in DT1 and DT2 respectively, DTdiff contains offset
   DT1 <- data.frame(X=data1$X[ID1], Y=data1$Y[ID1])
   DT2 <- data.frame(X=data2$X[ID2], Y=data2$Y[ID2])
   DTdiff <- (DT2-DT1)*1000
   names(DTdiff) <- c("Xd","Yd")

# just for information plot X,Y, and offset (multiplied by 100)
#   plot(0, type="n", xlim=c(-76, 76), ylim=c(-76,76), xaxs="i", yaxs="i")
#   arrows(DT1$X-76, DT1$Y-76, DT1$X-76+DTdiff$X*100, DT1$Y-76+DTdiff$Y*100, col="blue",
#      length=0.04)

# estimate rotation of the data fields - for each point separatelly
   Angle1 <- Arg(complex(real=DT1$X-76, imaginary=DT1$Y-76))
   Angle2 <- Arg(complex(real=DT2$X-76, imaginary=DT2$Y-76))
   Angle <- Angle2-Angle1

# return X,Y, of forst dat set, delta X and delta Y to second data set
   ret <- list(X=DT1$X, Y=DT1$Y, Xd=DTdiff$Xd, Yd=DTdiff$Yd, offset=apply(DTdiff, 2, median), rotation=mean(Angle))
   return(ret)
   cat(paste("X offset = ", mean(DTdiff$Xd)-1000," micron\n",sep=""))
   cat(paste("Y offset = ", mean(DTdiff$Yd)*1000," micron\n",sep=""))
   cat(paste("rotation = ",mean(Angle)," degree\n",sep=""))
}
