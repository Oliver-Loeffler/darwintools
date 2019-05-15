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

#------------------------------------------------
# subtracts dataset "data2" from "data1" if corresponding point in data2 identified
# within "dis" radius around particle in "data1" dis given in microns
# crit estimate criteria for paier destrimation - eiter "size" or "distance"
# paired is switch for pairwie subtraction and may contain the algorithm according to pairedDFX documentation
#------------------------------------------------

subtractDFX <- function(data1, data2, dis=100, paired="SORT", crit="distance")
{
   if (!paired==FALSE)
   {
      if(paired==TRUE) pairs <- pairedDFX(data1, data2, dis=dis, crit=crit, alg="SORT") else 
         pairs <- pairedDFX(data1, data2, dis=dis, crit=crit, alg=paired)
      if (is.null(pairs) | dim(pairs)[1]==0) res <- data1 else res <- data1[-pairs[,1],]

   } else{
      XY1 <- data.frame(X=data1$X, Y=data1$Y)
      XY2 <- data.frame(X=data2$X, Y=data2$Y)
      D2 <- (dis/1000)^2
      Len <- dim(XY1)[1]
      IDC <- c()
      for(i in 1:Len)
      {
         IDC[i] <- any(((XY2$X-XY1$X[i])^2+ (XY2$Y-XY1$Y[i])^2) < D2)
         if (i%%200==0) print(paste(i,"of", Len ))
      }
      res <- data1[which(!IDC),]
   } # end of non paired evaluation
   return(res)
}
