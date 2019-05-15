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
# calculates density map per mm^2 from raw data obtained from DFX file by readdata function
# return density map which can be smooth further
dmap <- function(data, grid=1)
{
   mcol <- trunc(152/grid)+1
   map <- matrix(data=0, nrow=mcol, ncol=mcol, byrow=TRUE)
   center <- mcol%%2
   xver <- trunc(data$X%/%grid)+1
   yver <- trunc(data$Y%/%grid)+1
#   ver <- cbind(xver, yver)

   xords <- seq(0, 152+grid, grid) 
   yords <- seq(0, 152+grid, grid) 

   for (i in 1:length(data[,1]))
   {
      map[xver[i], yver[i]] <- map[xver[i], yver[i]]+1 
   }
   msp <- map / grid*grid
#   apply(ver, 1, function(x, map){ map[x[1], x[2]] <- map[x[1], x[2]]+1}, map=map)

   res <- list(xords, yords, map)
   names(res) <- c("xords", "yords", "zden")
   return(res)   
}
