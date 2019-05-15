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

#------------------------------------
# calculation of criteria matrix - outer product of defect positions between two scans
# - distance between any two combinations of defects 
#------------------------------------

 pairCriteriaTable <- function(data1, data2, dis=100, crit="distance")
 {   
    XY1 <- data.frame(X=data1$X, Y=data1$Y)
    XY2 <- data.frame(X=data2$X, Y=data2$Y)   
    D2 <- (dis/1000)^2
    dis.matrix <- function(XY1, XY2) { sqrt((XY2$X - XY1[1])^2 + (XY2$Y - XY1[2])^2) }
    distances <- apply(XY1, 1, dis.matrix, XY2 = XY2)
    if(!is.matrix(distances)) distances <- matrix(distances, ncol=dim(data1)[1], nrow=dim(data2)[1] )

    distances[which(distances > dis/1000)] <- NA 
    if (all(is.na(distances))){ # there is at leas one pair existing 
       criteria <- NULL
       return(criteria)
       break()
    }
    if (crit == "size") {
       Sizes1 <- rep(0, dim(data1)[1])
       Sizes2 <- rep(0, dim(data2)[1])
       if ("Size" %in% names(data1)) Sizes1 <- data1$Size
       if ("Size" %in% names(data2)) Sizes2 <- data2$Size
       if ("SizeX" %in% names(data1)) Sizes1 <- sqrt(data1$SizeX^2 + data1$SizeY^2)
       if ("SizeX" %in% names(data2)) Sizes2 <- sqrt(data2$SizeX^2 + data2$SizeY^2)
       criteria <- abs(outer(Sizes2, Sizes1, "-"))
       criteria[which(is.na(distances))] <- NA
    }
    if (crit == "distance") criteria <- distances
#    if (class(criteria)!="matrix") # result is vector or single number
    rownames(criteria) <- 1:dim(data2)[1]
    colnames(criteria) <- 1:dim(data1)[1]
    return(criteria)
 } # end of pairCriteriaTable function
