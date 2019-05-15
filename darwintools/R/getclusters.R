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

# split criteria matrix into clusters 053
# CR matrix contains NA when distance between column- and row defect bigger than tolerance radius. when smaller, the CC contains distance
# defect IDs are stored as column and row names
# return all clusters including column and row names as items in list
 getClusters <- function(cr)
 {
# make copy of matrix
    criteria <- cr
# check if cr matrix has reasonable size 
    if(any(dim(cr))==0)
    {
       ClusterList <- list()
       return(ClusterList)
       break()
    }
# create matrix with 1 at position of candidate  or 0 when no relation possible, storage for estimated Clusters and loop counter
    crtemp <- cr 
    crtemp <- (crtemp+1) / (crtemp+1)
    crtemp[which(is.na(crtemp))] <- 0
# remove defects without pair candidate (columns and rows)
    unit_vector1 <- rep(1, dim(crtemp)[1])
    unit_vector2 <- rep(1, dim(crtemp)[2])
    col1 <- as.vector(unit_vector1 %*% crtemp)
    row1 <- as.vector(crtemp %*% unit_vector2)
    rowremove <- which(row1==0)
    colremove <- which(col1==0)
#    if (length(colremove)>0) cr <- cr[, -colremove]
    if (length(colremove)>0) cr <- matrix(cr[, -colremove], ncol= ncol(crtemp)-length(colremove) ,  nrow=nrow(crtemp))

    if (length(rowremove)>0 & dim(cr)[1]>0) cr <- matrix(cr[-rowremove, ], ncol=ncol(crtemp)-length(colremove) , nrow=nrow(crtemp)-length(rowremove))
# and change to matrix, just in case
#    cr <- as.matrix(cr)
    if (length(rowremove)>0) rownames(cr) <- rownames(criteria)[-rowremove] else rownames(cr) <- rownames(criteria)
    if (length(colremove)>0) colnames(cr) <- colnames(criteria)[-colremove] else colnames(cr) <- colnames(criteria)

# and refresh cr temp table
       crtemp <- (cr+1) / (cr+1)
       crtemp[which(is.na(crtemp))] <- 0
# create storage for estimated Clusters and loop counter
       ClusterList <- list()
       Counter <- 0
# add last row and column to both tables containing  0 and NA respectively to avoid collapse of matrix to vector in last loop
       crtemp <- cbind(crtemp, 0)
       crtemp <- rbind(crtemp, 0)
       cr <- cbind(cr, NA)
       cr <- rbind(cr, NA)

# loop over clusters - as number unknown, check size of crtemp instead
#       while(class(crtemp)=="matrix" & dim(crtemp)[2] > 0)
   
       while(class(crtemp)=="matrix")
       {
#       while(all(dim(crtemp)>1) & Counter<36)
# increase counter
          Counter <- Counter+1 
#          cat("counter: ", Counter,"\n")
# which rows contain candidate for pair defect with defect in first column?
#          rowi <- crtemp %*% crtemp[1,]
          rowi <- crtemp %*% c(1, rep(0,dim(crtemp)[2]-1))  
          csum <- c(0,1)
# repeat following procedure till no new potential pairs are addded (csum[1]== csum[2])
          while(csum[1] <csum[2])
          {
# which columns contain candidates for pair with defects x>0 in first row first column defect?
             coli <- t(rowi) %*% crtemp  
# what about rows defects interfering with previously estimated column defects
             rowi <- crtemp %*%  t(coli)
             Irows <- which(rowi>0) # find rows with some candidates
             Icols <- which(coli>0) # find columns with some candidates
 # count number of defect intreractions pre and post
             csum[1] <- csum[2] # store number of previous candidates in csum[1]
             csum[2] <- length(Irows)+length(Icols)# store number of curent candidates in csum[2]
          }
# generate matrix with same dimensions, content and names as cluster in cr
          ClusterI <- matrix(cr[Irows, Icols], ncol=length(Icols), nrow=length(Irows))
          colnames(ClusterI) <- colnames(cr)[Icols]
          rownames(ClusterI) <- rownames(cr)[Irows]
# store Cluster in Cluster list
          ClusterList[[Counter]] <- ClusterI
# remove identified cluster from cr and crtemp 
          cr <- cr[-Irows, -Icols]
          crtemp <- crtemp[-Irows, -Icols]
       } # end of cluster loop
# name Clusters
       names(ClusterList) <- 1:length(ClusterList)
       return(ClusterList)
 }# end of get clusters function
