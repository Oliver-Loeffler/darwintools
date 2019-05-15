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

#--------------------------------------
# estimation of pairs within cluster
#--------------------------------------
 resolveClusterSORT <- function(CL)
 {
# create matrix for defect pairs 
    pairs <- matrix(NA, ncol=2, nrow=max(dim(CL)))
    Mdim <- dim(CL)
# more rows than columns expected, therwise trasnpose CL matrix and finally flip columns in pairs matrix
    if(Mdim[2] > Mdim[1]) CL <- t(CL)
    for(i in 1: min(Mdim)) # loop over columns in cluster matrix CL
    {
       if(all(is.na(CL[,i]))) next()
       pairs[i,1] <- colnames(CL)[i]
       pairs[i,2] <- rownames(CL)[which.min(CL[,i])]
       CL[,i] <- NA
       CL[which(rownames(CL)==pairs[i,2]),] <- NA
    }
    if(Mdim[2] > Mdim[1])
    {
       Tpairs <- pairs[,1]
       pairs[,1] <- pairs[,2]
       pairs[,2] <- Tpairs
    } 
# remove empty pairs
    pairs <- pairs[which(!is.na(pairs[,1])),]
    pairs <- matrix(as.numeric(pairs), ncol=2, nrow=length(as.numeric(pairs))%/%2)
    colnames(pairs) <- c("data1", "data2")
    return(pairs)
 } # end of resolve cluster
