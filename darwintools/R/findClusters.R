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

#--------------------------------------------------
# identification of clusters in one inspection reprt either DFX, or KLA / Nu Flare
#--------------------------------------------------
 findClusters <- function(data, dis=200)
 {
    criteria <- pairCriteriaTable(data, data, dis=dis, crit="distance")
    clusters  <- getClusters(criteria)
    singleID <- which(lapply(clusters, function(x) {dim(x)[1]}) ==1)
    if(length(singleID)>0) clusters <- clusters[-singleID]
    DefectID <- lapply(clusters, function(x) {as.numeric(colnames(x))})
    Dlist <- lapply(DefectID, function(x){data[x,]})
    return(Dlist)
 }
