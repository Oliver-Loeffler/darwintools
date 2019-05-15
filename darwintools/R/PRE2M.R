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

#---------------------------
# calculation of particle removal efficiency map from 2 scans - coat- after particler application
# and post- post clean. This evaluation assumes no ( negligible number of) defects 
# before particle application and/or only removable defects on the blank.
# In other cases PRE3 function needs to be applied.
#---------------------------
PRE2M <- function(coat, post, smooth=TRUE)
{
   coatmap <- dmap(coat)
   postmap <- dmap(post)
   PRE2map <- postmap
   PRE2map$zden <- coatmap$zden - postmap$zden
   PRE2map$zden[which(PRE2map$zden < 0)] <- 0
   PRE2map$zden <- (PRE2map$zden)/coatmap$zden * 100
   PRE2map$zden[which(is.na(PRE2map$zden))] <- 0
   PRE2map$zden[, c(1:6, 148:152)] <- 0
   PRE2map$zden[c(1:6, 148:152), ] <- 0
   if (smooth) PRE2map <- dsmooth(PRE2map)
   return(PRE2map)
}   
