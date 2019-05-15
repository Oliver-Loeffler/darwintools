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
# calculation of particle removal efficiency map from 3 scans - pre - before particle application, 
# coat- after particler application
# and post- post clean. This evaluation assumes defect in pre scan are not removable 
# and onlz removable defects are added in coat scan 
# when pre scan is not avaiable, blank needs to be assumed to be perfect and PRE2map nees to be used isntead.
#---------------------------
PRE3M <- function(pre, coat, post, smooth=TRUE)
{
   premap <- dmap(pre)
   coatmap <- dmap(coat)
   postmap <- dmap(post)
   PRE3map <- postmap
   PRE3map$zden <- coatmap$zden - postmap$zden
   PRE3map$zden[which(PRE3map$zden < 0)] <- 0
   PRE3map$zden <- (PRE3map$zden)/(coatmap$zden - premap$zden) * 100
   PRE3map$zden[which(is.na(PRE3map$zden))] <- 0
   PRE3map$zden[, c(1:6, 148:152)] <- 0
   PRE3map$zden[c(1:6, 148:152), ] <- 0
   if (smooth) PRE3map <- dsmooth(PRE3map)
   return(PRE3map)
}   

