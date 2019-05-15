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
# smooth density map from density map calculated by dmap function
# grid is 3x3 mm isntead 1x1 mm
dsmooth <- function(data)
{
   resu <- data
   for (i in 2:(length(data$xords)-2))
   {
      for (j in 2:(length(data$yords)-2))
      {
         resu$zden[i,j]<- (data$zden[i-1,j-1]+data$zden[i,j-1]+
            data$zden[i+1,j-1]+ data$zden[i-1,j]+ data$zden[i,j]+
            data$zden[i+1,j]+ data$zden[i-1,j+1]+ data$zden[i,j+1]+
            data$zden[i+1,j+1])/9
      }
   }
   return(resu)
}

