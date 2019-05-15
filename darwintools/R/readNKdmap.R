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
# reads mapping.tmp and distri.dat files, sorts merge product according to increasing X and Y 
# and return data frame with columns equal to sum of both files
#----------------------------------------------------------------

readNKdmap <- function(FDir)
{
# read distri file
   distri <- readNKdistri(FDir)
# read map file
   map <- readNKmap(FDir)
   Dmap <- cbind(map, distri)
   Dmap <- Dmap[order(Dmap$Y),]
   Dmap <- Dmap[order(Dmap$X),]
   return(Dmap)
}



