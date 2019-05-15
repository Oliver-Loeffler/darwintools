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
# plot and plot radial density difference plot
# Darwin internal function
radialdifd <- function(data1,data2, title, bw=0.2)
{
# replacement for previous part using radd function
   rdens <- radd(data1, bw) 
   rdens2 <- radd(data2, bw) 
   rddens <- rdens
   rddens$y <- rdens$y-rdens2$y 
   plot(rddens, xlab="r [mm]", ylab=expression(bold(paste("Density [",n/mm^2,"]"))), main= title, 
      xlim=c(0, 100))
   abline(v=70, col="red")
}

