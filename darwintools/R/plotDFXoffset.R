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

#----------------------------------------------------
# plot offset as "registration plot" , incoming data are offset data calculated 
# by function getDFXoffset
#----------------------------------------------------
plotDFXoffset <- function(offset, scale=100, col="blue", add=FALSE, index=FALSE)
{
   if(!add) plot(0, type="n", xlim=c(-76, 76), ylim=c(-76,76), xaxs="i", yaxs="i", xlab="X [mm]", ylab="Y [mm]")
   arrows(offset$X-76, offset$Y-76, offset$X-76+offset$Xd/1000*scale,
      offset$Y-76+offset$Yd/1000*scale, col=col, length=0.04)
  mtext(paste("scale=", scale,"x"), side=3, line=2, outer=FALSE, col=col)
  mtext(paste("X offset=", round(offset$offset[1],1), "um, Y offset=", round(offset$offset[2], 1),
     "um, rotation=", round(offset$rot*180/pi, 3), "deg CCW"), side=3, line=1, outer=FALSE, col=col)
   if (index) text(offset$X-76, offset$Y-76, 1:length(offset$X), cex=0.7)
}
