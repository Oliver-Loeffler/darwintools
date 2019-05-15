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

#---------------------------------------------------------------
# plot additional defect into existing plot window created usually by viewplot function
# can be used as viewplot as well setting add=FALSE
# color, symbol type and size can be selected
#---------------------------------------------------------------
 addDFXplot <- function(data, title="", xlim=NULL, ylim=NULL, pch=19, cex=0.3,  col=NULL, add=TRUE, CATS=TRUE, border="mask")
 {
    if(is.null(xlim)) 
    {
       if(CATS==TRUE) 
       {
          xlim=c(0, 152)
          ylim=c(0, 152)       
       } else {
          xlim=c(-76,76)
          ylim=c(-76,76)       
       } 
   }
   if(length(col)==0) col <- "blue"
   if(add) 
   {
      if(CATS) points(data$X, data$Y, col=col, pch=pch, 
         cex=cex ) else points(data$X-76,data$Y-76, col=col, pch=pch, cex=cex ) 
#      points(data$X-76,data$Y-76, col=col, pch=pch, cex=cex ) 
   } else {
      if (CATS) 
      {
         plot(data$X, data$Y, xlab="X [mm]", ylab="Y [mm]",
            main=title, col=col, xlim=xlim+76, ylim=ylim+76, pch=pch, cex=cex, asp=1, xaxs="i", yaxs="i") 
         if ("ins" %in% border) rect(6, 6, 146,146, border="red", asp=1)
         if ("mask" %in% border) rect(0,0, 152,152, border="black", asp=1)
       } else {
          plot(data$X-76,data$Y-76, xlab="X [mm]", ylab="Y [mm]", main=title, 
             col=col, xlim=xlim, ylim=ylim, pch=pch, cex=cex, asp=1, xaxs="i", yaxs="i")
          if ("ins" %in% border) rect(-70,-70, 70,70, border="red", asp=1)
          if ("mask" %in% border) rect(-76,-76, 76,76, border="black", asp=1)
       } 
   }
}
