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

###################################################
# bubble plot CDU
###################################################
plotCDU <- function (x,y,CD, col=FALSE, scale=1, nwhite=TRUE, CATS=NA, xlim=NULL, ylim=NULL)
{
   XR <- trunc(log10(diff(range(x))))
   mantissa <- diff(range(x))/10^XR
   if (mantissa < 1.52) XR <- XR-1
   if (is.null(xlim)| is.null(ylim))
   {
# identify coordinates CATS or mask centered
      if (is.na(CATS)) {if (all(x>0)&all(y>0)) CATS <- TRUE else CATS <- FALSE }
      if (CATS){
         xlim <- c(0,15.2*10^XR)
         ylim <- c(0,15.2*10^XR)
      } else{
         xlim <- c(-7.6*10^XR,7.6*10^XR)
         ylim <- c(-7.6*10^XR,7.6*10^XR)
      }
   }
# create plot window
   plot(0, xlim=xlim, ylim=ylim, xlab="x", ylab="y", type="n", xaxs="i", yaxs="i", axes=FALSE)
   box()
   if(CATS){
      axis(1, at=seq(0, 15.2*10^XR, 1.5*10^XR))
      axis(2, at=seq(0, 15.2*10^XR, 1.5*10^XR))
   }else{
      axis(1, at=seq(-7.5*10^XR, 7.5*10^XR, 1.5*10^XR))
      axis(2, at=seq(-7.5*10^XR, 7.5*10^XR, 1.5*10^XR))
   }

# check if colors defined
   l_x <- length (x)
   options(warn=-1)
   if (col==FALSE) col <- rep("royalblue", l_x) else {
      if (length(col)==1) col <- rep(col, l_x)
   }
   options(warn=0)

#estimate 3sigma
   CDU3s <-round(3*sd(CD),1)
   CDmean <- round(mean(CD),1)
   CDUrange <- round(diff(range(CD)),1)
# change scale
   size <- CD - mean(CD)
   size <- size*scale*10^(XR-1)

# change color of negative bobles
  if (nwhite) col[which(size < 0)] <- "white"
  size <- abs(size)
#plot bubles
   symbols(x, y, circles=size, bg=col, add=TRUE, inches=FALSE)

   symbols(xlim[1]+0.05*diff(range(xlim)), ylim[2]-0.05*diff(range(ylim)), circles=trunc(CDU3s,0)*scale*10^(XR-1), bg=col, add=TRUE, inches=FALSE)
   text(xlim[1]+0.05*diff(range(xlim)), ylim[2]-0.05*diff(range(ylim)), 
      paste(" - ",trunc(CDU3s,0),"nm"), pos=4)

   mtext( paste("CDU 3s=",CDU3s, " ; range=", CDUrange, " ; ", "CDmean=",CDmean ),side=3, line=0.3, cex=1)
}

