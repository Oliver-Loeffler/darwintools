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

##############################################
# plot "spectra" data frame
# whether spectra or time trend is decided on hand of parameters given
plot_spectra <- function(SP, lambda=NULL, time=NULL, sp=1, col="black", xlab = NULL, ylab = NULL,
   xlim = NULL, ylim=NULL, zlim=NULL, tlim=NULL, llim=NULL, add=FALSE)
{       
   if(is.null(lambda)&is.null(time))
   {
      Pdata <- list(x=SP$t[,sp], y= SP$WL, z=as.matrix(SP$SP[,,sp]))
      if (!is.null(tlim)) xlim <- tlim
      if (!is.null(tlim)) xlim <- tlim
      if (!is.null(llim)) ylim <- llim
      if (is.null(xlim)) xlim <- range(Pdata$x)
      if (is.null(ylim)) ylim <- range(Pdata$y)
      if(is.null(xlab)) xlab <- "time[s]"
      if(is.null(ylab)) ylab <- expression(bold(lambda))
      par(omi=c(0,0,0,0), mai=c(0.5,0.5, 0.07, 0.05), mgp=c(1.3,0.4,0))
      par(font.axis=3, font.lab=2, cex.axis=0.8)
      surface(Pdata, type="I", xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, zlim=zlim)
   } else {
# plot spectra at given lambda
      if(!is.null(lambda)) 
      {
         if (!is.null(tlim)) xlim <- tlim
         Rlambda <- trunc(lambda*2)/2
         Pdata <- SP$SP[,which(SP$WL %in% Rlambda),sp]
         if(is.null(xlim)) xlim <- range(SP$t[,sp])     
         if(is.null(ylim)) ylim <- range(Pdata)
         if(is.null(xlab)) xlab <- "time[s]"
         if(is.null(ylab)) ylab <- "Intensity"
         if (! add) 
         {
            par(omi=c(0,0,0,0), mai=c(0.5,0.5, 0.07, 0.05), mgp=c(1.3,0.4,0))
            par(font.axis=3, font.lab=2, cex.axis=0.8)
            plot(0,0, type="n", xlim, ylim, xlab=xlab, ylab=ylab)
         }
         if(length(col)<length(Rlambda)) col <- rep(col, length(Rlambda)%/%length(col)+1 )
         if (length(Rlambda)>1){ for (i in 1:length(Rlambda))
               lines(SP$t[,sp],Pdata[,i], col=col[i])} else lines(SP$t[,sp],Pdata, col=col)
      } else {
# plot spectra at given time
         if(!is.null(time)) 
         {
            Rtime <- c()
            for (k in 1:length(time))
            {
               Rtime[k] <- SP$t[which.min(abs(time[k]-SP$t[,sp])),sp]
            }
 #print(length(Rtime))
            Pdata <- SP$SP[which(SP$t[,sp] %in% Rtime),,sp]
            if (!is.null(llim)) xlim <- llim
            if(is.null(xlim)) xlim <- range(SP$WL)     
            if(is.null(ylim)) ylim <- range(Pdata)
            if(is.null(xlab)) xlab <- expression(bold(paste(lambda,"[nm]",sep="")))
            if(is.null(ylab)) ylab <- "Intensity"
            if(! add)
            {
               par(omi=c(0,0,0,0), mai=c(0.5,0.5, 0.07, 0.05), mgp=c(1.3,0.4,0))
               par(font.axis=3, font.lab=2, cex.axis=0.8)
               plot(0,0, type="n", xlim, ylim=range(Pdata), xlab=xlab, ylab=ylab)
            }
            if (length(Rtime)>1) for (i in 1:length(Rtime)){
               lines(SP$WL,Pdata[i,], col=i)} else lines(SP$WL,Pdata, col=col)
         }  # end if ...

      } # end else
   } # end else 

}

