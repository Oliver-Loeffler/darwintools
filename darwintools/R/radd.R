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
# calculate radial density - 1 data set
# Darwin internal function
radd <- function(data, bw=0.2)
{
   rd <- sqrt((data[,4]-76)^2+(data[,5]-76)^2)
   options(warn=-1)
   rdens <-density(rd, kernel="gaussian", bw=bw, weights=rep(1,length(rd)), from=0, to=100, n=101) 
   options(warn=0)
   rdens$y[which(rdens$x <=70)] <- rdens$y[which(rdens$x <=70)]/(2*pi*rdens$x[which(rdens$x <=70)])
   rdens$y[which(rdens$x >70)] <- rdens$y[which(rdens$x >70)]/(2*pi*rdens$x[which(rdens$x >70)]-8*rdens$x[which(rdens$x >70)]*acos(70/rdens$x[which(rdens$x >70)]))
   rdens$y[which(rdens$x >98)] <- 0
   return(rdens)
}
