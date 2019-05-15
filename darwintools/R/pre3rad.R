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
# plot (+calculate) radial PRE function from 2 data sets - coat+post
# identical as rpre3 - kept for bacward compatibility of creport function
pre3.rad <- function(data1, data2, data3, title="")
{
# calculater radial function for coat and post 
# assumed 1- pre, 2- coat, 3- post
   rdens <- radd(data1) 
   rdens2 <- radd(data2) 
   rdens3 <- radd(data3) 
   rddens <- rdens
   rddens$y <- (rdens2$y - rdens3$y - rdens$y) / (rdens2$y - rdens$y)*100
   plot(rddens, xlab="r [mm]", ylab="PRE 3 [%]", ylim=c(0,100), main= title)
   abline(v=70, col="red")
}
