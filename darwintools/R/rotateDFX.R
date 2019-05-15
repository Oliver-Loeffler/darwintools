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

#####
#rotateDFX(data, angle)
rotateDFX <- function(data, angle, Bsize=152.0)
{
   if(!all(c("X","Y") %in% names(data))) stop("rotateDFX() : Missing X and Y columns in data set!")
   if(angle==0)print("use your brain first, then computer!!!")
   if(!angle %in% c(0,90,180,270)) print("angle parameter not accepted. try again") else {
# angle is 90, 180 or 270 CW 
      ret <- data
      if(angle==180)
      {
         ret$X <- Bsize - data$X
         ret$Y <- Bsize - data$Y
      }
      if(angle==90)
      {
         ret$X <- data$Y
         ret$Y <- Bsize - data$X
      }
      if(angle==270)
      {
         ret$X <- Bsize - data$Y
         ret$Y <- data$X
      }
      return(ret)
   }

}
