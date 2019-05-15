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

#--------------------------------------------
# corrects data frame (as readed by function readdata from DFx data file)
# by offset data obtained by function get.offset 
# correction is added, so if get.offset(data1, data2) used, correct.offset(data2, offset)
# provides data comparable with data1
# for now only shift in X, and Y is corrected
#--------------------------------------------
correctDFXoffset <- function(data, offset)
{
   X <- data$X- offset$offset[1]/1000
   Y <- data$Y- offset$offset[2]/1000

#   polalpha <- Arg( complex(real=X-76, imaginary=Y-76))
#   polr <- Mod( complex(real=X-76, imaginary=Y-76))
#   polalpha <- polalpha-offset$rotation

#   X2 <- Re(complex(modulus=polr, argument=polalpha))+76
#   Y2 <- Im(complex(modulus=polr, argument=polalpha))+76

   dataout <- data
#   dataout$X <- X2
#   dataout$Y <- Y2
   dataout$X <- X
   dataout$Y <- Y

   return(dataout)
}
