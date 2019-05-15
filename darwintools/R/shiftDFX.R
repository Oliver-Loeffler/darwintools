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

#-----------------------------------------
# shiftDFX()
# shifts DFX or other defect inspection data by "shift" in micron
#-----------------------------------------

shiftDFX <- function(data, shift)
{
   if(!all(c("X","Y") %in% names(data))) stop("rotateDFX() : Missing X and Y columns in data set!")
      ret <- data
      ret$X <- ret$X + shift[1]
      ret$Y <- ret$Y + shift[2]
      return(ret)
}
