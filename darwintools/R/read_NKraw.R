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

# read raw data of the N and K reflectometer 
# 
readNKraw <- function(FName)
{
# read header data
   input <- file(FName,"r")
   header <- readLines(input, 20)
   close (input)
   skip <- which(header == "-----------------------  ")
   skipByte <- sum(nchar(header[1:skip]))+skip
   header <- header[1:skip] 
   channelsID <- which(substr(header,1,3) == "WL:")
   channels <- substr (header[channelsID],4, nchar(header[channelsID]))
   channels <- as.numeric(unlist(strsplit(channels, ",")))
   WL <- seq(from= channels[2], to=channels[1], by=-channels[3])

   input <- file(FName,"rb")
   fill <- readBin(input, raw(), skipByte)
   sp_data <- readBin(input, raw(), length(WL)*4)
   close(input)
   Ref <- readBin(sp_data, double(), length(WL), signed=TRUE, size=4)

   data <- data.frame(WL=WL, R=Ref) 
   ret <- list(header=header, data=data)
   return (ret)
} # end of readNKraw function

