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

#----------------------------------------------------------------
# read single N&K spectra out of given file, gives back  columns "WL(nm)","Tsexp", "Rsexp"
#----------------------------------------------------------------

readNKspectra <- function(FName)
{
# defline function takes each data line apart into vector of numbers
    defline <- function(text) 
    {
       DP <- unlist(strsplit(text, "\t"))
       D <- unlist(strsplit(DP[1], " "))
       D <- c(D[nchar(D)>0], DP[2])
       options(warn=-1)
       Dcounts <- as.numeric(D)
       options(warn=0)
       return(Dcounts)
    }

# figure out wavelength vector
   Inp <- file(FName, "r")
   tmp <- readLines(Inp)
   close(Inp)
   nskip <- which(substr(tmp[],1,6) == "WL(nm)")+1
# read and identify header items
   Sheader <- gsub(pattern="\t" , replacement=" ", tmp[nskip-1])
   Items <- unlist(strsplit(Sheader, split=c(" ")))
   Items <- Items[which(!Items=="")]
#   Inp <- file(FName, "r")
#   tmp <- readLines(Inp, n= nskip)
#   Input <- readLines(Inp)
#   close(Inp)
   Input <- tmp[seq(-1,-nskip, -1) ]

# find out data column widths
   Pos1 <- gregexpr(" ", Input[1])
   Pos12 <- gregexpr(" ", Input[2]) 
   if (all(Pos1[[1]]==Pos12[[1]]))  # is fast version possible /all lines have identical length
   {
      Pos2 <- gregexpr("\t", Input[1])
      Pos <-  c(Pos1[[1]], Pos2[[1]])
      Pos <- c(1,Pos[which(diff(Pos)>1)])
      Begin <- Pos[1:length(Pos)-1]
      End <- Pos[2:length(Pos)]
      data <- matrix(NA, ncol=length(Items), nrow=length(Input))
      for(i in 1:length(Items)) data[,i] <- as.numeric(substr(Input,Begin[i], End[i]))
      data <- data.frame(data)
   } else # line shave different alignment
   {
      A <- t(sapply(Input, defline))
      data <- data.frame(A)
   }

   names(data) <- Items
   return(data)

}



