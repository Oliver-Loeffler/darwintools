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
# read all N&K spectra out of given file, gives back  columns "WL(nm)","Tsexp", "Rsexp"
#---------------------------------------------------------------
readNK <- function(FDir)
{
   Flist <- dir(FDir,pattern="output.")
# create list of output.* files with increasing index
   OutEnds <- substr(Flist, 8, nchar(Flist))
   Pt <- strsplit(OutEnds, "_")
   PtIndex <- order(as.numeric(lapply(Pt, "[", 1)))
   Flist <- file.path(FDir, Flist[PtIndex])

 #  Nr <- length(Flist)
 #  if (substr(FDir, nchar(FDir),nchar(FDir))=="/") 
 #    Flist <- filepaste(FDir,"output.", 1:Nr, sep="") else  Flist <- paste(FDir,"/output.", 1:Nr, sep="")
# figure out wavelength vector
   Inp <- file(Flist[1], "r")
   tmp <- readLines(Inp, n=35)
   close(Inp)
   nskip <- which(substr(tmp[],1,6) == "WL(nm)")+1
   Sheader <- gsub(pattern="\t" , replacement=" ", tmp[nskip-1])
   Items <- unlist(strsplit(Sheader, split=c(" ")))
   Items <- Items[which(!Items=="")]
   ID <- which(Items %in% c("WL(nm)","Tsexp", "Tpexp","Rsexp", "Rpexp"))
   WL <- read.table(Flist[1], skip=nskip, as.is=TRUE)[,1]
   npoints <- length(Flist)
# create arrays for output data for each Rs, Ts, Rp, Tp - all experimental
   Rs <- array(data=NA, dim=c(length(WL), npoints), dimnames=c("source","WL","Point"))
   Ts <- array(data=NA, dim=c(length(WL), npoints), dimnames=c("source","WL","Point"))
   Rp <- array(data=NA, dim=c(length(WL), npoints), dimnames=c("source","WL","Point"))
   Tp <- array(data=NA, dim=c(length(WL), npoints), dimnames=c("source","WL","Point"))

   for(i in 1:length(Flist))
   {
      A <- readNKspectra(Flist[i])
# stare Rsexp to 1, Tsexp 2, Rpexp,3 and Tpexp 4 
      Rs[,i] <- A$Rsexp
      Ts[,i] <- A$Tsexp
      Rp[,i] <- A$Rpexp
      Tp[,i] <- A$Tpexp
   }

# read map
  map <- readNKmap(FDir)

# complete data for export
   ret <- list(WL=WL, Rs=Rs, Ts=Ts, Rp=Rp, Tp=Tp, map=map)
   return(ret)
}
