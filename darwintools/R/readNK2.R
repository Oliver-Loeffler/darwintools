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
readNK2 <- function(FDir)
{
   FRSlist <- dir(FDir,pattern="^rsdata.")
   FRPlist <- dir(FDir,pattern="^rpdata.")
   FTSlist <- dir(FDir,pattern="^tsdata.")
   FTPlist <- dir(FDir,pattern="^tpdata.")

# create list of data.* files with incpreasing index
   NrRS <- length(FRSlist)
   NrRP <- length(FRPlist)
   NrTS <- length(FTSlist)
   NrTP <- length(FTPlist)

   if (substr(FDir, nchar(FDir),nchar(FDir))=="/") 
     FDir <- substr(FDir, 1, nchar(FDir)-1) 
    FRS <- file.path(FDir, paste("rsdata.", 1:NrRS, sep="")) 
    FRP <- file.path(FDir, paste("rpdata.", 1:NrRP, sep="")) 
    FTS <- file.path(FDir, paste("tsdata.", 1:NrTS, sep=""))
    FTP <- file.path(FDir, paste("tpdata.", 1:NrTP, sep=""))
# create resulting R and T intesinty array
   npoints <- max(c(NrRS,NrTS,NrRP,NrTP))
   WL <- seq(1000, 190, -1)
# create arrays for output data for each Rs, Ts, Rp, Tp - all experimental
   Rs <- array(data=NA, dim=c(length(WL), npoints), dimnames=c("source","WL","Point"))
   Ts <- array(data=NA, dim=c(length(WL), npoints), dimnames=c("source","WL","Point"))
   Rp <- array(data=NA, dim=c(length(WL), npoints), dimnames=c("source","WL","Point"))
   Tp <- array(data=NA, dim=c(length(WL), npoints), dimnames=c("source","WL","Point"))
   
   for(i in 1:max(c(NrRS,NrTS,NrRP,NrTP)))
   {
      if(NrRS>=i) {
         RSI <- readNKraw(FRS[i])
         Rs[,i] <- RSI$data$R
      }
      if(NrRP>=i){
         RPI <- readNKraw(FRP[i])
         Rp[,i] <- RPI$data$R
      }
      if(NrTS>=i){
         TSI <- readNKraw(FTS[i])
         Ts[,i] <- TSI$data$R
      }
      if(NrTP>=i){
         TPI <- readNKraw(FTP[i])
         Tp[,i] <- TPI$data$R
      }
   }
# read map
  map <- readNKmap(FDir)

# complete data for export
   ret <- list(WL=WL, Rs=Rs, Ts=Ts, Rp=Rp, Tp=Tp, map=map)
   return(ret)
}
