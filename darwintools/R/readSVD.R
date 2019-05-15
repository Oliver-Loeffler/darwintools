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

# read svd spectra data  v063
# corrected for source numbers, and variable channel number
# return value is list containing
# wavelength vector WL, time stamp vector TS and spectra array SP etc.
readSVD <- function(FName)
{
# read header data
   EventStamp <- charToRaw("System Event")
   ConfStamp <- charToRaw("Configuration:")
   EndStamp <- charToRaw("End Of Sequence")
   RecipeStamp <- charToRaw("Recipe Name :")
   WaferStamp <- charToRaw("Wafer Name :")
   LotStamp <- charToRaw("Lot Name :")
   CassetteStamp <- charToRaw("Cassette Name :")
   SlotStamp <- charToRaw("Slot Name :")
   StepStamp <- charToRaw("Step :")

   filesize <- file.info(FName)$size
   if (filesize < 2000) {
      close (input)
      stop(" Endpoint file corrupted")
   }   
   sp.header <- 128
   input <- file(FName,"rb")
   fill <- readBin(input, integer(),1)
#   data.size <- readBin(input, integer(),signed=FALSE,1)
   data.size <- readBin(input, integer(),1)
   header.offset <- readBin(input, integer(),2)
   header.frames <- readBin(input, integer(),2)
   header <- readBin(input, raw(), header.offset[1]-24)

   if ((data.size-header.offset[1])>1 ) sp_data <- readBin(input, raw(), data.size-header.offset[1])
# read config part - espetially wavelength range
   sp.setup <- readBin(input, raw(),100000000)
   close (input)

   Pointer <- 1
   IDL <- readBin(header[Pointer], integer(), 1, size=1)
   ChamberID <- rawToChar(header[(Pointer+1):(Pointer+IDL)])
   Pointer <- Pointer + IDL+1
   SVCL <- readBin(header[Pointer], integer(), 1, size=1)
   SVC <- rawToChar(header[(Pointer+1):(Pointer+SVCL)])
   Pointer <- Pointer + SVCL+3 


   RecipeID <- grepRaw (RecipeStamp, header, fixed=TRUE)
   WaferID <- grepRaw (WaferStamp, header, fixed=TRUE)
   LotID <- grepRaw (LotStamp, header, fixed=TRUE)
   CasetteID <- grepRaw (CassetteStamp, header, fixed=TRUE)
   SlotID <-  grepRaw (SlotStamp, header, fixed=TRUE)

   RecipeRaw <- header[(Pointer):(Pointer+258)]
   WaferRaw <- header[(Pointer+264):(Pointer+522)]
   LotRaw <- header[(Pointer+528):(Pointer+786)]
   CasetteRaw <- header[(Pointer+792):(Pointer+1050)]

   RecipeRaw[RecipeRaw<32] <- as.raw(32)
   Recipe <- rawToChar(RecipeRaw)
   WaferRaw[WaferRaw<32] <- as.raw(32)
   Wafer <- rawToChar(WaferRaw)
   LotRaw[LotRaw<32] <- as.raw(32)
   Lot <- rawToChar(LotRaw)
   CasetteRaw[CasetteRaw<32] <- as.raw(32)
   Casette <- rawToChar(CasetteRaw)

   Recipe <- gsub("^\\s+|\\s+$", "", unlist(strsplit(Recipe, ":"))[2])
   Recipe <- unlist(strsplit(Recipe, " "))[1]
   Wafer <- gsub("^\\s+|\\s+$", "", unlist(strsplit(Wafer, ":"))[2])
   Wafer <- unlist(strsplit(Wafer, " "))[1]
   Lot <- gsub("^\\s+|\\s+$", "", unlist(strsplit(Lot, ":"))[2])
   Lot <-  unlist(strsplit(Lot, " "))[1]
   Casette <- gsub("^\\s+|\\s+$", "", unlist(strsplit(Casette, ":"))[2])
   Casette <-  unlist(strsplit(Casette, " "))[1]
#----------------------------------


#   RecipeL <- readBin(header[159], integer(), 1, size=1)
#   Recipe <- rawToChar(header[160:(RecipeL+160)])

   startWL <- readBin(sp.setup[29:32], integer(),1, signed=TRUE, size=4)
   endWL <- readBin(sp.setup[33:36], integer(),1, signed=TRUE, size=4)
   itime <- readBin(sp.setup[17:20], integer(),1, signed=TRUE, size=4)
# estimate channel count per spectra
   channels <- (endWL-startWL)*2+1
   spectra.size <- channels*2+ sp.header
   spectra.count <- (data.size-header.offset[1])/spectra.size

   if(spectra.count>0){ # spectra contained in the SVD file
      spectra.sources <- spectra.count / header.frames[1]
      spectra <- array(NA, dim=c(header.frames[1],channels, spectra.sources))
      tstamp <- array(NA, dim=c(header.frames[1], spectra.sources))
      time <- array(NA, dim=c(header.frames[1], spectra.sources))
      IDframe <-array(NA, dim=c(header.frames[1], spectra.sources))
      for (i in 0:(header.frames[1]-1))
      {
         for (j in 0:(spectra.sources-1))
         {
            jump <- (i*spectra.sources+j)*spectra.size
            tstamp[i+1,j+1] <- rawToChar(sp_data[(jump+1):(jump+21)])
            time[i+1,j+1] <- readBin(sp_data[(jump+41):(jump+44)], integer(),1, )
            IDframe[i+1,j+1] <- readBin(sp_data[(jump+45):(jump+49)], integer(),1)
            spectra[i+1,,j+1] <- readBin(sp_data[(jump+129):(jump+129+channels*2)], integer(),channels, signed=FALSE, size=2)
         }
      }
      WL <- seq(startWL, endWL, 0.5)
      Stamp <- tstamp[1]
   } else { # no spectra contained
      spectra.sources <- NA
      spectra.count <- 0
      spectra <- NA
      WL <- seq(startWL, endWL, 0.5)
      time <- NA
      IDframe <- NA
      Stamp <- NA
   }

# extract logistics info 

   EvID <- grepRaw (EventStamp, sp.setup, fixed=TRUE)
   ConfID <- grepRaw (ConfStamp, sp.setup, fixed=TRUE, all=TRUE )
   EndID <- grepRaw (EndStamp, sp.setup, offset= ConfID, fixed=TRUE)

   if(length(EndID)==0) EndID <- length(sp.setup)

   if(length(ConfID)==0){
      Logi <- NA
      AGC <- NA
      EP <- NA
      File <- NA
      Config <-NA
      warning (" No Logistics info available")
   } else {
      LogsPtr <-  sp.setup[(ConfID-29): (ConfID-20)]
      LogsPtr <- 29 - max(which(LogsPtr==00)) + 2
      LogsCount <- readBin(sp.setup[ConfID-LogsPtr], integer(),1, size=1, signed=FALSE)
      
      Logistics <- sp.setup[(ConfID-LogsPtr+2): (EndID+14)]
      # decompose Logistics
      Pointer <- 1
      Counter <- 1
      Tstamp <- c()
      Label  <- c()
      Value  <- c()
      
      ##########################
      for(i in 1:LogsCount)
      {
         ByteCount  <- readBin(Logistics[Pointer], integer(),1, size=1, signed=FALSE)
         Chain <- rawToChar(Logistics[(Pointer+1):(Pointer+ByteCount)]) 
         SPLIT <- unlist(strsplit(Chain, ":"))
         Tstamp[Counter] <- paste(SPLIT[1:3], collapse=":")
         #      Tstamp[Counter] <- substr(Tstamp[Counter], 1, (nchar(Tstamp[Counter])-1))
         Label[Counter] <- SPLIT[4]
         if(length(SPLIT)>4) Value[Counter] <- paste(SPLIT[5:length(SPLIT)], collapse=" ") else Value [Counter] <- NA
         Pointer <- Pointer+ByteCount+1
         Counter <- Counter +1
      }
      
      # end of logistics info 
      Logi <- data.frame(Time=Tstamp, Label=Label, Value=Value, stringsAsFactors = FALSE) 
      Logi$Label <- gsub("^\\s+|\\s+$", "", Logi$Label)
      Logi$Value <- gsub("^\\s+|\\s+$", "", Logi$Value)
      EPID <- grep ("ENDPOINT at", Label) 
      
      if (length(EPID) >0) {
         EP <- sapply(EPID, function(x){as.numeric(unlist(strsplit(Label[x], " "))[4])} )
      }else EP <- NA
      
      Config <- Value[grep("Configuration", Label)]
      Config <- substr(Config, 2, nchar(Config))
      File <- gsub(" ",":", Value[grep("New data file", Label)])
      File <- substr(File, 2, nchar(File))
      AGCID <- grep("Integration altered from", Value)
      if(length(AGCID) >0) {
         AGCSPLIT <- unlist(strsplit(unlist(strsplit(Value[AGCID], "ms")), " " ))
         options(warn = -1)
         AGCSPLIT <- na.omit( as.numeric(AGCSPLIT)) 
         AGC <- AGCSPLIT[length(AGCSPLIT)]
         options(warn = 0)
      } else AGC <- NA
   } # end of logistics decomposition



   ret <- list(WL=WL, t=time/1000, SP=spectra, TF=IDframe, SpectraCount=spectra.count, 
      TStamp=Stamp, IT=itime, AGC=AGC, SourceCount=spectra.sources, EP=round(EP,2), Config=Config,
      File=File, Recipe=Recipe, Wafer=Wafer, Lot=Lot, Casette=Casette, Logistics=Logi, 
      ChamberID=ChamberID) 

   return (ret)
} # end of readSVD function

