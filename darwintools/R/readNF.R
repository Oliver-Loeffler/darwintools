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

#------------------------------------------------
# readNF inspection file  from Fname file
# CATS decides if rotate and transform coordinates from aligment to CATS
# ver 0.3.4 - accepting inspection files without defects
#------------------------------------------------
readNF <- function(Fname, CATS=TRUE)
{
   NFpath <- "T:/Tools/INS116/Results/TEXT" 
   NF2path <- "T:/Tools/INS120/Results/TEXT" 
   NF3path <- "T:/Tools/INS131/Results/TEXT" 

   if(!file.exists(Fname) & file.exists(file.path(NFpath, Fname)))
      Fname <- file.path(NFpath,Fname)
   if(!file.exists(Fname) & file.exists(file.path(NF2path, Fname)))
      Fname <- file.path(NF2path,Fname)
   if(!file.exists(Fname) & file.exists(file.path(NF3path, Fname)))
      Fname <- file.path(NF3path,Fname)
   if (! file.exists(Fname)) stop(paste(Fname, " file not existing!",sep=""))
# change working directory
   Inp <- file(Fname, "r")
   tmp <- readLines(Inp, n=10000)
   close(Inp)
   stripNr <- which(substr(tmp[],1,11) == "Defect List")+3
   AliNr  <- which(substr(tmp[],1,15) == "    Start Point")
   AngleNr <- which(substr(tmp[],1,23)== "    Mask 90 degree Turn")
   RefT <- unlist(strsplit(tmp[AliNr],c("X:","Y:")))[2]
   RefT <- unlist(strsplit(RefT,"Y:"))
   RefX <- as.numeric(RefT[1])
   RefY <- as.numeric(RefT[2])
# get orientation of the mask
   Inspection <- substr(tmp[AngleNr],25,nchar(tmp[AngleNr]))
   angle <- 0
   if(gregexpr("No", tmp[AngleNr])>-1) angle <- 0
   if(gregexpr("Yes", tmp[AngleNr])>-1) angle <- 90

# read defect list
   Inp <- file(Fname, "r")
   tmp <- readLines(Inp, n=stripNr)
   tmp <- readLines(Inp, n=1)
   Input <- c()
   while (substr(tmp,1,14) != "Algorithm List")
   {
      Input <- c(Input,tmp) 
      tmp <- readLines(Inp, n=1)
   }
   close(Inp)
   Input <- Input[1:(length(Input)-2)]
   options(warn=-1)
   if(Input=="") Input <- c()
   options(warn=0)
   Index <-  as.numeric(substr(Input,1,5))
   if (length(Index)==0) 
   { # no defects in inspection reports
      data <- data.frame(Index=c(0), X=c(0), Y=c(0), SizeX=c(0),
         SizeY=c(0), Type=c(0))
      data <- data[-1,]
   } else { # file contains at least one defect
      if (!Index[1]==1) print(paste("Read inspection file", basename(Fname)), " error")
# convert data to readable form
      if (CATS) # rotate and shift to CATS coordiates
      {
         X1 <- as.numeric(substr(Input,6,17)) + RefX- 76200
         Y1 <- as.numeric(substr(Input,18,30)) + RefY- 76200
         X <- (X1*cos(angle/180*pi)-Y1*sin(angle/180*pi)+ 76200)/1000
         Y <- (X1*sin(angle/180*pi)+Y1*cos(angle/180*pi)+ 76200)/1000
      } else { # leave coordiNates as is
         X <- as.numeric(substr(Input,6,17))/1000
         Y <- as.numeric(substr(Input,18,30))/1000
      }
      SizeX <- as.numeric(substr(Input,31,37))
      SizeY <- as.numeric(substr(Input,38,45))
# estimate defect type
      ID <- substr(Input,85,93)
      Type <- substr(ID, 1, 2)
      if(Type=="Un") Descr <- "" else Descr <- substr(ID, 4, nchar(ID))
      
      data <- data.frame(Index,X,Y,SizeX,SizeY,Type, Descr)
   } # end of if....for at least one defect  
   return(data)
}
