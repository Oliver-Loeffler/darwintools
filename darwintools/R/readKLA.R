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

#--------------------------------------------------
# read KLA inspection file  from Fname file
# CATS decides if rotata and transfrom coordinates from aligment to CATS
# version 0.3.4 - accepting inspection fiels without defects
#--------------------------------------------------
readKLA <- function(Fname, CATS=TRUE)
{
   KLApath <- "T:/Tools/INS112/Reports" 
   KLApath2 <- "T:/Tools/INS135/Reports" 

   replacestring <- function(Sx, find, replac)
   {
      x <- Sx
      for(i in 1:length(replac))
      {
         x <- chartr(find[i],replac[i], x)
      } 
      return(x)
   }

   if (!file.exists(Fname) & file.exists(file.path(KLApath, Fname)))
         Fname <- file.path(KLApath,Fname)
   if (!file.exists(Fname) & file.exists(file.path(KLApath2, Fname)))
         Fname <- file.path(KLApath2,Fname)
   if (! file.exists(Fname)) stop(paste(Fname, " file not existing!",sep=""))

   Inp <- file(Fname, "r")
   tmp <- readLines(Inp)
   close(Inp)
   stripNr <- grep("Chrome Defect List:", tmp[])
   Defectsum2Nr <- grep("Chrome Defect Classification Summary", tmp[]) + 2
   AliNr <- grep("Alignment Points:", tmp[])+1
   AliDBNr <- grep("Database Alignment Point Locations:", tmp[])+1
   AngleNr <- grep("Inspection:", tmp[])
   if(length(grep("Reference:", tmp[AliNr]))==0)
   { # reference point not avaialbe (probably blank inspection)
      RefX <- 0
      RefY <- 0
   }else { # reference points given
      Reference <- replacestring(tmp[AliNr],c(":","mm", "(" ),c(",","  ", ","))
      Reference <- unlist(strsplit(Reference, ","))[2:3]
      RefX <- as.numeric(Reference[1])
      RefY <- as.numeric(Reference[2])
   }
   if (length(AliDBNr)>0)
   { # die to database alignment performed- CATS coordinates of alignment point is available
      Reference <- replacestring(tmp[AliDBNr],c(":","mm", "(" ),c(",","  ", ","))
      Reference <- unlist(strsplit(Reference, ","))[2:3]
      RefDBX <- as.numeric(Reference[1])
      RefDBY <- as.numeric(Reference[2])
   } else{
      RefDBX <- -1
      RefDBY <- -1
   }
# get orientation of the mask
   Inspection <- unlist(strsplit(tmp[AngleNr], ":"))[2]
   RA <- unlist(strsplit(Inspection,"-"))
   if (length(RA) > 2) RA <- RA[3] else RA <- 0
   if(RA == 0) angle <- 0
   if(nchar(RA)>=4) # there is some more sophisticated text
   {
      RA <- unlist(strsplit(RA, "@"))[1]
      if(substr(RA,nchar(RA)-1,nchar(RA)) =="IU") angle <- as.numeric(substr(RA,1, nchar(RA)-2))
      if(substr(RA,nchar(RA)-1,nchar(RA)) =="GU") angle <- 360- as.numeric(substr(RA,1, nchar(RA)-2))
   } 
   if(RefDBX >0)
   {# CATS coodinates of reference points available
      RefX <- RefDBX
      RefY <- RefDBY
   } else {# CATS coodinates of reference points not available
      if((angle==0 | angle==180) & RefX>0) 
      {
         if (abs(RefX-5.5) < 3.5 & abs(RefY-14.0) < 3.5) 
         { # CATS coordinates of reference not known, alignment position close to expected JEN cross placement
            RefX <- 5.5
            RefY <- 14.0
         }
      }
      if((angle==90 | angle==270) & RefX>0) 
      {
         if (abs(RefX-14.0) < 3.5 & abs(RefY-5.5) < 3.5) 
         { # CATS coordinates of reference not known, alignment position close to expected JEN cross
            RefX <- 14.0
            RefY <- 5.5
         }
      }
   }

# if not find arbitrary set to 510 (a number is as bad as any other)
  if ((length(stripNr)==0 & length(Defectsum2Nr)==0) | length(AliNr)==0) stop(paste(Fname, " wrong file format!",sep=""))
  if (length(stripNr)==0) # means there are no defects in the inspection file
  {
     data <- data.frame(Index=c(0), X=c(0), Y=c(0), SizeX=c(0),
        SizeY=c(0), Type=c(0), Detector=c(0))
     data <- data[-1,]
  } else { # no defects and missing title line
     Input <- tmp[(stripNr+1):length(tmp)]
     rm(tmp)
     options(warn=-1)
     Index <-  as.numeric(substr(Input,1,5))
     Input <- Input[which(!is.na(Index))]
     Index <-  as.numeric(substr(Input,1,5))
     options(warn=0)
     if (length(Index)==0) 
     { # file contains no defect, but title "Chrome defects present"
        data <- data.frame(Index=c(0), X=c(0), Y=c(0), SizeX=c(0),
           SizeY=c(0), Type=c(0), Detector=c(0))
        data <- data[-1,]
     } else { # file contains at least one defect
# convert data to readable form
# estimate position of indexes
        LocX1 <- unlist(gregexpr("Location:", Input[]))+9
        LocX2 <- unlist(regexpr(",", Input[]))-1
        LocY1 <- unlist(regexpr(",", Input[]))+1
        LocY2 <- unlist(regexpr("mm", Input[]))-1
#        SizeB <- unlist(gregexpr("Size:", Input[]))+5
#        SizeE <- unlist(gregexpr("Type:", Input[], fixed=TRUE))-1
#        InputSize <- substr(Input, SizeB, SizeE)
#        SizeENum <- unlist(gregexpr( "nm (", InputSize, fixed=TRUE))-1
#        InputSize <- substr(InputSize, 1, SizeENum)
#        SizeXY <- cbind(sapply(strsplit(InputSize, ","), "[",1 ), sapply(strsplit(InputSize, ","), "[",2 ))
        
        SizX1 <- unlist(gregexpr("Size:", Input[], fixed=TRUE))+5
#        SizX2 <- unlist(gregexpr(",", Input[]))[seq(2,2*length(Input),2)] -1
        SizX2 <- sapply(gregexpr(",", Input[]), "[", 2 ) -1
        SizY1 <- SizX2 +2
        SizY2 <- unlist(regexpr("(", Input[], fixed=TRUE))-4
        TypeT <- unlist(gregexpr("Type:", Input[], fixed=TRUE))+6
        ClasB <- unlist(gregexpr("Class:", Input[]))+6
        DescB <- unlist(gregexpr("Descr:", Input[]))+6
        DescB[which(DescB==5)] <- NA 

        if (CATS) # rotate and shift to CATS coordinates
        {
# read columns from Input
           X1 <- as.numeric(substr(Input, LocX1, LocX2)) + RefX- 76.2
           Y1 <- as.numeric(substr(Input, LocY1, LocY2)) + RefY- 76.2
           X <- X1*cos(angle/180*pi)-Y1*sin(angle/180*pi)+ 76.2
           Y <- X1*sin(angle/180*pi)+Y1*cos(angle/180*pi)+ 76.2
        } else { # leave coordiNates as is
           X <- as.numeric(substr(Input, LocX1, LocX2))
           Y <- as.numeric(substr(Input, LocY1, LocY2))
        }
        SizeX <- as.numeric(substr(Input, SizX1, SizX2))
        SizeY <- as.numeric(substr(Input, SizY1, SizY2))
# estimate defect type
        Detector <- substr(Input,TypeT, ClasB-7)
        Type <- substr(Input,ClasB+1, ClasB+2)
        Descr <- substr(Input, DescB+1, nchar(Input))
        data <- data.frame(Index,X,Y,SizeX,SizeY,Type, Detector, Descr)
      } # end of file contains at least one defect- else to with title line
   } # end of file contains at least one defect - else to without title line
   return(data)
}
