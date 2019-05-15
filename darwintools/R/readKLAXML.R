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

###########################################
# read KLA inspection file  from Fname XML file
# CATS decides if rotata and transfrom coordinates from aligment to CATS
readKLAXML <- function(Fname, CATS=TRUE)
{
   KLAXMLpath <- "T:/Tools/INS112/xml" 
   if (!file.exists(Fname) & file.exists(file.path(KLAXMLpath, Fname)))
         Fname <- file.path(KLAXMLpath,Fname)
   if (! file.exists(Fname)) stop(paste(Fname, " file not existing!",sep=""))
   raw <- readLines(Fname)
   doc <- xmlTreeParse(paste(raw,collapse=""), useInternalNodes=TRUE)	
   Index <- xpathSApply(doc,"//DefectList//DefectDetailsList//Defect",xmlGetAttr,"Index")
   DefNR <- length(which(nchar(Index)>0))
   Index <- Index[1:DefNR]
   if (length(Index)==0) 
   { # file contains no defect, but title "Chrome defects present"
      data <- data.frame(Index=c(0), X=c(0), Y=c(0), SizeX=c(0),
         SizeY=c(0), Type=c(0), Detector=c(0))
      data <- data[-1,]
   } else {
   X <- xpathSApply(doc,"//DefectList/DefectDetailsList/Defect",xmlGetAttr,"X")[1:DefNR]
   Y <- xpathSApply(doc,"//DefectList/DefectDetailsList/Defect",xmlGetAttr,"Y")[1:DefNR]
   SizeX <- xpathSApply(doc,"//DefectList/DefectDetailsList/Defect",xmlGetAttr,"SizeX")[1:DefNR]
   SizeY <- xpathSApply(doc,"//DefectList/DefectDetailsList/Defect",xmlGetAttr,"SizeY")[1:DefNR]
   Type <- substr(xpathSApply(doc, "//DefectList/DefectDetailsList/Defect", xmlGetAttr,"UserClassification")[1:DefNR],1,2)
   Detector <- xpathSApply(doc,"//DefectList/DefectDetailsList/Defect",xmlGetAttr,"DefectType")[1:DefNR]
   Note <- xpathSApply(doc, "//DefectList/DefectDetailsList/Defect", xmlGetAttr,"Note")[1:DefNR]
   Residue2x2 <- xpathSApply(doc, "//DefectList/DefectDetailsList/Defect", xmlGetAttr,"Residue2x2")[1:DefNR]
   Residue3x3 <- xpathSApply(doc, "//DefectList/DefectDetailsList/Defect", xmlGetAttr,"Residue3x3")[1:DefNR]

   AliDBNRType <- xpathSApply(doc,"//Inspection/Recipe/PlateAlignment/DatabaseAlignmentPoints/DatabaseAlignmentPoint", xmlGetAttr, "Type")
   AliDBX <- xpathSApply(doc,"//Inspection/Recipe/PlateAlignment/DatabaseAlignmentPoints/DatabaseAlignmentPoint", xmlGetAttr, "X")
   AliDBY <- xpathSApply(doc,"//Inspection/Recipe/PlateAlignment/DatabaseAlignmentPoints/DatabaseAlignmentPoint", xmlGetAttr, "Y")
   Inspection <- xpathSApply(doc,"//Inspection/GeneralInspectionData", xmlGetAttr, "InspectionName")

   Reftype <- xpathSApply(doc,"//Inspection/Recipe/PlateAlignment/AlignmentPoints/AlignmentPoint", xmlGetAttr, "Type")
   RefX <- xpathSApply(doc,"//Inspection/Recipe/PlateAlignment/AlignmentPoints/AlignmentPoint", xmlGetAttr, "X")
   RefY <- xpathSApply(doc,"//Inspection/Recipe/PlateAlignment/AlignmentPoints/AlignmentPoint", xmlGetAttr, "Y")
      
   Ref <- data.frame(Type=Reftype, X=RefX, Y=RefY) 
   RefX <- as.numeric(as.character(Ref$X[1]))
   RefY <- as.numeric(as.character(Ref$Y[1]))
   free(doc)

   if (length(AliDBNRType)>0)
   { # die to database alignment performed- CATS coordinates of alignment point is available
      RefDBX <- as.numeric(AliDBX[AliDBNRType=="Reference"])
      RefDBY <- as.numeric(AliDBY[AliDBNRType=="Reference"])
   } else{
      RefDBX <- -1
      RefDBY <- -1
   }

#      Inspection <- substr(tmp[AngleNr],20,50)
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
#
   if (CATS) # rotate and shift to CATS coordinates
   {
# read columns from Input
      X1 <- as.numeric(X) + RefX- 76.2
      Y1 <- as.numeric(Y) + RefY- 76.2
      X <- X1*cos(angle/180*pi)-Y1*sin(angle/180*pi)+ 76.2
      Y <- X1*sin(angle/180*pi)+Y1*cos(angle/180*pi)+ 76.2
   } else { # leave coordiNates as is
      X <- as.numeric(X)
      Y <- as.numeric(Y)
   }
   Index <- as.numeric(Index)
   SizeX <- as.numeric(SizeX)
   SizeY <- as.numeric(SizeY)
   Residue2x2 <- as.numeric(unlist(Residue2x2))
   Residue3x3 <- as.numeric(unlist(Residue3x3))
   if(length(Residue2x2)==0) Residue2x2 <- rep(NA, length(Index))
   if(length(Residue3x3)==0) Residue3x3 <- rep(NA, length(Index))
   data <- data.frame(Index,X,Y,SizeX,SizeY,Type, Detector, Note, Residue2x2, Residue3x3, stringsAsFactors=FALSE)
}
   return(data)
}


