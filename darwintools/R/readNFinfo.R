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
# readNFinfo inspection file  from Fname file
# ver 065
#------------------------------------------------

readNFinfo <- function(Fname)
{
# extract X and Y coordinates for e.g. alignment mark
   getAlignment <- function(txt, match=c("X:","Y:"))
   {
      Xpos <- as.numeric(gregexpr(match[1] , txt)[1])
      Ypos <- as.numeric(gregexpr(match[2] , txt)[1])
      Xraw <- substr(txt, Xpos+nchar(match[1]), Ypos-1)
      Yraw <- substr(txt, Ypos+nchar(match[1]), nchar(txt))
      result <- c(as.numeric(Xraw), as.numeric(Yraw))
      return(result)
   }

# extracting data from line with one parameter only
   split1par <- function(txt)
   {
      result <- substr(txt,  57 ,nchar(txt))
      return(result)
   }

# extract insperction areas table 
   readArea <- function(txt)
   {
      Pos <- txt[grep("Position", txt)]
      Size <- txt[grep("Size", txt)]
      P <-  lapply(Pos, getAlignment, match=c("X :","Y :"))
      S <-  lapply(Size, getAlignment, match=c("X :","Y :"))
      PositionX <- sapply(P, "[", 1)
      PositionY <- sapply(P, "[", 2)
      SizeX <- sapply(S, "[", 1)
      SizeY <- sapply(S, "[", 2)
      output <- data.frame(PositionX=PositionX, PositionY=PositionY, SizeX=SizeX, SizeY=SizeY)
      return(output)
   }

# extract RIG points table 
   readRIG <- function(txt)
   {
      P <-  lapply(txt, getAlignment, match=c("X :","Y :"))
      X <- sapply(P, "[", 1)
      Y <- sapply(P, "[", 2)
      PTID <- as.numeric(gregexpr("Point", txt))+6
      MIID <- as.numeric(gregexpr("[micron]", txt, fixed=TRUE))-1
      txttmp <- substr(txt, PTID, MIID)
      Id <- as.numeric(sapply(strsplit(txttmp, " "), "[", 1))
      Name <- sapply(strsplit(txttmp, " "), "[", 2)
      output <- data.frame(Id, Name, X, Y, stringsAsFactors=FALSE)
      return(output)
   }

# reading light cal info table
   readLightCal <-function(txt)
   {
      PosT <- grep("Target", txt)
      LightVars <-c("CorrWGrad","CorrBGrad","WGrad","BGrad")
      CorrWT <- grep("Corr.W-Grad.Val.", txt)
      CorrBT <- grep("Corr.B-Grad.Val.", txt)
      WGradT <- grep(" W-Grad.Val.", txt)
      BGradT <- grep(" B-Grad.Val.", txt)
      TargetID <- as.numeric(gregexpr( "Target" , txt[PosT])[[1]])
      MeanID <- as.numeric(gregexpr( "Mean" , txt[PosT])[[1]])
      TTarget <- as.numeric(substr(txt[c(CorrWT, CorrBT, WGradT, BGradT)], TargetID[1]-2, TargetID[1]+10))
      TMean <- as.numeric(substr(txt[c(CorrWT, CorrBT, WGradT, BGradT)], MeanID[1]-2, MeanID[1]+10))
      RTarget <- as.numeric(substr(txt[c(CorrWT, CorrBT, WGradT, BGradT)], TargetID[2]-2, TargetID[2]+10))
      RMean <- as.numeric(substr(txt[c(CorrWT, CorrBT, WGradT, BGradT)], MeanID[2]-2, MeanID[2]+10))
      output <- data.frame(Name= LightVars, TransTarget=TTarget, TransMean=TMean, ReflectTarget=RTarget, ReflectMean=RMean, stringsAsFactors=FALSE)
      return(output)
   }

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

   SerialNr <- grep("Serial No. :", tmp)
   DeviceNr <- grep("Series     :", tmp)
   IStartNr <- grep("Inspection Start:", tmp)
   IEndNr <- grep("Inspection End  :", tmp)
   ToolNr <- grep("System Type     :", tmp)
   LayerNr <- grep("Layer      :", tmp)
   LayoutNr <- grep("Layout     :", tmp)
   JobNr <- grep("Lot        :", tmp)
   IDNr <- grep("ID         :", tmp)
   PattNr <- grep("Pattern Information", tmp)
   ModeNr <- grep("Mode  ", tmp)
   ModeNr <- ModeNr[which.min(ModeNr-PattNr)]
   ResolutionNr <- grep("Resolution     ", tmp)
   IdataNr <- grep("Inspection Data Name   ", tmp)
   MaskInfNr <- grep("Mask Information", tmp)
   MaskAbsNr <- grep("Mask Absorber   ", tmp)
   MaskAbsNr <- MaskAbsNr[which.min(MaskAbsNr-MaskInfNr)]
   Mask90DNr <- grep("Mask 90 degree Turn", tmp)
   AliNr <- grep("Start Point Information", tmp)
   StartAliNr <- grep("Start Point                           [micron]", tmp, fixed=TRUE)
   InspTactNr <- grep("Inspection Tact Time", tmp)
   TotalStripNr <- grep("Total Stripes Count     ", tmp)
   StartStripNr <- grep("Specified Start Stripe No", tmp)
   EndStripNr <- grep("Specified End Stripe No", tmp)
   IStatusNr <- grep("Inspection Status    ", tmp)
   IAreaCountNr <- grep("Inspection Area         Number", tmp)
   NIAreaCountNr <- grep("Non-inspection Area     Number", tmp)
   PriorIAreaNr <- grep("Prior-inspection Area   Number", tmp)
   SumDimNr <- grep("Sum of Inspection Dimension", tmp)
   Align1Nr <- grep("Alignment Point 1", tmp)
   Align2Nr <- grep("Alignment Point 2", tmp)
   Align3Nr <- grep("Alignment Point 3", tmp)
   Align4Nr <- grep("Alignment Point 4", tmp)
   RIGPtsNr <- grep("RIG Point", tmp)
   StagespeedNr <- grep("Stage Speed", tmp)
   LightCalNr <- grep("Light Intensity Calibration Information", tmp)
   VersionNr <- grep("Version Information", tmp)


   SerialI <- as.numeric(gsub(" ", "", unlist(strsplit(tmp[SerialNr], ":"))[2]))
   DeviceI <- gsub(" ", "", substr(unlist(strsplit(tmp[DeviceNr], ":"))[2], 1,40))
   TTT <- gregexpr("Inspection Start:", tmp[IStartNr])
   IStartI <- substr(tmp[IStartNr], TTT[[1]]+18, nchar(tmp[IStartNr]))
   TTT <- gregexpr("Inspection End  :", tmp[IEndNr])
   IEndtI <- substr(tmp[IEndNr], TTT[[1]]+18, nchar(tmp[IEndNr]))
   TTT <- gregexpr("System Type     :", tmp[ToolNr])
   ToolI <- substr(tmp[ToolNr], TTT[[1]]+18, nchar(tmp[ToolNr]))
   LayerI <-  gsub(" ", "", substr(unlist(strsplit(tmp[LayerNr], ":"))[2], 1,40))
   LayoutI <-  gsub(" ", "", substr(unlist(strsplit(tmp[LayoutNr], ":"))[2], 1,40))
   JobI <- as.numeric(gsub(" ", "", substr(unlist(strsplit(tmp[JobNr], ":"))[2], 1,40)))
   IDI <- gsub(" ", "", substr(unlist(strsplit(tmp[IDNr], ":"))[2], 1,40))
   ModeI <- split1par(tmp[ModeNr])
   ResolutionI <- as.numeric(split1par(tmp[ResolutionNr]))*1000
   IdataI <- split1par(tmp[IdataNr])
   MaskAbsI <- split1par(tmp[MaskAbsNr])
   Mask90DI <- split1par(tmp[Mask90DNr])
   AlignmentI <- getAlignment(tmp[StartAliNr]) 
   ITI <- strptime(split1par(tmp[InspTactNr]), format="%HH %MM %SS")
   InspTactI <- strftime(ITI, format="%H:%M:%S")
   TotalStripI <- as.numeric(split1par(tmp[TotalStripNr]))
   StartStripI <- as.numeric(split1par(tmp[StartStripNr]))
   EndStripI <- as.numeric(split1par(tmp[EndStripNr])) 
   IStatusI <- split1par(tmp[IStatusNr]) 
   if(length(IAreaCountNr)>0) 
   {
      IAreaCountI <- as.numeric(split1par(tmp[IAreaCountNr]))
      IArea <- readArea(tmp[(IAreaCountNr+1) : (IAreaCountNr+ 2*IAreaCountI +1)])
   } else {
      IAreaCountI <- NA
      IArea <- NA
   } 
   if(length(NIAreaCountNr)>0) 
   {
      NIAreaCountI <- as.numeric(split1par(tmp[NIAreaCountNr])) 
      if(NIAreaCountI>0) 
         NIArea <- readArea(tmp[(NIAreaCountNr+1) : (NIAreaCountNr+ 2*NIAreaCountI +1)]) else  NIArea <- NA
   } else {
      NIAreaCountI <- NA
      NIArea <- NA
   } 
   if(length(PriorIAreaNr)>0) PriorIAreaI <- as.numeric(split1par(tmp[PriorIAreaNr])) else
      PriorIAreaI <- NA
   SumDimI <- as.numeric(split1par(tmp[SumDimNr]))
   if(length(Align1Nr)) Align1I <- getAlignment(tmp[Align1Nr], match=c("X :","Y :")) else 
      Align1I <- NA
   if(length(Align2Nr)) Align2I <- getAlignment(tmp[Align2Nr], match=c("X :","Y :")) else 
      Align2I <- NA 
   if(length(Align3Nr)) Align3I <- getAlignment(tmp[Align3Nr], match=c("X :","Y :"))  else 
      Align3I <- NA
   if(length(Align4Nr)) Align4I <- getAlignment(tmp[Align4Nr], match=c("X :","Y :"))  else 
      Align4I <- NA
   if(length(RIGPtsNr)>0) {
      RigNrI <- length(RIGPtsNr)
      RigI <- readRIG (tmp[RIGPtsNr])
   } else {
      RigNrI <- NA
      RigI <- NA
   } 
   StagespeedI <- as.numeric(split1par(tmp[StagespeedNr]))
   LightCalI <- readLightCal(tmp[LightCalNr:VersionNr-1] )

   output <- list(Serial=SerialI, Device=DeviceI, Layer=LayerI, IStart=IStartI, IEndt=IEndtI, Tool=ToolI,  Layout=LayoutI, Job=JobI, ID=IDI, Mode=ModeI, Resolution=ResolutionI, Idata=IdataI, MaskAbs=MaskAbsI, Mask90D=Mask90DI, Alignment=AlignmentI, InspTact=InspTactI, 
TotalStrip=TotalStripI, StartStrip=StartStripI, EndStrip=EndStripI, IStatus=IStatusI, 
IAreaCount=IAreaCountI, NIAreaCount=NIAreaCountI, IArea=IArea, NIArea=NIArea, PriorIArea=PriorIAreaI, SumDim=SumDimI, Align1=Align1I, Align2=Align2I, Align3=Align3I, Align4=Align4I, RigNr=RigNrI, Rig=RigI, Stagespeed=StagespeedI, LightCal=LightCalI)
   return(output)
}

