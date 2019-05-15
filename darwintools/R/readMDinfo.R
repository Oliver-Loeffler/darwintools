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

# ---------------------------------------------------------------
# read MD2500 inspection file  from Fname file
# ---------------------------------------------------------------

readMDinfo <- function (Fname) 
{
    MDpath <- "T:/Tools/DDE127/Readable_files"
    if (!file.exists(Fname) & file.exists(file.path(MDpath, Fname))) 
        Fname <- file.path(MDpath, Fname)
    if (! file.exists(Fname)) stop(paste(Fname, " file not existing!",sep=""))

    Inp <- file(Fname, "r")
    tmp <- readLines(Inp, n = 700)
    close(Inp)
    Date <- grep("Date :", tmp)
    Time <- grep("Time :", tmp)
    Machine <- grep("Machine Name", tmp)
    Mask <- grep("Mask Name", tmp)
    Lot <- grep("Lot No.", tmp)
    Comment <- grep("Comment", tmp)
    Slice <- grep("Slice Name1", tmp)
    Speed <- grep("Inspection Speed", tmp)
    AUTO <- grep("AUTO COMP", tmp)
    ChipDist <- grep("Chip Distance", tmp)
    Start <- grep("Start Position", tmp)
    Reference <- grep("Reference Position", tmp)
    ChipNo <- grep("No. of Chips", tmp)
    Scribe <- grep("Scribe Line", tmp)
    DefectLim <- grep("Defect Limit", tmp)
    LensSep <- grep("Lens Separation", tmp)
    Align  <- grep("Align on Scan", tmp)
    InspChip <- grep("Inspection Chips", tmp)
    DefectTot <- grep("Total Defect", tmp)
    DefectCheck <- grep("Check Defect", tmp)
    DefectPinhole <- grep("Pinhole Defect", tmp)
    DefectSpot <- grep("Spot Defect", tmp)

    TDate <- strsplit(tmp[Date], ":")[[1]][2]
    TTime <- paste(strsplit(tmp[Time], ":")[[1]][2:4], collapse=":")
    TMachine <- substr(tmp[Machine], 36, 80)
    TMask <- substr(tmp[Mask], 36, 80)
    TLot <- substr(tmp[Lot], 36, 80)
    TComment <- substr(tmp[Comment], 36, 80) 
    TSlice <- substr(tmp[Slice], 36, 80) 
    TSpeed <- substr(tmp[Speed], 36, 80) 
    TAUTO <- substr(tmp[AUTO], 36, 80) 
    TChipDist <- substr(tmp[ChipDist], 36, 80) 
    TStart <- substr(tmp[Start], 36, 80) 
    TReference <- substr(tmp[Reference], 36, 80) 
    TChipNo <- substr(tmp[ChipNo], 36, 80) 
    TScribe <- substr(tmp[Scribe], 36, 80) 
    TDefectLim <- substr(tmp[DefectLim], 36, 80) 
    TLensSep <- substr(tmp[LensSep], 36, 80) 
    TAlign <- substr(tmp[Align], 36, 80) 
    TInspChip <- substr(tmp[InspChip], 36, 80) 
    TDefectTot <- substr(tmp[DefectTot], 36, 80) 
    TDefectCheck <- substr(tmp[DefectCheck], 36, 80) 
    TDefectPinhole <- substr(tmp[DefectPinhole], 36, 80) 
    TDefectSpot <- substr(tmp[DefectSpot], 36, 80) 
    result <- list(Date= TDate, Time=TTime, Machine=TMachine, Mask=TMask, Lot=TLot,
       Comment=TComment, Slice=TSlice, Speed=TSpeed, AUTO=TAUTO, ChipDist=TChipDist, 
       Start=TStart, Reference=TReference, ChipNo=TChipNo, Scribe=TScribe,
       DefectLim=TDefectLim, LensSep=TLensSep, Align=TAlign, InspChip=TInspChip,
       DefectTot=TDefectTot, DefectCheck=TDefectCheck, DefectPinhole=TDefectPinhole, 
       DefectSpot=TDefectSpot) 
    return(result)
}

