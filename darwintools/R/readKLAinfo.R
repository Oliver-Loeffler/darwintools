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

readKLAinfo <-function (Fname) 
{
    KLApath <- "T:/Tools/INS112/Reports" 
    defline <- function(text) {
       D <- unlist(strsplit(text, " "))
       D <- D[nchar(D)>0]
       options(warn=-1)
       Dcounts <- as.numeric(D)
       options(warn=0)
#       Dtype <- paste(D[is.na(Dcounts)], collapse=" ")
#       Dcounts <- Dcounts[!is.na(Dcounts)]
# alternative selection optimized for digit at the end of defect name separed by space
       Dcounts <- Dcounts[(length(D)-4):length(D)]
       Dtype <- paste(D[1:(length(D)-5)], collapse=" ")


       return(list(Dtype=Dtype, Dcounts=Dcounts))
    }

    defectSplit <- function(defects) {
        defnames <- c("Type", "Small", "Medium", "Large", "XLarge", 
            "Total")
        def <- lapply(defects, defline)
        deftype <- unlist(lapply(def, "[", 1))
        defcounts <- unlist(lapply(def, "[", 2))
        defsmall <- defcounts[seq(1, length(defcounts), (length(defcounts)%/%length(deftype)))]
        defmedium <- defcounts[seq(2, length(defcounts), (length(defcounts)%/%length(deftype)))]
        deflarge <- defcounts[seq(3, length(defcounts), (length(defcounts)%/%length(deftype)))]
        defXlarge <- defcounts[seq(4, length(defcounts), (length(defcounts)%/%length(deftype)))]
        deftotal <- defcounts[seq(5, length(defcounts), (length(defcounts)%/%length(deftype)))]
        deflist <- data.frame(deftype, defsmall, defmedium, deflarge, 
            defXlarge, deftotal)
        names(deflist) <- defnames
        return(deflist)
    }
    splitDetector <- function(line) {
       Detector <- unlist(strsplit(line, " "))
       options(warn=-1)
       Detector <- as.numeric(Detector)
       options(warn=0)
       Detector <- Detector[which(!is.na(Detector))]
       return(Detector)
    }
    replacestring <- function(Sx, find, replac) {
        x <- Sx
        for (i in 1:length(replac)) {
            x <- chartr(find[i], replac[i], x)
        }
        return(x)
    }
   if (!file.exists(Fname) & file.exists(file.path(KLApath, Fname)))
         Fname <- file.path(KLApath,Fname)
   if (! file.exists(Fname)) stop(paste(Fname, " file not existing!",sep=""))
#    if (is.character(Fname)) {
        Inp <- file(Fname, "r")
        tmp <- readLines(Inp)
        close(Inp)
        PlateNr <- grep("Plate:", tmp)
        InspectionNr <- grep("Inspection:", tmp[])
        IDNr <- grep("KT9X FMID:", tmp[])
        InspTypeNr <- grep("InspectionType", tmp[])
        PixelNr <- grep("PixelSize", tmp[])
        SystemNr <- grep("Inspection System:", tmp[])
        IDateNr <- grep("Data Stored Date:", tmp[])
        AliTypeNr <- grep("PlateAlignment:", tmp[]) + 1
        AliNr <- grep("Alignment Points:", tmp[]) + 1
        AliDBNr <- grep("Database Alignment Point Locations:", 
            tmp[]) + 1
        AreaNr <- grep("Inspection Area List:", tmp[])
        BLCNr <- grep("Bottom Left Corner", tmp[])
        TRCNr <- grep("Top Right Corner", tmp[])
        DefectsumNr <- grep("Chrome User Classification Summary", 
            tmp[]) + 2
        Defectsum2Nr <- grep("Chrome Defect Classification Summary", 
            tmp[]) + 2
        TotNr <- grep("Total", tmp[])
        TotalNr <- TotNr[which(TotNr > DefectsumNr)]
        Total2Nr <- TotNr[which(TotNr > Defectsum2Nr)][1]
        DetDBTHR1Nr <- grep("DBT_HiRes_1 ", tmp[])[which((grep("DBT_HiRes_1 ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDBTHR2Nr <- grep("DBT_HiRes_2 ", tmp[])[which((grep("DBT_HiRes_2 ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDBTL2ONr <- grep("DBT_Litho_2_Over ", tmp[])[which((grep("DBT_Litho_2_Over ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDBTL2UNr <- grep("DBT_Litho_2_Under ", tmp[])[which((grep("DBT_Litho_2_Under ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDBRHR1Nr <- grep("DBR_HiRes_1 ", tmp[])[which((grep("DBR_HiRes_1 ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDBRHR2Nr <- grep("DBR_HiRes_2 ", tmp[])[which((grep("DBR_HiRes_2 ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDDTHR1Nr <- grep("DDT_HiRes_1 ", tmp[])[which((grep("DDT_HiRes_1 ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDDTHR2Nr <- grep("DDT_HiRes_2 ", tmp[])[which((grep("DDT_HiRes_2 ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDDTL2ONr <- grep("DDT_Litho_2_Over ", tmp[])[which((grep("DDT_Litho_2_Over ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDDTL2UNr <- grep("DDT_Litho_2_Under ", tmp[])[which((grep("DDT_Litho_2_Under ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDDRHR1Nr <- grep("DDR_HiRes_1 ", tmp[])[which((grep("DDR_HiRes_1 ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        DetDDRHR2Nr <- grep("DDR_HiRes_2 ", tmp[])[which((grep("DDR_HiRes_2 ", 
            tmp[]) %in% grep("Defect", tmp[])))]
        Reference <- replacestring(tmp[AliNr], c(":", "mm", "("), 
            c(",", "  ", ","))
        Reference <- unlist(strsplit(Reference, ","))[2:3]
        RefX <- as.numeric(Reference[1])
        RefY <- as.numeric(Reference[2])
        if (length(AliDBNr)> 0) {
            Reference <- replacestring(tmp[AliDBNr], c(":", "mm", 
                "("), c(",", "  ", ","))
            Reference <- unlist(strsplit(Reference, ","))[2:3]
            RefDBX <- as.numeric(Reference[1])
            RefDBY <- as.numeric(Reference[2])
        }
        else {
            RefDBX <- -1
            RefDBY <- -1
        }
        Inspection <- unlist(strsplit(tmp[InspectionNr], ":"))[2]
        InspType <- unlist(strsplit(tmp[InspTypeNr], " "))
        InspType <- InspType[which(nchar(InspType) > 0)][2]
        Pixel <- replacestring(tmp[PixelNr], c("Size", "um"), 
            c(",   ", "  "))
        Pixel <- as.numeric(unlist(strsplit(Pixel, ","))[2:3])
        Plate <- unlist(strsplit(tmp[PlateNr], ":"))[2]
        ID <- unlist(strsplit(tmp[IDNr], ": "))[2]
        System <- unlist(strsplit(tmp[SystemNr], ": "))[2]
        Date <- unlist(strsplit(tmp[IDateNr], ": "))[2]
        Alignment <- unlist(strsplit(tmp[AliTypeNr], ": "))[2]
        Reference <- replacestring(tmp[BLCNr], c("ner", "mm"), 
            c(",  ", "  "))
        Reference <- unlist(strsplit(Reference, ","))
        BLC1 <- as.numeric(Reference[2])
        BLC2 <- as.numeric(Reference[3])
        Reference <- replacestring(tmp[TRCNr], c("ner", "mm"), 
            c(",  ", "  "))
        Reference <- unlist(strsplit(Reference, ","))
        TRC1 <- as.numeric(Reference[2])
        TRC2 <- as.numeric(Reference[3])
        if (length(DefectsumNr) > 0) 
            defects <- defectSplit(tmp[(DefectsumNr + 1):TotalNr])
        else defects <- c()
        if (length(Defectsum2Nr) > 0) 
            defects2 <- defectSplit(tmp[(Defectsum2Nr + 1):Total2Nr])
        else defects2 <- c()
        DBTHiRes1 <- splitDetector(tmp[DetDBTHR1Nr])
        DBTHiRes2 <- splitDetector(tmp[DetDBTHR2Nr])
        DBRHiRes1 <- splitDetector(tmp[DetDBRHR1Nr])
        DBRHiRes2 <- splitDetector(tmp[DetDBRHR2Nr])
        DBTL2O <- splitDetector(tmp[DetDBTL2ONr])
        DBTL2U <- splitDetector(tmp[DetDBTL2UNr])
        DDTHiRes1 <- splitDetector(tmp[DetDDTHR1Nr])
        DDTHiRes2 <- splitDetector(tmp[DetDDTHR2Nr])
        DDRHiRes1 <- splitDetector(tmp[DetDDRHR1Nr])
        DDRHiRes2 <- splitDetector(tmp[DetDDRHR2Nr])
        DDTL2O <- splitDetector(tmp[DetDDTL2ONr])
        DDTL2U <- splitDetector(tmp[DetDDTL2UNr])
        Area <- (TRC1 - BLC1) * (TRC2 - BLC2)
        data <- list(Inspection = Inspection, InspType = InspType, 
            Pixel = Pixel, Plate = Plate, ID = ID, System = System, 
            Date = Date, Alignment = Alignment, Area = Area, 
            Defects = defects, DefectsClass = defects2, DBTHiRes1 = DBTHiRes1, 
            DBTHiRes2 = DBTHiRes2, DBRHiRes1 = DBRHiRes1, DBRHiRes2 = DBRHiRes2, 
            DBTL2O = DBTL2O, DBTL2U = DBTL2U, DDTHiRes1 = DDTHiRes1, 
            DDTHiRes2 = DDTHiRes2, DDRHiRes1 = DDRHiRes1, DDRHiRes2 = DDRHiRes2, 
            DDTL2O = DDTL2O, DDTL2U = DDTL2U)
#    }
    return(data)
}