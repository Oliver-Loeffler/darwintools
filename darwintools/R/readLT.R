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


readLT <- function (Fname, CATS = TRUE) 
{
    LTpath <- "T:/Tools/DDE166/01_Results/csv-Files"
    if (!file.exists(Fname) & file.exists(file.path(LTpath, Fname))) 
        Fname <- file.path(LTpath, Fname)
    if (file.exists(Fname)) {
        Inp <- file(Fname, "r")
        tmp <- readLines(Inp)
        close(Inp)
    } else stop(paste(Fname, " file not existing!", sep = ""))
    options(warn = -1)
    DataListID <- which(tmp[] == "InspectionDataList")
    SpacesID <- which(tmp[] == "")
    FrontID <- which(tmp[] == "@Front")
    BackID <- which(tmp[] == "@Back")
    EOFID <- which(tmp[] == "EOF")
    DISEID <- which(tmp[] == "DISEResultDataList")
    options(warn = 0)
    data <- c()
    if (length(FrontID) > 0) {
        Start <- DataListID[min(which(DataListID > FrontID))]
        End <- SpacesID[min(which(SpacesID > Start))] - 1
        if (End >= Start + 2 & substr(tmp[Start + 2], 1, 1) == 
            "1") 
            data <- c(data, tmp[(Start + 2):End])
    }
    if (length(BackID) > 0) {
        Start <- DataListID[min(which(DataListID > BackID))]
        End <- SpacesID[min(which(SpacesID > Start))] - 1
        if (End >= Start + 2 & substr(tmp[Start + 2], 1, 1) == 
            "1") 
            data <- c(data, tmp[(Start + 2):End])
    }
    DefNum <- as.numeric(sapply(strsplit(data, ","), "[", 1))
    Side <- sapply(strsplit(data, ","), "[", 2)
    X <- as.numeric(sapply(strsplit(data, ","), "[", 3)) + 76.2
    Y <- as.numeric(sapply(strsplit(data, ","), "[", 4)) + 76.2
    Type <- sapply(strsplit(data, ","), "[", 5)
    SizeX <- as.numeric(sapply(strsplit(data, ","), "[", 6)) * 
        1000
    SizeY <- as.numeric(sapply(strsplit(data, ","), "[", 7)) * 
        1000
    Size <- SizeX
    Channel <- as.numeric(sapply(strsplit(data, ","), "[", 8))
    Pixel <- as.numeric(sapply(strsplit(data, ","), "[", 9))
    Bin <- as.numeric(sapply(strsplit(data, ","), "[", 10))
    ID <- 1:length(X)
    # and now DISE front
    if (length(FrontID)>0 & any(DISEID < EOFID)) {
        Start <- DISEID[1]
        if (length(BackID)>0) End <- BackID-2 else End <- EOFID-2
        
        if (End >= Start + 2 & substr(tmp[Start + 2], 1, 1) %in% 0:9) 
            dataDISEF <- tmp[(Start + 2):End]
    }    else dataDISEF <- c()
    # and DISE back
    if (length(BackID) > 0 & any(DISEID > BackID)) {
        Start <- DISEID[which(DISEID > BackID)]
        End <- EOFID - 2
        if (End >= Start + 2 & substr(tmp[Start + 2], 1, 1) %in% 0:9) 
            dataDISEB <- c(tmp[(Start + 2):End])
    } else dataDISEB <- c()
    dataDISE <- c(dataDISEF, dataDISEB)
    if (length(dataDISE) > 0) {
        DISEId <- as.numeric(sapply(strsplit(dataDISE, ","), "[", 1))
        DISESide <- sapply(strsplit(dataDISE, ","), "[", 2)
        DISESizeX <- as.numeric(sapply(strsplit(dataDISE, ","), "[", 6)) * 1000
        DISESizeY <- as.numeric(sapply(strsplit(dataDISE, ","), "[", 7)) * 1000
        DISESize <- apply(cbind(DISESizeX, DISESizeY), 1, max)
        DISE <- data.frame(ID = DISEId, Side = DISESide, SizeX = DISESizeX, 
                           SizeY = DISESizeY, Size = DISESize)
        if (any(DISE$Side == "Front")) {
            DISEF <- DISE[which(DISE$Side == "Front"), ]
            IDs <- which(Side == "Front" & DefNum %in% DISEF$ID)
            SizeX[IDs] <- DISEF$SizeX
            SizeY[IDs] <- DISEF$SizeY
            Size[IDs] <- DISEF$Size
        }
        if (any(DISESide == "Back")) {
            DISEB <- DISE[which(DISE$Side == "Back"), ]
            IDs <- which(Side == "Back" & DefNum %in% DISEB$ID)
            SizeX[IDs] <- DISEB$SizeX
            SizeY[IDs] <- DISEB$SizeY
            Size[IDs] <- DISEB$Size
        }
    }
    result <- data.frame(Id = ID, DefNum = DefNum, Side = Side, 
                         X = X, Y = Y, Type = Type, Size = Size, Channel = Channel, 
                         Pixel = Pixel, Bin = Bin)
    return(result)
}