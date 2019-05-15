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

readMD <- function (Fname) 
{
    MDpath <- "T:/Tools/DDE127/Readable_files"
    if (!file.exists(Fname) & file.exists(file.path(MDpath, Fname))) 
        Fname <- file.path(MDpath, Fname)
    if (! file.exists(Fname)) stop(paste(Fname, " file not existing!",sep=""))

    Inp <- file(Fname, "r")
    tmp <- readLines(Inp, n = 700)
    close(Inp)
    stripNr <- which(substr(tmp, 1, 4) == "DFNO") + 1
    AngleNr <- which(substr(tmp[], 1, 7) == "Lot No.")
    Id_Nr <- which(substr(tmp[], 1, 9) == "Mask Name")
    Ref_Nr <- which(substr(tmp[], 1, 14) == "Start Position")
    Inspection <- substr(tmp[AngleNr], 8, nchar(tmp[AngleNr]))
    if (any(c(length(stripNr), length(AngleNr), length(Id_Nr), 
       length(Inspection)))==0) stop(paste(Fname, " wrong file format!",sep=""))
    RA <- unlist(strsplit(Inspection, "_"))
    if (length(RA) > 2) RA <- as.numeric(RA[3]) else RA <- 0
    if (RA == 0) angle <- 0
    if (!is.na(RA)) angle <- RA
    alpha <- angle/180 * pi
    Ref <- tmp[Ref_Nr]
    Ref <- as.numeric(unlist(strsplit(substr(Ref, 15, (nchar(Ref)-4)), "/")))
# modified to correct for MD2500 alignment
    if(RA %in% c(0,180)) {
       AliX <- 5.5
       AliY <- 138.4
    }
    if(RA %in% c(90,270)) {
       AliX <- 14.0
       AliY <- 146.9    
    }
    RefX <- Ref[1]
    RefY <- Ref[2]
# end of added code - further code modifed bellow
    if (!is.numeric(stripNr)) stripNr <- 37
    Inp <- file(Fname, "r")
    tmp <- readLines(Inp, n = stripNr)
    Input <- readLines(Inp)
    Input <- gsub("~", " ", Input)
    close(Inp)
    Index <- as.numeric(substr(Input, 1, 5))
# check for empty defect file 
    if (length(Index)==0) 
    {
       data <- data.frame(Index=c(0), X=c(0), Y=c(0), SizeX=c(0),
          SizeY=c(0), Type=c(0), Kind=c(0), ChipX=c(0), ChipY=c(0))
       data <- data[-1,]
    } else {
# recalculation of defect coordinates to CATS coordinates 
# MD coordinate system is X-growing left to right, Y growing top to bottom
# readMD version 0.3.7 specific alignment and referenc recalculation
#       X1 <- as.numeric(substr(Input, 20, 28)) + AliX +RefX 
#       Y1 <- AliY - RefY - as.numeric(substr(Input, 30, 38))

#  alternative2 - matching previous defect position using readMD ver 0.3.6 and older
       X1 <- as.numeric(substr(Input, 20, 28))
       Y1 <- 152.0 - as.numeric(substr(Input, 30, 38))


# rotate data - counterclockwise!
       rotM <- matrix(c(cos(alpha), -sin(alpha), sin(alpha), cos(alpha)), ncol=2, nrow=2, byrow=TRUE)
#       V <- c(X1-76, Y1-76)
# alternative 
       V <- rbind(X1-76, Y1-76)

       Vr <- t(rotM %*% V) 
       X <- Vr[,1] +76
       Y <- Vr[,2] +76

#       X <- ((X1-76) * cos(alpha) - (Y1-76) * sin(alpha) + 76.0)
#       Y <- ((X1-76) * sin(alpha) + (Y1-76) * cos(alpha) + 76.0)

       ChipX <- as.numeric(substr(Input, 6, 10))
       ChipY <- as.numeric(substr(Input, 11, 16))
       SizeX <- as.numeric(substr(Input, 56, 62))
       SizeY <- as.numeric(substr(Input, 64, 70))
       Kind <- gsub(" ", "", substr(Input, 50, 53))
       Type <- max(SizeX, SizeY)
       data <- data.frame(Index, X, Y, SizeX, SizeY, Type, Kind, ChipX, ChipY)
       } # end of if condition for empty file
    return(data)
}

