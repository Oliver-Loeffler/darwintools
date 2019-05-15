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

readDFX <- function (Fname, Dread = 0:5, slim = NULL)
{
    DFXpath <- "T:/Tools/DDE102/inspection_files"
    if (!file.exists(Fname) & file.exists(file.path(DFXpath, Fname))) 
        Fname <- file.path(DFXpath, Fname)
    if (file.exists(Fname)) {
        IDs <- list(A = c(68, 101, 102, 101, 107, 116, 101, 32, 
            76, 97, 99, 107, 115, 101, 105, 116, 101), B = c(80, 
            105, 110, 104, 111, 108, 101, 115, 32, 67, 104, 114, 
            111, 109), C = c(69, 105, 110, 115, 99, 104, 108, 
            252 , 115, 115, 101, 32, 67, 104, 114, 111, 
            109), D = c(68, 101, 102, 101, 107, 116, 101, 32, 
            71, 108, 97, 115, 115, 101, 105, 116, 101), E = c(69, 
            105, 110, 115, 99, 104, 108, 252, 115, 115, 101, 32, 71,
            108, 97, 115))
        IDC <- sapply(IDs, function(x) {rawToChar(as.raw(x))})
        Encoding(IDC) <- "latin1"
        Inp <- file(Fname, "r")
        tmp <- readLines(Inp, n = 200)
        Encoding(tmp) <- "latin1"
        close(Inp)
        options(warn = -1)
        stripNr <- min(which(tmp[] %in% IDC))
        options(warn = 0)
        if (!is.numeric(stripNr)) 
            stripNr <- 19
    } else stop(paste(Fname, " file not existing!", sep = ""))     
    
    col.names <- c("Id", "Nr", "Size", "X", "Y", "Status", "Status2")
    what = list("", "", "", "", "", "", "")
    names(what) <- col.names
    options(warn = -1)
    data <- scan(Fname, skip = stripNr, what, fill = TRUE, flush = TRUE, 
        quiet = TRUE)
    options(warn = 0)
    for (i in 1:5) {
        data[[i]] <- type.convert(data[[i]], as.is = TRUE, dec = ",")
    }
    row.names <- as.character(seq_len(length(data[[1]])))
    class(data) <- "data.frame"
    row.names(data) <- row.names
    options(warn = -1)
    data[, 1] <- as.numeric(data[, 1])
    data[, 2] <- as.numeric(data[, 2])
    options(warn = 0)
    data <- data[which(data[[1]] %in% Dread), ]
    if (!is.null(slim)) {
        if (is.numeric(slim) & length(slim == 2)) 
            data <- data[which(data$Size > slim[1] & data$Size < 
                slim[2]), ]
    }
    data$Status <- paste(data$Status, data$Status2)
    belongs <- rep(1, length(data$X))
    data <- cbind(data[,1:6], belongs)
    return(data)
}
