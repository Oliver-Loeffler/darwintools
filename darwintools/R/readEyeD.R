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

readEyeD <- function (FName)
{
    Fsize <- file.info(FName)$size
    sp.header <- 128
    input <- file(FName, "rb")
    fill <- readBin(input, integer(), 1)
    data.size <- readBin(input, integer(), signed = FALSE, 1)
    header.offset <- readBin(input, integer(), 2)
    header.frames <- readBin(input, integer(), 2)
    fill <- readBin(input, raw(), header.offset[1] - 24)
    sp_data <- readBin(input, raw(), data.size - header.offset[1])
#    sp.setup <- readBin(input, raw(), 100)
    sp.setup <- readBin(input, raw(), Fsize-data.size)

    close(input)
    startWL <- readBin(sp.setup[29:32], integer(), 1, signed = FALSE,
        size = 4)
    endWL <- readBin(sp.setup[33:36], integer(), 1, signed = FALSE,
        size = 4)
    itime <- readBin(sp.setup[17:20], integer(), 1, signed = FALSE,
        size = 4)
    channels <- (endWL - startWL) * 2 + 1
    spectra.size <- channels * 2 + sp.header
    spectra.count <- (data.size - header.offset[1])/spectra.size
    spectra.sources <- spectra.count/header.frames[1]
    spectra <- array(NA, dim = c(header.frames[1], channels,
        spectra.sources))
    tstamp <- array(NA, dim = c(header.frames[1], spectra.sources))
    time <- array(NA, dim = c(header.frames[1], spectra.sources))
    IDframe <- array(NA, dim = c(header.frames[1], spectra.sources))
    for (i in 0:(header.frames[1] - 1)) {
        for (j in 0:(spectra.sources - 1)) {
            jump <- (i * spectra.sources + j) * spectra.size
            tstamp[i + 1, j + 1] <- rawToChar(sp_data[(jump +
                1):(jump + 21)])
            time[i + 1, j + 1] <- readBin(sp_data[(jump + 41):(jump +
                44)], integer(), 1, )
            IDframe[i + 1, j + 1] <- readBin(sp_data[(jump +
                45):(jump + 49)], integer(), 1)
            spectra[i + 1, , j + 1] <- readBin(sp_data[(jump +
                129):(jump + 129 + channels * 2)], integer(),
                channels, signed = FALSE, size = 2)
        }
    }
    WL <- seq(startWL, endWL, 0.5)
    Stamp <- tstamp[1]
    AGC <- 0
###################################################
## experimental part for AGC read out
    # find AGC if present
    TXTdef <- "Integration altered from"
    TXTI <- charToRaw(TXTdef)
#    Sb <- readBin(sp.setup, integer(), length(sp.setup), signed = FALSE, size = 1) 
#    Sb <- readLines(sp.setup, n=500)
    IDs <- which(sp.setup==TXTI[1])
    for(i in 2:length(TXTI))
    {
        IDs <- IDs+1
        IDs <- IDs[which(sp.setup[IDs]==TXTI[i])]
    }
    if(length(IDs)>0) 
    {
       AGCTXT <- readBin(sp.setup[IDs[1]+2:(IDs[1]+20)], "character", 1)
       AGCTXT <- unlist(strsplit(unlist( strsplit(AGCTXT,"ms"))," "))
      options(warn = -1)
      AGCTXT <- na.omit( as.numeric(AGCTXT)) 
      if(length(AGCTXT)>1) AGC<- AGCTXT[2]
      options(warn = 0)

    }
######################################################

    ret <- list(WL, time/1000, spectra, IDframe, spectra.count,
        Stamp, spectra.sources, itime, AGC)
    names(ret) <- c("WL", "t", "SP", "TF", "SpectraCount", "TStamp",
       "SourceCount",  "IT", "AGC")
    return(ret)
}
