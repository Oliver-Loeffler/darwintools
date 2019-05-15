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

readLTinfo <- function (Fname){
  LTpath <- "T:/Tools/DDE166/01_Results/csv-Files"
  
  if (!file.exists(Fname) & file.exists(file.path(LTpath, Fname))) 
    Fname <- file.path(LTpath, Fname)
  if (file.exists(Fname)) {
    
    Inp <- file(Fname, "r")
    tmp <- readLines(Inp)
    Encoding(tmp) <- "latin1"
    close(Inp)
    options(warn = -1)
    
  } else stop(paste(Fname, " file not existing!", sep = ""))   
  
  #find lines to read info
  empty_lines <- which(tmp == "")
  begin_common <- which(tmp == "@Common")+1
  end_common <- min(empty_lines)-1
  data <- list()
  data[[1]] <- tmp[begin_common:end_common]
  
  begin_front <- which(tmp == "@Front")+1   #will be numeric(0), if the value does not exist
  begin_back  <- which(tmp == "@Back")+1
  
  # fill Result list with raw csv format
  if(length(begin_front)){
    end_front <- min(which(tmp == "InspectionDataList"))-1
    data[[2]] <- tmp[begin_front:end_front]
  }
  if(length(begin_back)){
    end_back <- max(which(tmp == "InspectionDataList"))-1
    data[[length(data)+1]] <- tmp[begin_back:end_back]
  }
  names(data) <- c("Common", if(length(begin_front))"Front", if(length(begin_back))"Back" )
  options(warn = 0)
  
  #slice data: csv to columns
  for(i in 1:length(data)){
    tmp2 <- data[[i]]
    rownames <- c(); values <- c()
    for(j in 1:length(tmp2)){
      rownames[j] <- unlist(strsplit(tmp2[j], ","))[1]
      values[j] <- paste(unlist(strsplit(tmp2[j], ","))[-1], collapse= "; ")
    }
    if(i==1){data[[i]] <- list(rownames, values) #@common parameters are returned each
    }else{#@front and back are returned as table
      data[[i]] <- data.frame(matrix(values, nrow=1)) 
      data[[i]][] <- lapply(data[[i]], as.character) # convert parts of the data.frame from factor to character
      colnames(data[[i]]) <- rownames
    }
  } # end for i
  
  #get number of defects
  #####
  defects <- readLT(Fname)
  types <- unique(defects$Type)
  defects_front  <- defects[which(defects$Side=="Front"),]
  defects_back <- defects[which(defects$Side=="Back"),]
  
  count <- matrix(NA, nrow=2, ncol=length(types)+1)
  for(i in 1:length(types)){
    count[1,i] <- length(which(defects_front$Type==types[i]))
    count[2,i] <- length(which(defects_back$Type==types[i]))
  }
  count[1,ncol(count)] <- nrow(defects_front)
  count[2,ncol(count)] <- nrow(defects_back)
  colnames(count) <- c(as.character(types), "Total")
  rownames(count) <- c("Front", "Back")
  
  #output of Result
  #####
  # four cases: front+back, front only, back only, none
  if(length(begin_front) & length(begin_back) ){
  # entries of @Common are saved each, Front and Back in a sublist
  Result <- list(Job=unlist(data[[1]][2])[1], Pod=unlist(data[[1]][2])[2], BlankSerial=unlist(data[[1]][2])[3], 
                 ModelName=unlist(data[[1]][2])[4], Recipe=unlist(data[[1]][2])[5], Notches=unlist(data[[1]][2])[6], 
                 LoadCasseteName=unlist(data[[1]][2])[7], LoadSlot=unlist(data[[1]][2])[8], UnloadCassetteName=unlist(data[[1]][2])[9], 
                 UnloadSlot=unlist(data[[1]][2])[10], UnloadedSide=unlist(data[[1]][2])[11], Binning=unlist(data[[1]][2])[12], 
                 BinningReason=unlist(data[[1]][2])[13], DefectCount=as.data.frame(count), Front=data[[2]], Back=data[[3]] )
} else if(length(begin_front) & !length(begin_back) ){
  Result <- list(Job=unlist(data[[1]][2])[1], Pod=unlist(data[[1]][2])[2], BlankSerial=unlist(data[[1]][2])[3], 
                 ModelName=unlist(data[[1]][2])[4], Recipe=unlist(data[[1]][2])[5], Notches=unlist(data[[1]][2])[6], 
                 LoadCasseteName=unlist(data[[1]][2])[7], LoadSlot=unlist(data[[1]][2])[8], UnloadCassetteName=unlist(data[[1]][2])[9], 
                 UnloadSlot=unlist(data[[1]][2])[10], UnloadedSide=unlist(data[[1]][2])[11], Binning=unlist(data[[1]][2])[12], 
                 BinningReason=unlist(data[[1]][2])[13], DefectCount=as.data.frame(count), Front=data[[2]] )
} else if(!length(begin_front) & length(begin_back) ){
  Result <- list(Job=unlist(data[[1]][2])[1], Pod=unlist(data[[1]][2])[2], BlankSerial=unlist(data[[1]][2])[3], 
                 ModelName=unlist(data[[1]][2])[4], Recipe=unlist(data[[1]][2])[5], Notches=unlist(data[[1]][2])[6], 
                 LoadCasseteName=unlist(data[[1]][2])[7], LoadSlot=unlist(data[[1]][2])[8], UnloadCassetteName=unlist(data[[1]][2])[9], 
                 UnloadSlot=unlist(data[[1]][2])[10], UnloadedSide=unlist(data[[1]][2])[11], Binning=unlist(data[[1]][2])[12], 
                 BinningReason=unlist(data[[1]][2])[13], DefectCount=as.data.frame(count), Back=data[[2]] )
} else {
  Result <- list(Job=unlist(data[[1]][2])[1], Pod=unlist(data[[1]][2])[2], BlankSerial=unlist(data[[1]][2])[3], 
                 ModelName=unlist(data[[1]][2])[4], Recipe=unlist(data[[1]][2])[5], Notches=unlist(data[[1]][2])[6], 
                 LoadCasseteName=unlist(data[[1]][2])[7], LoadSlot=unlist(data[[1]][2])[8], UnloadCassetteName=unlist(data[[1]][2])[9], 
                 UnloadSlot=unlist(data[[1]][2])[10], UnloadedSide=unlist(data[[1]][2])[11], Binning=unlist(data[[1]][2])[12], 
                 BinningReason=unlist(data[[1]][2])[13], DefectCount=as.data.frame(count) )
}
  
return(Result)
  
} #end function
