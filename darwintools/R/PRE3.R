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

#---------------------------
# calculation of particle removal efficiency from 3 scans pre - before contamination, coat - after particler application
# and post- post clean. This evaluation assumes no ( negligible number of) defects 
# before particle application and/or only removable defects on teh blank.
# In other cases PRE3 funstions needs to be applied.
#---------------------------
 PRE3 <- function(pre, coat, post, slim=NULL)
 {
# check if raw DFX data or similar are provided as parameter( e.g. KLA data)
    if((class(coat)!=class(post) | class(coat)!=class(pre)) | 
      ((any(names(coat)!=names(post)) | any(names(coat)!=names(pre)))
     & ((class(pre)!= "matrix" | class(coat)!= "matrix" ) | class(post)!= "matrix") )) stop("\n Data types differ! \n")
# here for data frames with columns X,Y
    if(all((c("X", "Y") %in% names(coat))) & all(names(coat)==names(post)) & all(names(coat)==names(pre)) )
    {
       if(!is.null(slim)) 
       {
          if(length(slim)==2){
             PreC <- length(which(pre$Size> slim[1] & pre$Size < slim[2]))
             CoatC <- length(which(coat$Size> slim[1] & coat$Size < slim[2]))
             PostC <- length(which(post$Size> slim[1] & post$Size < slim[2]))
             PRE <- (CoatC-PostC)/(CoatC-PreC)*100
             if(CoatC < PostC) PRE <- -abs(PRE)
          }else stop("\n 2 slim limits expected! \n")
       } else {
          PRE <- (dim(coat)[1]-dim(post)[1])/(dim(coat)[1] -dim(pre)[1])*100
       } # end of is.null(slim)
    } # end of  evaluation for DFX raw data
# evaluation for defect density maps created using e.g. by function dmap
    if((all(names(coat)==names(post)) & all(names(coat)==names(pre))) & any(sapply(coat, class)=="matrix"))
    {
       PRE <- coat
       ID <- which(sapply(coat, class)=="matrix")
       PRE[[ID]] <- coat[[ID]]-post[[ID]]
       enum <- PRE[[ID]]
       PRE[[ID]][which(PRE[[ID]] < 0)] <- 0
       PRE[[ID]] <-  PRE[[ID]]/ (coat[[ID]] - pre[[ID]])*100
       PRE[[ID]][which(enum < 0)] <- 0
       PRE[[ID]][which(is.na(PRE[[ID]]))] <- 0
       PRE[[ID]][which(PRE[[ID]]==Inf)]<-(enum/coat[[ID]])[which(PRE[[ID]]==Inf)]*100
       PRE[[ID]][which(PRE[[ID]]<0)] <- (enum/coat[[ID]])[which(PRE[[ID]]<0)]*100
    }
# evaluation of matrix data only - obtained ??
    if((class(coat)=="matrix" & class(post)=="matrix") & class(pre)=="matrix" )
    {
       PRE <- (coat-post)
       enum <- PRE
       PRE[which(PRE < 0)] <- 0
       PRE <-  PRE/(coat - pre)*100
       PRE[which(is.na(PRE))] <- 0
       PRE[which(PRE==Inf)] <- (enum/coat)[which(PRE==Inf)] *100
       PRE[which(PRE<0)] <- (enum/coat)[which(PRE<0)]*100
    }
    if (exists("PRE")) return(PRE)
 }