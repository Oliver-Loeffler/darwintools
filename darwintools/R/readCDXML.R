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

#------------------------------------------
# read CD SEM data from Fname XML file
#------------------------------------------
readCDXML <- function(Fname)
{
   raw <- readLines(Fname)
   doc <- xmlTreeParse(paste(raw,collapse=""), useInternalNodes=TRUE)	
   Index <- xpathSApply(doc,"//MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/CODE/ID", xmlValue)
   SubIndex <- xpathSApply(doc,"/MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/SUB_ID", xmlValue)
   X <- xpathSApply(doc,"//MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/X",xmlValue)
   Y <- xpathSApply(doc,"//MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/Y",xmlValue)
   XDie <- xpathSApply(doc,"//MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/X_DIE",xmlValue)
   YDie <- xpathSApply(doc,"//MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/Y_DIE",xmlValue)
   CD_Name <- xpathSApply(doc,"//MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/CODE/CDNAME",xmlValue)
   Orientation <- xpathSApply(doc,"//MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/CODE/ORIENTATION",xmlValue)
   Bottom <- xpathSApply(doc,"//MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/VALUE/CD",xmlValue)
#   Top_Bottom <- xpathSApply(doc,"//MEASUREMENT_PROCESS//MEASUREMENT/SITES/SITE/VALUE/TOP_BOTTOM",xmlValue)
   Type <- xpathSApply(doc,"//MEASUREMENT_PROCESS/MEASUREMENT/SITES/SITE/CODE/TYPE",xmlValue)
   free(doc)

   Index <- as.numeric(Index)
   SubIndex <- as.numeric(SubIndex)
   Bottom <- as.numeric(Bottom)
#   Top_Bottom <- as.numeric(Top_Bottom)
   X <- as.numeric(X)
   Y <- as.numeric(Y)
   XDie <-as.numeric(XDie)
   YDie <-as.numeric(YDie)
   res <- data.frame(Index=Index,SubIndex=SubIndex, 
       X=X, Y=Y, Bottom=Bottom, Target=CD_Name, Type=Type, 
       Orientation=Orientation, XDie=XDie, YDie=YDie) 
   return(res) 
}
