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

#-----------------------------------------------------
# extended / modified version of paired DFX to accomodate alternative algorithm for 
# defect pair identification - more robust and possibly faster than current one - "TSP"
# distadvatage of alternative "SORT" algorithm is (rarely) assignment of not closest combination of
# defects, based on position of each defect in distance matrix calculated during evaluation  
#-----------------------------------------------------

pairedDFX <- function (data1, data2, dis = 100, crit = "distance", alg="SORT") 
{

# identify defects in XY1 which may be candidates for pair to XY2 defects 
#    get_candidates <- function(XY1, XY2) {
#        any(((XY2$X - XY1[1])^2 + (XY2$Y - XY1[2])^2) < D2)
#    }

#------------------------------------------------------
# function TSP - algorithm for defect pairs formation - exact but slow one
#------------------------------------------------------
 TSP <- function(candidates_1, candidates_2, criteria)
 { 
#first make copy of criteria table
    cr <- criteria
# remove rows and columns containing NA only
# remove defects without pair candidate and already resolved defects from criteria matrix
    rowremove <- which(apply(cr, 1, function(x){all(is.na(x))}))
    colremove <- which(apply(cr, 2, function(x){all(is.na(x))}))
    if (length(colremove)>0) cr <- cr [, -colremove]
    if (length(rowremove)>0) cr <- cr [-rowremove, ]
# and change to matrix, just in case
    cr <- matrix(cr, ncol=ncol(criteria)-length(colremove), nrow=nrow(criteria)-length(rowremove))
    if (length(rowremove)>0) rownames(cr) <- rownames(criteria)[-rowremove] else rownames(cr) <- rownames(criteria)
    if (length(colremove)>0) colnames(cr) <- colnames(criteria)[-colremove] else colnames(cr) <- colnames(criteria)


# and go on as in TSP in previous version e.g. darwintools 0.3.8
    pairs <- matrix(NA, ncol = 2, nrow = ncol(criteria), byrow = TRUE)
    for (i in 1:length(candidates_1)) {
       if (!(candidates_1[i] %in% pairs[, 1])) {
          rows <- which(!is.na(cr[, i]))
          temp_matrix <- matrix(cr[rows, ], nrow=length(rows), ncol=ncol(cr))

          cols <- which(apply(temp_matrix, 2, function(x) {any(!is.na(x))}))
          criteria_field <- matrix(cr[rows, cols], nrow=length(rows))
#needed later on - placeholders for rownames and columnames of criteria field---
          crit_Cnames <- as.numeric(colnames(cr)[cols])
          crit_Rnames <- as.numeric(rownames(cr)[rows])
          related <- dim(criteria_field)

          if (related[1] == 1 & related[2] == 1) {
             paired_index <- i
             pairs[paired_index, 1] <- candidates_1[cols]
             pairs[paired_index, 2] <- candidates_2[rows]
          } else { # table is not 1x1 
             if (related[2] > related[1]) { # table width > table height
# rotate criteria_field
                criteria_field <- t(criteria_field)
                crit_Rnames <- as.numeric(colnames(cr)[cols])
                crit_Cnames <- as.numeric(rownames(cr)[rows])
                DDD <- related[c(2,1)]
             } else DDD <- related
             B <- matrix(NA, ncol=DDD[2], nrow=DDD[1])
             B[,1:DDD[2]] <- 1:DDD[1]
             eval(parse(text= paste("D <- expand.grid(",
                paste( paste(rep(" criteria_field[,",DDD[2]), 1:DDD[2], rep("]",DDD[2]), sep=""),sep="",collapse=","), ")", sep=""))) 

             eval(parse(text= paste("ID <- expand.grid(",
                paste( paste(rep(" B[,",DDD[2]), 1:DDD[2], rep("]",DDD[2]), sep=""),sep="",collapse=","), ")", sep="")))
# find invalide rows, (since not completelly filled)
             skipcombination <- which(apply(D, 1, function(x){any(is.na(x))}))
             if (length(skipcombination)>0) {
                D <- D[-skipcombination,]
                ID <- ID[-skipcombination,]
             } 
# which lines contain indexes from  diferent items
             repeated <- which(apply(ID, 1, function(x) {any(duplicated(x))}))
# kick out lines with repeated indexes
             if (length(repeated)>0) {
                D <- D[-repeated,]
                ID <- ID[-repeated,]
             }
# estimate order of sums for all combinations
             Sumorder <- order(apply(D,1,sum))[1]
             favorite <- D[Sumorder,]
             favoriteID <- as.numeric(ID[Sumorder,])

             for (column_i in 1:length(favoriteID)) {
                 if(related[1]==DDD[1])
                 {
                    partner_i <- crit_Cnames[column_i]
                    partner_j <- crit_Rnames[favoriteID[column_i]]
                 } else{
                    partner_j <- crit_Cnames[column_i]
                    partner_i <- crit_Rnames[favoriteID[column_i]]
                 }
                 paired_index <- which(candidates_1 == partner_i)
                 pairs[paired_index, 1] <- partner_i
                 pairs[paired_index, 2] <- partner_j
             }
          } # end of table is not 1x1  
       } #end of if (candidates %in% pairs[,1])...."
    } # end of "for (i in 1:length(candidates_1..."
# kick out empty rows 
    pairs <- pairs[which(!is.na(pairs[, 1])), ]
    pairs <- matrix(as.numeric(pairs), ncol=2, nrow=(length(as.numeric(pairs))%/%2))
    colnames(pairs) <- c("data1", "data2")
    return(pairs)
 } # end of TSP function


#------------------------------------------------------
# function SORT - algorithm for defect pairs formation - fast one, but not as exact as TSP
#------------------------------------------------------
 SORT <- function(criteria)
 {
# start of SORT main code
    cr <- criteria
#--------------------------------
    pairs1 <- matrix(NA, ncol=2, nrow=max(dim(cr)))

    crtemp <- cr 
    crtemp <- (crtemp+1) / (crtemp+1)
    crtemp[which(is.na(crtemp))] <- 0
# keep only rows and columns containing exactly 1 pair candidate 
# which rows/ columns shall remain
    unit_vector2 <- rep(1, dim(cr)[2])
    row1 <- crtemp %*% unit_vector2
#    pairs1mat <- crtemp[row1==1,]
# and change to matrix, just in case
    pairs1mat <- matrix(crtemp[row1==1,], nrow=length(which(row1==1)), ncol=ncol(crtemp))
    rownames(pairs1mat) <- rownames(cr)[row1==1]
    colnames(pairs1mat) <- colnames(cr)

    unit_vector1 <- rep(1, dim(pairs1mat)[1])
    col1 <- unit_vector1 %*% pairs1mat
#    pairs1mat <- pairs1mat[ , col1==1]
# and change to matrix, just in case
    pairs1mat <- matrix(pairs1mat[ , col1==1], nrow=nrow(pairs1mat), ncol=length(which(col1==1)) )
    rownames(pairs1mat) <- rownames(cr)[row1==1]
    colnames(pairs1mat) <- colnames(cr)[col1==1]

    Mdim <- dim(pairs1mat)
    if(all(Mdim>0))# empty amtrix resulting from no 1/1 defect pair presence
       if(Mdim[1] > Mdim[2]) # more rows than columns
       {
          rvect <- seq(1, Mdim[1], 1)
          row1ID <- rvect %*% pairs1mat 
          pairs1[1:Mdim[2],1] <- colnames(pairs1mat)
          pairs1[1:Mdim[2],2] <- rownames(pairs1mat)[row1ID]
       } else {
          cvect <- seq(1, Mdim[2], 1)
          col1ID <- pairs1mat %*% cvect 
          pairs1[1:Mdim[1],2] <- rownames(pairs1mat)
          pairs1[1:Mdim[1],1] <- colnames(pairs1mat)[col1ID]
       }
     
# single defect to single defect assignment resolved / finished
# remove defects without pair candidate and already resolved defects from criteria matrix
    unit_vector1 <- rep(1, dim(crtemp)[1])
    unit_vector2 <- rep(1, dim(crtemp)[2])
    col1 <- as.vector(unit_vector1 %*% crtemp)
    row1 <- as.vector(crtemp %*% unit_vector2)

    rowremove <- c( which(row1==0), which(rownames(cr)%in% pairs1[,2]))
    colremove <- c( which(col1==0), which(colnames(cr)%in% pairs1[,1]))
    if (length(colremove)>0 & class(cr)=="matrix") cr <- cr[, -colremove]
    if (length(rowremove)>0 & class(cr)=="matrix") cr <- cr[-rowremove, ]
# start search for pairs in more complicated cases
# more than simple pair of two defect

# identify clusters
    Clusters <- getClusters(cr)

# create matrix for defect pairs 
    pairs2 <- matrix(NA, ncol=2, nrow=2)

# loop over clusters + identification of pairs
    if(length(Clusters)>0)
    {
       for(CID in 1:length(Clusters))
       {
          temp_pairs <- resolveClusterSORT(Clusters[[CID]])
          pairs2 <- rbind(pairs2, temp_pairs)
       }
    } 
    pairs <- rbind(pairs1, pairs2)
    pairs <- pairs[which(!is.na(pairs[,1])),]
    pairs <- matrix(as.numeric(pairs), ncol=2, nrow=(length(as.numeric(pairs))%/%2))
    colnames(pairs) <- c("data1", "data2")
    return(pairs)
 } # end of SORT function
#------------------------------------------



# start of evaluation script 
    if (!(crit %in% c("size", "distance"))) stop("Error. crit parameter unknown!")

# get criteria table
    criteria <- pairCriteriaTable(data1, data2, dis=dis, crit=crit)

# no candidates for paires found 
  if (is.null(criteria))  
  {
     pairs <- data.frame(data1 =c(), data2=c())
     return(pairs)
     break()
  }
    if (alg=="TSP") {
# identify pair candidates in data1 
       candidates_1 <- which(apply(criteria, 2, function(x){!all(is.na(x))}))
# identify pair candidates in data2 
       candidates_2 <- which(apply(criteria, 1, function(x){!all(is.na(x))}))
# candidates paires identified
       pairs <- TSP(candidates_1, candidates_2, criteria)
    }
    if (alg=="SORT") pairs <- SORT(criteria)
    return(pairs)
}
