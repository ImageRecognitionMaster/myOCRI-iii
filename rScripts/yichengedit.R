##==========================================
##  kinship-help.R
##  Dataset is provided called: emma.rda
##==========================================

#change to a directory,
setwd("C:/Users/Yicheng/Downloads")

#load the dataset
load("emmadat.rda")
str(emmadat)
dim(emmadat$xs)
dim(emmadat$ys)
dim(emmadat$K)

snps <- emmadat$xs


##============================
##  Function to be run first
#Run the following function
emma.kinship <- function(snps, method="additive", use="all") {
  #assigning some values and testing if the matrix is made up of only 0, 0.5
  #1, and NA values
  n0 <- sum(snps==0,na.rm=TRUE)
  nh <- sum(snps==0.5,na.rm=TRUE)
  n1 <- sum(snps==1,na.rm=TRUE)
  nNA <- sum(is.na(snps))
  
  stopifnot(n0+nh+n1+nNA == length(snps))
  
  #flags is testing row means, dominant and recessive are disjoint events
  #flags takes all rowmeans, if true as.double assigns value of 1
  #if false, as.double assigns value of 0
  #then, that vector of 0's and 1's is repeated 110 times (number of columns in matrix)
  #so the flags matrix has 110 (original matrix number of columns) identical columns
  
  if ( method == "dominant" ) {
    flags <- matrix(as.double(rowMeans(snps,na.rm=TRUE) > 0.5),nrow(snps),ncol(snps))
    snps[!is.na(snps) & (snps == 0.5)] <- flags[!is.na(snps) & (snps == 0.5)]
  }
  else if ( method == "recessive" ) {
    flags <- matrix(as.double(rowMeans(snps,na.rm=TRUE) < 0.5),nrow(snps),ncol(snps))
    snps[!is.na(snps) & (snps == 0.5)] <- flags[!is.na(snps) & (snps == 0.5)]
  }
  else if ( ( method == "additive" ) && ( nh > 0 ) ) {
    dsnps <- snps
    rsnps <- snps
    flags <- matrix(as.double(rowMeans(snps,na.rm=TRUE) > 0.5),nrow(snps),ncol(snps))
    dsnps[!is.na(snps) & (snps==0.5)] <- flags[!is.na(snps) & (snps==0.5)]
    flags <- matrix(as.double(rowMeans(snps,na.rm=TRUE) < 0.5),nrow(snps),ncol(snps))
    rsnps[!is.na(snps) & (snps==0.5)] <- flags[!is.na(snps) & (snps==0.5)]
    snps <- rbind(dsnps,rsnps)
  }
  
  #this option uses the rowMeans in a similar way to flags
  #the rowMeans of the original matrix is turned into a column vector
  #and repeated by 110 (number of columns of orig matrix)
  #the way of getting rid of NA values.
  if ( use == "all" ) {
    mafs <- matrix(rowMeans(snps,na.rm=TRUE),nrow(snps),ncol(snps))
    snps[is.na(snps)] <- mafs[is.na(snps)]
  }
  #this option uses all non-NA values without computing rowMeans
  else if ( use == "complete.obs" ) {
    snps <- snps[rowSums(is.na(snps))==0,]
  }
  
  #creates an 110 x 110 matrix of all NA's, except diagonals are 1
  n <- ncol(snps)
  K <- matrix(nrow=n,ncol=n)
  diag(K) <- 1
  
  #The line assigning x is the most difficult line.
  #The loop itself runs through every NON-DIAGONAL ELEMENT in matrix K
  #I am not sure of the context of the data of the original matrix.
  #But since it is a matrix uniquely made up of 0's and 1's
  #Taking 1-(any subset of the matrix) results in a switch of all digits.
  #Example: 1-(0,1,0,1) becomes (1,0,1,0)
  #Also, if two subsets are multiplied, by laws of matrix multiplication
  #(a, b, c, d)*(e, f, g, h) == (ae, bf, cg, dh)
  #Line x, part 1, is turning all pairs of non-diagonals in (ae, bf, cg, dh) where if
  #and ONLY if both numbers in the pair were originally 1
  #will ae, bf, cg, dh result in 1, if there was a 0, the result will be 0
  #since 1*0 = 0 and 0*0 = 0, but 1*1 = 1
  #Line x, part 2, is doing the same, but for the switched versions
  #remember, (0,1,0,1) would become (1,0,1,0)
  #Then, the two parts are added. What remains 1 are the pairs where either
  #originally both had 1, or both had 0, remember since 1-1 = 0
  #Then, in the next line, the RATIO of 1 to 1 and 0 to 0 correspondence is found
  #So K will become a matrix of RATIOS of 1 to 1 and 0 to 0 correspondence divided by
  #TOTAL(1 to 1, 0 to 1, 1 to 0, 0 to 0)
  #Then, to save code time, the work is cut in half and the same result is posted in K[i,j] to K[j,i]
  #The end result K matrix is a matrix where transpose(K) == K
  for(i in 2:n) {
    for(j in 1:(i-1)) {
      x <- snps[,i]*snps[,j] + (1-snps[,i])*(1-snps[,j])
      K[i,j] <- sum(x,na.rm=TRUE)/sum(!is.na(x))
      K[j,i] <- K[i,j]
    }
  }
  return(K)
}

x <- snps[,2]*snps[,1] + (1-snps[,2])*(1-snps[,1])
K[2,1] <- sum(x,na.rm=TRUE)/sum(!is.na(x))
K[1,2] <- K[2,1]

K <- emma.kinship(emmadat$xs,"additive","all")
