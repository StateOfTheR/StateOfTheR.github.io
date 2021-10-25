#### This program exemplifies the use of the "big" packages on the Arabidopsis dataset.
#### The different parts of the program can be run independently.
#### Required packages are specified at the beginning of each part, 
#### along with the work directory.
#### WARNING: if you want to re-run this program on the Arabidopsis dataset,
### you'll need... the dataset, that is big (500Mo) and not provided with this code...


        ################################################          
        #### 
        ####    Part I: the usual way
        #### 
        ################################################          


rm(list=ls())
setwd('D:/R/StateOfTheR/BigData')
Times.usual <- matrix(0,100,3)
Names.usual <- rep(NA,100)
cpt <- 1
        

        #### Load packages


library(data.table)


        #### Load the data


#Enhanced version: using fread
ptm <- proc.time()
Arabidopsis <- fread('./Data/Arabidopsis/Arabidopsis.txt',header=F,sep=';',colClasses = 'integer')
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Load with fread'
cpt <- cpt+1
format(object.size(Arabidopsis),units='Mb')

#Naive version: using read.table
ptm <- proc.time()
Arabidopsis <- as.matrix(read.table('./Data/Arabidopsis/Arabidopsis.txt',header=F,sep=';',colClasses = 'integer'))
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Load with read.table'
cpt <- cpt+1
colnames(Arabidopsis) <- paste0("L",1:ncol(Arabidopsis))
rownames(Arabidopsis) <- paste0("M",1:nrow(Arabidopsis))
Arabidopsis[1:5,1:10]
format(object.size(Arabidopsis),units='Mb')


        #### Compute allelic frequencies


#With apply
ptm <- proc.time()
Freq <- apply(Arabidopsis,2,mean)
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Freq with apply'
cpt <- cpt+1

#With sapply
ptm <- proc.time()
Freq <- sapply(1:nrow(Arabidopsis),function(ii) mean(Arabidopsis[ii,]))
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Freq with sapply'
cpt <- cpt+1

#With rowMeans
ptm <- proc.time()
Freq <- rowMeans(Arabidopsis)
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Freq with rowMeans'
cpt <- cpt+1

#With some algebra
ptm <- proc.time()
Freq <- as.matrix(Arabidopsis)%*%rep(1,ncol(Arabidopsis))
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Freq with algebra'
cpt <- cpt+1


        #### Perform filtering


#With rowMeans
ptm <- proc.time()
MinMaf <- 0.05
MafFilter <- Freq > MinMaf | Freq < 1-MinMaf
Filtered <- Arabidopsis[MafFilter,]
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Filtering'
cpt <- cpt+1


        #### Compute the kinship matrix (IBS style) and its inverse


##Compute kinship
ptm <- proc.time()
Kinship <- (crossprod(Filtered) + crossprod(1-Filtered))/nrow(Filtered)
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Kinship'
cpt <- cpt+1

##Compute inverse using solve
ptm <- proc.time()
InvKinship <- solve(Kinship)
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Direct inversion'
cpt <- cpt+1

##Compute inverse using svd
ptm <- proc.time()
SVD <- svd(Kinship)
InvKinship <- tcrossprod(SVD$u*(rep(1,length(SVD$d))%*% matrix(1/sqrt(SVD$d),nrow=1)))
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Inversion from svd 1'
cpt <- cpt+1

##Compute inverse using chol
ptm <- proc.time()
InvKinship <- chol2inv(chol(Kinship))
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Inversion from choleski'
cpt <- cpt+1

Summary <- Times.usual[1:(cpt-1),] 
rownames(Summary) <- Names.usual[1:(cpt-1)]
saveRDS(Summary,file='./Results/Summary_UsualWay.rds')



        ################################################          
        #### 
        ####    Part II: the chunked way
        #### 
        ################################################          


rm(list=ls())
setwd('D:/R/StateOfTheR/BigData')
Times.seg <- matrix(0,100,3)
Names.seg <- rep(NA,100)
cpt <- 1


        #### Required packages


## NONE !!!


        #### Compute frequencies and kinship

trial <- file('./Data/Arabidopsis/Arabidopsis.txt')
NbSnp <- 214051
NbInd <- 1307
NbSnpPerRound <- 20000
MinMaf <- 0.05
Remains <- NbSnp%%NbSnpPerRound
NbRound <- (NbSnp-Remains)/NbSnpPerRound
Kinship <- matrix(0,NbInd,NbInd)
NbSnpForKinship <- 0
ptm <- proc.time()
open(trial)
Freq <- sapply(c(rep(NbSnpPerRound,NbRound),Remains), function(nblines){
  Don <- matrix(scan(trial,skip=0,nlines=nblines,quiet=T,sep=';'),nrow=nblines,byrow = TRUE)
  FreqLoc <- rowMeans(Don)
  MafFilter <- FreqLoc > MinMaf | FreqLoc < 1-MinMaf
  Kinship <<- Kinship + crossprod(Don[MafFilter,]) + crossprod(1-Don[MafFilter,])
  NbSnpForKinship <<- NbSnpForKinship + sum(MafFilter)
  return(FreqLoc)
})
close(trial)
Times.seg[cpt,] <- (proc.time()-ptm)[1:3]
Names.seg[cpt] <- 'Inversion from choleski'
cpt <- cpt+1
Kinship <- Kinship/NbSnpForKinship  
Kinship[1:5,1:5]


Summary <- Times.seg[1:(cpt-1),,drop=FALSE] 
rownames(Summary) <- Names.seg[1:(cpt-1)]
saveRDS(Summary,file=paste0('./Results/Summary_ChunkedWay_',NbSnpPerRound,'.rds'))



          ################################################          
          #### 
          ####    Part III: the bigmemory way
          #### 
          ################################################          


rm(list=ls())
setwd('D:/R/StateOfTheR/BigData')
Times.big <- matrix(0,100,3)
Names.big <- rep(NA,100)
cpt <- 1


          #### Required packages


library(bigmemory)
library(biganalytics)
library(dplyr)

##Note: if you want the latest version of bigalgebra, you MUST download it
## from github. Parts of the following code may not run if done otherwise...
library(devtools)
install_github('cdeterman/bigalgebra')
library(bigalgebra)



        #### Load data from a file


## Load the data
ptm <- proc.time()
Arabidopsis <- read.big.matrix("./Data/Arabidopsis/Arabidopsis.txt", 
                               type ="double", header = F,sep=';') 
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Load with read.big.matrix'
cpt <- cpt+1
NbSnp <- nrow(Arabidopsis)
NbLines <- ncol(Arabidopsis)


        #### Compute Freq, filter on MAF and compute kinship


##Note: pay attention to the class of the objects created by the different codelines.

##Compute frequencies using colmean
ptm <- proc.time()
Freq <- Arabidopsis %>% t %>% colmean %>% as.big.matrix
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Freq with colmean'
cpt <- cpt+1
##Note: Have a look at your tmp file when doing this. For Windows users, 
## is something like
## C:\Users\--YourUserName--\AppData\Local\Temp
## One can observe that a backup for t(Arabidopsis) is created on disk, 
## making this chunk of code not that attractive in terms of disk storage usage.
## Also means that if your re-run this code, another backup file will be created...
##Note: the resulting object is a big.matrix pointer.

##Compute frequencies using apply
ptm <- proc.time()
Freq <- biganalytics::apply(Arabidopsis,1,mean)
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Freq with bigalgebra::apply'
cpt <- cpt+1
##Note: really unefficient
##Note: the resulting object is a vector in R!!!

##Compute frequencies using rowMeans
ptm <- proc.time()
Freq <- rowMeans(Arabidopsis[,])
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Freq with rowMeans'
cpt <- cpt+1
##Note: the resulting object is a vector in R (as expected).

##Compute frequencies using algebra
ptm <- proc.time()
Freq <- Arabidopsis[,]%*%rep(1/NbLines,NbLines)
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Freq with algebra'
cpt <- cpt+1
##Note: the resulting object is a vector in R (as expected).

##Compute frequencies using big algebra
ptm <- proc.time()
Freq <- Arabidopsis%*%as.big.matrix(rep(1/NbLines,NbLines))
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'freq with bigalgebra'
cpt <- cpt+1
##Note: the resulting object is a big.matrix pointer.


        #### Perform Filtering


ptm <- proc.time()
MinMaf <- 0.05
MafFilter <- Freq[,] > MinMaf | Freq[,] < 1-MinMaf
Filtered <- as.big.matrix(Arabidopsis[MafFilter,])
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Filtering'
cpt <- cpt+1


        #### Compute the kinship matrix (IBS style) and its inverse


## RAM version
#Kinship matrix
ptm <- proc.time()
Kinship <- (crossprod(Filtered) + crossprod(1-Filtered))/nrow(Filtered)
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Kinship, RAM version'
cpt <- cpt+1
##Note: the resulting object is a big.matrix pointer.

#Inverse with solve
ptm <- proc.time()
InvKin <- solve(Kinship[,])
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Inverse with solve, RAM version'
cpt <- cpt+1

#Inverse with chol
ptm <- proc.time()
InvKin <- chol2inv(chol(Kinship)[,])
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Inverse with chol, RAM version'
cpt <- cpt+1

## Local version
#Kinship matrix
ptm <- proc.time()
Kinship <- (crossprod(Filtered[,]) + crossprod(1-Filtered[,]))/nrow(Filtered)
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Kinship, local version'
cpt <- cpt+1

#Inverse with solve
ptm <- proc.time()
InvKin <- solve(Kinship)
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Inverse with solve, local version'
cpt <- cpt+1

#Inverse with chol
ptm <- proc.time()
InvKin <- chol2inv(chol(Kinship)[,])
Times.big[cpt,] <- (proc.time()-ptm)[1:3]
Names.big[cpt] <- 'Inverse with chol, local version'
cpt <- cpt+1


Summary <- Times.big[1:(cpt-1),] 
rownames(Summary) <- Names.big[1:(cpt-1)]
saveRDS(Summary,file='./Results/Summary_BigWay.rds')



