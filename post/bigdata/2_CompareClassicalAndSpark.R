#### This program exemplifies the use of spark and sparklyr on the Orange dataset.
#### The different parts of the program can be run independently.
#### Required packages are specified at the beginning of each part, 
#### along with the work directory.
#### WARNING: if you want to re-run this program on the Orange dataset,
### you'll need... the dataset, that is big (500Mo) and not provided with this code...

## Interesting tutorials:
## https://www.rstudio.com/resources/webinars/introducing-an-r-interface-for-apache-spark/
## http://tutoriels-data-mining.blogspot.com/2016/05/programmation-r-sous-spark-avec-sparkr.html
## http://spark.rstudio.com/


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


        #### Required packages


library(data.table)
library(dplyr)
library(ggplot2)
library(nycflights13)



        #### Load the data


## Load the data
ptm <- proc.time()
Orange <- fread('./Data/Orange/NIDT_D4C_2G3G4G_2017105.CSV',sep=';',header=F)
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'fread'
cpt <- cpt+1
colnames(Orange) <- c('Date_Time','Id_RelAnt','NbSim_Fr','NbSim_Other','NbCom_Fr','NbCom_Other')
Orange

## Create a Time variable and compute relevant quantities
ptm <- proc.time()
MeanPerHour <- Orange %>%
  mutate(Time = substr(Date_Time,start=12,stop=13)) %>%
  group_by(Time) %>%
  summarise(M_NbSim_Fr=mean(NbSim_Fr),M_NbSim_Other=mean(NbSim_Other),M_NbCom_Fr=mean(NbCom_Fr),M_NbCom_Other=mean(NbCom_Other))  
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Local mean computation'
cpt <- cpt+1

##Compute the global mean
ptm <- proc.time()
GlobalMean_NbSimFr <- MeanPerHour %>% summarise(Mean=mean(M_NbSim_Fr))
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Local global mean for NbSimFr'
cpt <- cpt+1

##Have a look!
ptm <- proc.time()
GlobalMean_NbSimFr <- MeanPerHour %>% summarise(Mean=mean(M_NbSim_Fr))
Times.usual[cpt,] <- (proc.time()-ptm)[1:3]
Names.usual[cpt] <- 'Local display'
cpt <- cpt+1

Summary <- Times.usual[1:(cpt-1),] 
rownames(Summary) <- Names.usual[1:(cpt-1)]
saveRDS(Summary,file='./Results/SummaryOrange_UsualWay.rds')

## Some graphical displays
ggplot(MeanPerHour, aes(M_NbSim_Fr,M_NbCom_Fr)) + geom_point() +
  geom_point(aes(M_NbSim_Other,M_NbCom_Other), colour = 'red')

ggplot(MeanPerHour, aes(Time,M_NbCom_Fr)) + geom_point(aes(Time,M_NbCom_Fr))


        ################################################          
        #### 
        ####    Part II: use of Spark   
        #### 
        ################################################          


rm(list=ls())
setwd('D:/R/StateOfTheR/BigData')
Times.spark <- matrix(0,100,3)
Names.spark <- rep(NA,100)
cpt <- 1


        #### Install spark (and dplyr)


install.packages("sparklyr")
library(sparklyr)
spark_install(version = "2.2.0")
devtools::install_github("rstudio/sparklyr")
install.packages(c("nycflights13", "Lahman"))
library(dplyr)


        #### Open a spark session


sc <- spark_connect(master="local")
##Note: make sure Spark 2.2.0 (or more recent version) is used 
## before running the following


        #### Load data in Spark


## From a file
Orange_tbl <- 
  spark_read_csv(sc = sc,
                 name = 'Orange',
                 path = 'D:/R/StateOfTheR/BigData/Data/Orange/NIDT_D4C_2G3G4G_2017105.CSV.gz', 
                 header = FALSE, delimiter = ';')
Orange_tbl <- Orange_tbl %>%
  rename('Date_Time'='V1','Id_RelAnt'='V2','NbSim_Fr'='V3', 
         'NbSim_Other'='V4','NbCom_Fr'='V5','NbCom_Other'='V6')
Orange_tbl


        #### What is a lazy query ?


#Let's do the required job
ptm <- proc.time()
MeanPerHour <- Orange_tbl %>%
  mutate(Time = substr(Date_Time,start=12,stop=13)) %>%
  group_by(Time) %>%
  summarise(M_NbSim_Fr=mean(NbSim_Fr),M_NbSim_Other=mean(NbSim_Other),M_NbCom_Fr=mean(NbCom_Fr),M_NbCom_Other=mean(NbCom_Other))  
Times.spark[cpt,] <- (proc.time()-ptm)[1:3]
Names.spark[cpt] <- 'Spark mean computation'
cpt <- cpt+1

ptm <- proc.time()
GlobalMean_NbSimFr <- MeanPerHour %>% summarise(Mean=mean(M_NbSim_Fr))
Times.spark[cpt,] <- (proc.time()-ptm)[1:3]
Names.spark[cpt] <- 'Spark global mean for NbSimFr'
cpt <- cpt+1

ptm <- proc.time()
GlobalMean_NbSimFr
Times.spark[cpt,] <- (proc.time()-ptm)[1:3]
Names.spark[cpt] <- 'Spark global mean for NbSimFr, print'
cpt <- cpt+1
##Note: at this last step we do require the computation of the global mean so that it can be printed. 
## It is only at this step that the previous tasks are really performed...


ptm <- proc.time()
SDMean_NbSimFr <- MeanPerHour %>% summarise(SD=sd(M_NbSim_Fr))
SDMean_NbSimFr
Times.spark[cpt,] <- (proc.time()-ptm)[1:3]
Names.spark[cpt] <- 'Spark SD for mean per hour, print'
cpt <- cpt+1
##Note: this step is much faster than the previous one because MeanPerHour has already
## been evaluated!


        #### Transfer data from on environment to another


## From Spark to R memory
MeanPerHour_R <- collect(MeanPerHour)

## Move it from Spark to Disk
spark_write_csv(x=MeanPerHour, path = 'D:/R/StateOfTheR/BigData/Results/MeanPerHour.csv',header = T, delimiter = ';')

## Move it from R to Spark
MeanPerHour_2 <- copy_to(sc, MeanPerHour_R, name= "MeanPerHour_Sp",overwrite = T)

## Have a look at the different environments:
ls()
src_tbls(sc)


        #### Make some graphical displays


ggplot(MeanPerHour_R, aes(M_NbSim_Fr,M_NbCom_Fr)) + geom_point() +
  geom_point(aes(M_NbSim_Other,M_NbCom_Other), colour = 'red')

ggplot(MeanPerHour_R, aes(Time,M_NbCom_Fr)) + geom_point(aes(Time,M_NbCom_Fr))
##Note: one needs to collect before plotting.


        #### Build a shorter version of the table by selecting some obs.


## Build the list of antenna IDs
ListRelAnt <- Orange_tbl %>% distinct(Id_RelAnt) %>% collect()

## Select data for the first 5 antennas (and group by antenna)
OrangePerRelAnt <- Orange_tbl %>%
  group_by(Id_RelAnt) %>%
  filter(Id_RelAnt %in% ListRelAnt$Id_RelAnt[1:5])  


        #### Use some R functions to create a table in the spark environment


AddDensity_trial1 <- OrangePerRelAnt %>% mutate(Density = dpois(NbSim_Fr,lambda = 100)) 
AddDensity_trial1
##Note: does not work because the R function dpois is not recognized

AddDensity_trial2 <- OrangePerRelAnt %>%  collect() %>% mutate(Density = dpois(NbSim_Fr,lambda = 100)) 
AddDensity_trial2
##Note: does work, but required to load data in R memory...

AddDensity_trial3 <- OrangePerRelAnt %>% spark_apply(function(d){dpois(d$NbSim_Fr,lambda=100)})
AddDensity_trial3
##Note: the good way ;)


        #### Try to make some fancy stuffs





## Now have a look at the regression between NbCom_Fr and NbSim_Fr
## per antenna
OrangePerRelAnt %>%
  spark_apply(
    function(d) broom::tidy(lm(NbCom_Fr ~ NbSim_Fr, d)),
    names = c("term", "estimate", "std.error", "statistic", "p.value","sigma"),
    group_by = "Id_RelAnt"
  )

##Note: About the tidy function
## https://cran.r-project.org/web/packages/broom/vignettes/broom.html


MeanPerHour %>% 
  mutate(TimeNum = Time + 0) %>%
  mutate(TimeSlice = ifelse(TimeNum %in% c(0:5,18:23), 'Evening',
                            ifelse(TimeNum %in% 6:11, 'Morning', 'Afternoon'))) %>%
  mutate(TimeRecoded = ifelse((TimeSlice == 'Evening')&(TimeNum %in% 18:23), TimeNum-24, TimeNum)) %>%
  group_by(TimeSlice) %>%
  spark_apply(
    function(d) broom::tidy(lm(M_NbCom_Fr ~ TimeRecoded, d)),
    names = c("term", "estimate", "std.error", "statistic", "p.value","sigma"),
    group_by = "TimeSlice"
  ) 

saveRDS(Summary,file='./Results/SummaryOrange_SparkWay.rds')






