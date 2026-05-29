

#library
library(xlsx)
library(dplyr)
library(FLCore)

setwd("D:/WorkingGroups/2026_WG/2026WGBIE/a4a/ldb8c9a/data")

#indices

#CPUE.SPGFS

data<- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="CPUE.SPGFS")
head(data)
head(tail(data))
summary(data)
df2 <- mutate_all(data[,-1], function(x) as.numeric(as.character(x)))
df2

tun1 <- FLQuant(as.vector(t(df2)),dim=c(8,38,1,1,1,1))
names(dimnames(tun1))[1] <- "age"

dimnames(tun1)[2]$year <- as.character(1988:2025)
dimnames(tun1)[1]$age <- as.character(0:7)
tun1

Tun1 <- FLIndex(index=tun1)
Tun1@range[c("startf", "endf")] <- c(0.75, 1.00)
Tun1@name <- "SPGFS"
Tun1@desc <- "Spanish IBTS survey index in 8c and 9a; Cpue in numbers per 30min; L. boscii"
Tun1@type <- 'numbers'
Tun1@index[Tun1@index==0] <- NA
Tun1
Tun1@index



#LPUE.LCGOTBDEF_1

data<- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="LPUE.LCGOTBDEF_1")
head(data)
head(tail(data))
summary(data)
df2 <- mutate_all(data[,-1], function(x) as.numeric(as.character(x)))
df2

tun2 <- FLQuant(as.vector(t(df2)),dim=c(7,14,1,1,1,1))
names(dimnames(tun2))[1] <- "age"
dimnames(tun2)[2]$year <- as.character(1986:1999)
tun2

Tun2 <- FLIndex(index=tun2)
Tun2@range[c("startf", "endf")] <- c(0, 1.00)
Tun2@name <- "LPUE.LCGOTBDEF_1"
Tun2@desc <- "Spanish bottom trawlers (Coru?a) in subarea 8c9a from 1986-1999; L. boscii"
Tun2@type <- 'numbers'
Tun2@index[Tun2@index==0] <- NA
Tun2
Tun2@index


#LPUE.LCGOTBDEF_2

data<- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="LPUE.LCGOTBDEF_2")
head(data)
head(tail(data))
summary(data)
df2 <- mutate_all(data[,-1], function(x) as.numeric(as.character(x)))
df2

tun3 <- FLQuant(as.vector(t(df2)),dim=c(7,26,1,1,1,1))
names(dimnames(tun3))[1] <- "age"
dimnames(tun3)[2]$year <- as.character(2000:2025)
tun3

Tun3 <- FLIndex(index=tun3)
Tun3@range[c("startf", "endf")] <- c(0, 1.00)
Tun3@name <- "LPUE.LCGOTBDEF_2"
Tun3@desc <- "Spanish bottom trawlers (Coru?a) in subarea 8c9a from 2000-2020; L. boscii"
Tun3@type <- 'numbers'
Tun3@index[Tun3@index==0] <- NA
Tun3
Tun3@index


#CPUE.PTCRUST

data<- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="CPUE.PTCRUST")
head(data)
head(tail(data))
summary(data)
df2 <- mutate_all(data[,-1], function(x) as.numeric(as.character(x)))
df2

tun4 <- FLQuant(as.vector(t(df2)),dim=c(7,22,1,1,1,1))
names(dimnames(tun4))[1] <- "age"

dimnames(tun4)[2]$year <- as.character(1997:2018)
dimnames(tun4)[1]$age <- as.character(1:7)
tun4

Tun4 <- FLIndex(index=tun4)
Tun4@range[c("startf", "endf")] <- c(0.50, 0.75)
Tun4@name <- "PTCRUST"
Tun4@desc <- "Portuguese Crustacean survey index in 9a; Cpue in numbers per hour; L. boscii"
Tun4@type <- 'numbers'
Tun4@index[Tun4@index==0] <- NA
Tun4
Tun4@index

#CPUE.OAB

data<- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="CPUE.OAB")
head(data)
head(tail(data))
summary(data)
data$year<-as.numeric(data$year)
data$index<-as.numeric(data$index)


df2 <- list(age="all", year=2003:2020)
Tun5 <- FLIndexBiomass(FLQuant (NA,dimnames=df2))
index(Tun5) <- data$index
Tun5@range[c("startf", "endf")] <- c(0, 1.00)
Tun5@name <- "CPUE.OAB"
Tun5@desc <- "Observers on board Spanish bottom trawlers (OTB_DEF) in subarea 8c9a from 1986-2020; L. boscii"
Tun5@index[Tun5@index==0] <- NA
range(Tun5)[c("min","max")] <- c(2,4)
Tun5
Tun5@index


#Create FLIndices

tun <- FLIndices(Tun1,Tun2,Tun3,Tun4,Tun5)

#Select the year and ages of each index
tun.sel <- FLIndices(trim(tun[[1]],age=0:6),trim(tun[[2]],age=3:6),trim(tun[[3]],age=3:6),trim(tun[[4]],age=1:6),trim(tun[[5]],age=2:6))
                     
save(tun.sel,file="../out/ldb8c9aIndices.RData")


