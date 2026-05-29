
#ldb8c9a

#Read files
install.packages("FLCore", repos="http://flr-project.org/R")
library(xlsx)
library(dplyr)
library(FLCore)

setwd("D:/WorkingGroups/2026_WG/2026WGBIE/a4a/ldb8c9a/data")
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="catches.n")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)


df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))
df2

#catches.n
catches.n <- FLQuant(as.vector(t(df2[,-1])),dim=c(8,40,1,1,1,1),units="10^3")
names(dimnames(catches.n))[1] <- "age"
dimnames(catches.n)[1]$age <- as.character(0:7)
dimnames(catches.n)[2]$year <- as.character(1986:2025)
catches.n


#landings.n
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="landings.n")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)


df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))
df2

landings.n <- FLQuant(as.vector(t(df2[,-1])),dim=c(8,40,1,1,1,1),units="10^3")
names(dimnames(landings.n))[1] <- "age"
dimnames(landings.n)[1]$age <- as.character(0:7)
dimnames(landings.n)[2]$year <- as.character(1986:2025)
#landings.n <- window(landings.n, start=1984,end=2018)
landings.n

#discards.n
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="discards.n")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)


df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))
df2
#df2[,10:11]<- NA
#df2[30:35,] <- NA

discards.n <- FLQuant(as.vector(t(df2[,-1])),dim=c(8,40,1,1,1,1),units="10^3")
names(dimnames(discards.n))[1] <- "age"
dimnames(discards.n)[1]$age <- as.character(0:7)
dimnames(discards.n)[2]$year <- as.character(1986:2025)

discards.n

#catches
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="ldb8c9a_caton")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)


df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))
df2

catches <- FLQuant(df2$catches,dim=c(1,40,1,1,1,1),units="t")
names(dimnames(catches))[1] <- "age"
names(dimnames(catches))[1]$age <- "all"
dimnames(catches)[2]$year <- as.character(1986:2025)
catches

#landings
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="ldb8c9a_caton")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)


df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))
df2

landings <- FLQuant(df2$landings,dim=c(1,40,1,1,1,1),units="t")
names(dimnames(landings))[1] <- "age"
names(dimnames(landings))[1]$age <- "all"
dimnames(landings)[2]$year <- as.character(1986:2025)
landings

# OTRA FORMA PROBAR Y BORRAR

# Convertir los valores a numéricos
#df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))
#df2

# Extraer los años desde los nombres de las filas
#anhos <- rownames(data)

# Crear el objeto FLQuant usando los años extraídos
#landings <- FLQuant(df2$landings, dim = c(1, length(anhos), 1, 1, 1, 1), units = "t")

# Asignar nombres a las dimensiones
#names(dimnames(landings))[1] <- "age"
#dimnames(landings)$age <- "all"
#dimnames(landings)$year <- anhos

#landings

#discards
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="ldb8c9a_caton")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)


df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))
df2

discards <- FLQuant(df2$discards,dim=c(1,40,1,1,1,1),units="t")
names(dimnames(discards))[1] <- "age"
names(dimnames(discards))[1]$age <- "all"
dimnames(discards)[2]$year <- as.character(1986:2025)
discards

#catches.wt
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="catches.wt")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)


df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))
df2

catches.wt <- FLQuant(as.vector(t(df2[,c(-1)])),dim=c(8,40,1,1,1,1),units="kg")
names(dimnames(catches.wt))[1] <- "age"
dimnames(catches.wt)[1]$age <- as.character(0:7)
dimnames(catches.wt)[2]$year <- as.character(1986:2025)
catches.wt

#landings.wt
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="landings.wt")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)

df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))
df2

landings.wt <- FLQuant(as.vector(t(df2[,c(-1)])),dim=c(8,40,1,1,1,1),units="kg")
names(dimnames(landings.wt))[1] <- "age"
dimnames(landings.wt)[1]$age <- as.character(0:7)
dimnames(landings.wt)[2]$year <- as.character(1986:2025)
landings.wt

#discards.wt
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="discard.wt")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)

df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))

discards.wt <- FLQuant(as.vector(t(df2[,c(-1)])),dim=c(8,40,1,1,1,1),units="kg")
names(dimnames(discards.wt))[1] <- "age"
dimnames(discards.wt)[1]$age <- as.character(0:7)
dimnames(discards.wt)[2]$year <- as.character(1986:2025)
discards.wt

#stock.wt
data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="catches.wt")   # we consider stock.wt=catch.wt
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)

df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))

stock.wt <- FLQuant(as.vector(t(df2[,c(-1)])),dim=c(8,40,1,1,1,1),units="kg")
names(dimnames(stock.wt))[1] <- "age"
dimnames(stock.wt)[1]$age <- as.character(0:7)
dimnames(stock.wt)[2]$year <- as.character(1986:2025)
stock.wt


#mortality

m <- FLQuant(0.2,dim=c(8,40,1,1,1),units="m")
names(dimnames(m))[1] <- "age"
dimnames(m)[1]$age <- as.character(0:7)
dimnames(m)[2]$year <- as.character(1986:2025)
m

#maturity

data <- read.xlsx2("../data/inputdata_ldb8c9a_work.xlsx",sheetName="mat")
names(data)
dim(data)
head(data)
head(tail(data))
summary(data)
colnames(data)

df2 <- mutate_all(data, function(x) as.numeric(as.character(x)))

mat <- FLQuant(as.vector(t(df2[,-1])),dim=c(8,40,1,1,1,1),units="")
names(dimnames(mat))[1] <- "age"
dimnames(mat)[1]$age <- as.character(0:7)
dimnames(mat)[2]$year <- as.character(1986:2025)
mat

#harvest.spwn, harvest before spawning

harvest.spwn <- FLQuant(0,dim=c(8,40,1,1,1),units="")
names(dimnames(harvest.spwn))[1] <- "age"
dimnames(harvest.spwn)[1]$age <- as.character(0:7)
dimnames(harvest.spwn)[2]$year <- as.character(1986:2025)
harvest.spwn


#m.spwn, mortality before spawning

m.spwn <- FLQuant(0,dim=c(8,40,1,1,1),units="")
names(dimnames(m.spwn))[1] <- "age"
dimnames(m.spwn)[1]$age <- as.character(0:7)
dimnames(m.spwn)[2]$year <- as.character(1986:2025)
m.spwn


#FLSTOCK
stock <- FLStock(catch.n =catches.n, landings.n= landings.n, discards.n=discards.n,
                 catch.wt=catches.wt,landings.wt=landings.wt, discards.wt=discards.wt,stock.wt=stock.wt,
                 catch= catches,landings=landings, discards=discards,
                 m=m, mat=mat,
                 harvest.spwn=harvest.spwn,m.spwn=m.spwn)

stock@name <- "ldb8c9a"
stock@desc <- "run1"
stock@range["minfbar"] <- 2
stock@range["maxfbar"] <- 4
stock@range

# Stock
stock <- setPlusGroup(stock, 6)

save(stock,file="../out/ldb8c9ainputs.RData")

