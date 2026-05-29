#############################################################################################
# catch_table.R - DESC
# /catch_table.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

### Clean slate
rm(list=ls())

setwd("D:/WorkingGroups/2026_WG/2026WGBIE/a4a/ldb8c9a/data")
load("../out/fitldb/fitDef/model/runs.RData")
#setwd(path)

library(icesAdvice)
library(flextable)
library(data.table)
library(writexl)


#ICES ADVICE SHEET

fbar(runs$Fmsy)[, '2027']
ssb(runs$Fmsy)[, '2027']
rec(runs$Fmsy)[, '2027']
landings(runs$Fmsy)[, '2026']
discards(runs$Fmsy)[, '2026']
catch(runs$Fmsy)[, '2026']
ssb(runs$Fmsy)[, '2026']

#load(paste0(path,"runs.RData"))

forecastYr <- range(runs[[1]])["maxyear"]-1

discardFages <- 0:2  #is this correct?
landFages <- range(runs[[1]])["minfbar"]:range(runs[[1]])["maxfbar"] #is 2:4 correct?

Funwanted <- function(x, ages=discardFages) { 
  quantMeans((discards.n(x)[ac(ages),] / catch.n(x)[ac(ages),]) *
               harvest(x)[ac(ages)])
}

Fwanted <- function(x, ages=landFages) {  
  quantMeans((landings.n(x)[ac(ages),] / catch.n(x)[ac(ages),]) *
               harvest(x)[ac(ages)])
}

metrics(window(runs[[1]], start=forecastYr, end=forecastYr),
        list(catch=catch, wanted=landings, unwanted=discards,
             F=fbar, Fwanted=Fwanted, Funwanted=Funwanted))

unlist(lapply(runs, is, "FLStock"))

# TABLE. Advice sheet annual catch options

tab_options <- lapply(runs, function(x) {
  model.frame(
    FLQuants(c(metrics(window(x, start=forecastYr, end=forecastYr),
                       list(catch=catch, wanted=landings, unwanted=discards,
                            F=fbar, Fwanted=Fwanted, Funwanted=Funwanted)),
               metrics(window(x, start=forecastYr+1, end=forecastYr+1),
                       list(SSB=ssb)))), drop=TRUE)})


tab_options <- rbindlist(tab_options, idcol="basis")

# ADD SSB change, 100 - (old / new) * 100
ssbiy <- c(ssb(runs[[1]])[, as.character(forecastYr)])
tab_options[, ssbchange:=(SSB - ssbiy) / ssbiy * 100]

# ADD TAC change
tab_options[, tacchange:=(catch - c(tac)) / c(tac) * 100]

# ADD advice change
tab_options[, advicechange:=(catch - c(advice)) / c(advice) * 100]

# FIX zeroes

tab_options[, (2:ncol(tab_options)) := lapply(.SD, function(x) ifelse(abs(x) < 0.0001, 0, x)), .SDcols=2:ncol(tab_options)]


# SET first column
#tab_options$basis <- c("MSY approach: F[MSY]", "F=MAP F[MSY lower]",
#                       "F=MAP F[MSY upper]", "F=0", "F[pa]", "F[lim]", 
#                       paste0("SSB (",forecastYr+1,")=B[pa]"),
#                       paste0("SSB(",forecastYr+1,")=B[lim]"), 
#                       paste0("SSB(",forecastYr+1,")=MSY B[trigger]"),
#                       paste0("SSB(",forecastYr+1,")=SSB(",forecastYr,")"),
#                       "F[2025]", "Roll-over TAC")

tab_options$basis <- c("MSY approach: F[MSY]", "F=MAP F[MSY lower]",
                       "F=MAP F[MSY upper]", "F=0", "F[pa]", "F[lim]", 
                       paste0("SSB (",forecastYr+1,")=B[pa]"),
                       paste0("SSB(",forecastYr+1,")=B[lim]"), 
                       paste0("SSB(",forecastYr+1,")=MSY B[trigger]"),
                       "F[2026]", 
                       paste0("SSB(", forecastYr + 1, ")=SSB(", forecastYr, ")"),
                       "Roll-over TAC") 

# CALL round and icesRound
tab_options[, c(2,3,4,8) := lapply(.SD, round, digits=0), .SDcols = c(2,3,4,8)]
tab_options[, c(5,6,7) := lapply(.SD, round, digits=3), .SDcols = c(5,6,7)]
tab_options[, c(9,10,11) := lapply(.SD, icesRound), .SDcols = c(9,10,11)]

## ICES Rounding for F
x <- as.logical(tab_options[,"F"]>=0.2)
tab_options[x,"F"] <- round(tab_options[x,"F"],2)
x <- as.logical(tab_options[,"Fwanted"]>=0.2)
tab_options[x,"Fwanted"] <- round(tab_options[x,"Fwanted"],2)
x <- as.logical(tab_options[,"Funwanted"]>=0.2)
tab_options[x,"Funwanted"] <- round(tab_options[x,"Funwanted"],2)

# CREATE table
#ft <- flextable(tab_options[c(1,2,3,1,11,4,5,6,7,8,9,10,12),])
ft <- flextable(tab_options)

# SET colnames

ftabops <- set_header_labels(ft, 
                             basis="Basis", 
                             catch=paste0("Total catch (",forecastYr,")"), 
                             wanted=paste0("Wanted catch (",forecastYr,")"),
                             unwanted=paste0("Unwanted catch (",forecastYr,")"), 
                             F=paste0("F[total] (ages ",min(landFages),"-",max(landFages),") (",forecastYr,")"),
                             Fwanted=paste0("F[wanted] (ages ",min(landFages),"-",max(landFages),") (",forecastYr,")"), 
                             Funwanted= paste0("F[unwanted] (ages ",min(discardFages),"-",max(discardFages),") (",forecastYr,")"), 
                             SSB=paste0("SSB (",forecastYr+1,")"), 
                             ssbchange="% SSB change", 
                             tacchange="% TAC change",
                             advicechange="% Advice change")

dimnames(tab_options)[[2]] <- c("Basis", 
                               paste0("Total catch (",forecastYr,")"), 
                               paste0("Wanted catch (",forecastYr,")"),
                               paste0("Unwanted catch (",forecastYr,")"), 
                               paste0("F[total] (ages ",min(landFages),"-",max(landFages),") (",forecastYr,")"),
                               paste0("F[wanted] (ages ",min(landFages),"-",max(landFages),") (",forecastYr,")"), 
                               paste0("F[unwanted] (ages ",min(discardFages),"-",max(discardFages),") (",forecastYr,")"), 
                               paste0("SSB (",forecastYr+1,")"), 
                               "% SSB change", "% TAC change", "% Advice change")



writexl::write_xlsx(tab_options,
                    path="../out/fitldb/fitDef/catch_options.xlsx")

#TO EXTRACT the stock numbers of projected years to do the comparison in advice


#stock.n(runs$Fmsy)
#as.data.frame(stock.n(runs$Fmsy)[,,1,1,1,1])

x <- stock.n(runs$Fmsy)

# Reducir a 2D eliminando las dimensiones de tamaño 1
mat <- x[,,1,1,1,1, drop = TRUE]  # <- ESTA ES LA CLAVE

# Asignar nombres
rownames(mat) <- dimnames(x)$age
colnames(mat) <- dimnames(x)$year

# Exportar a CSV
write.csv(mat, '../out/fitldb/fitDef/stf_stocknumbers.csv', row.names = TRUE)


