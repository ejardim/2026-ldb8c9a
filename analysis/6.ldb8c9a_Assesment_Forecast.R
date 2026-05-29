# model.R - Run forecast
# 2020_sol.27.4_forecast/model.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

install.packages("devtools")
install.packages("FLCore", repo = "http://flr-project.org/R")
devtools::install_github("ices-tools-prod/msy")
install.packages(c("FLa4a", "FLasher", "ggplotFL"), repos="https://flr-project.org/R")
install.packages(c("ggplot2", "snpar", "foreach", "data.table"))
devtools::install_github("flr/a4adiags")


library(FLCore)
library(icesTAF)
library(ggplot2)
library(ggplotFL)
library(FLFishery)
library(FLasher)
library(FLa4a)
library (a4adiags)


setwd("D:/WorkingGroups/2026_WG/2026WGBIE/a4a/ldb8c9a/data")
mkdir("model")


# LOAD a4a input (stock) and output (fit1)

load("../out/fitldb/fitDef/ldb8c9a_2020_EqSim_Workspace.RData")
load("../out/fitldb/fitDef/ldb8c9afitDef.Rdata")



# CREATE updated FLStock
run <- stock + fit1

# --- SETUP

# refpts: Bpa, Fpa, Blim, Flim, BmsyTrig, Fmsy, FmsyLow, FmsyUpp

# MODEL, ADVICE and FORECAST year

dy <- dims(run)$maxyear
ay <- dy + 1
fy <- ay + 1

# advice current year

advice <- FLQuant(2825, dimnames=list(age='all', year=ay), units="tonnes")
tac <- advice

# SET fully selected ages  ## 
fages <- seq(2, 4)


# GEOMEAN but last year
#rec0gm <- exp(mean(log(window(stock.n(run)["1",], end=-1))))

# GEOMEAN but last year

ages <- range(stk)["min"]:range(stk)["max"]
years <- range(stk)["minyear"]:range(stk)["maxyear"]
meanFages <- c(range(stk)["minfbar"],range(stk)["maxfbar"])
nyears <- length(years)

#rec0gm <- exp(mean(log(window(stock.n(run)["0",], start=1990, end=2023))))
rec0gm <- exp(mean(log(window(stock.n(run)["0",], start=2023, end=2025)))) ##GM LAST 3 YEARS
rec0gm
# [1] 22747.69

# --- SETUP future

# 3 years, 5 years wts/selex, 3 years discards
fut <- stf(run, nyears=3, wts.nyears = 5, fbar.nyears=5, disc.nyears=5)

# GET F status quo (Fsq)
#Fsq <- expand(fbar(fut)[, ac(dy)], year=ay)
#Fsq <- expand(yearMeans(fbar(fut)[, ac((dy-2):dy)]), year=ay)
Fsq <- FLCore::expand(fbar(fut)[, ac(dy)], year = ay)
Fsq
#valor

# SET geomean SRR
gmsrr <- predictModel(model=rec~a, params=FLPar(c(rec0gm), units="thousands",
            dimnames=list(params="a", year=seq(ay, length=3), iter=1)))

# > gmsrr   # AI: reemplaza el reclutamiento con rec1gm del a?o intermedio (2022) y los dos siguientes.
# An object of class "FLQuants": EMPTY
# model:  
#   rec ~ a
# 
# params:  
#   An object of class "FLPar"
# year
# params    2022        2023      2024  
#  a 43826 43826 43826
# units:  thousands 


refpts <- FLPar(refPts[1,])
# GENERATE targets from refpts
#targets <- expand(as(refpts, 'FLQuant'), year=fy)
targets <- FLCore::expand(as(refpts, "FLQuant"), year = fy)

##############################################################################
#A second round added to include a new variable in the output
# GENERATE targets from refpts   
#need to run the forecast once to see what the SSB at the start of the advice year is: as.numeric(ssb(runs[[1]])[,as.character(fy)])

as.numeric(ssb(runs[[1]])[,as.character(fy)])
#12387.61


# Then enter it manually
refPts <- cbind(refPts, 12387.61) #fill manually
dimnames(refPts)[[2]][ncol(refPts)] <- "constSSB"
refpts <- FLPar(refPts[1,])
#targets <- expand(as(refpts, 'FLQuant'), year=fy)
targets <- FLCore::expand(as(refpts, "FLQuant"), year = fy)


#################

#AI: to complete in the advice the assumptions for interim year and forecast.


fbar(runs$Fmsy)[, '2026']
fbar(runs$Fmsy)[, '2027']
ssb(runs$Fmsy)[, '2027']
rec(runs$Fmsy)[, '2027']
catch(runs$Fmsy)[, '2026']
landings(runs$Fmsy)[, '2026']
discards(runs$Fmsy)[, '2026']

#Otro forma de obtener los valores de los a?os intermedios:

# as.numeric(catch(runs[[1]])[,as.character(ay)])
# #[1] 
# ay
# #[1] 2022
# as.numeric(landings(runs[[1]])[,as.character(ay)])
# #[1]
# as.numeric(discards(runs[[1]])[,as.character(ay)])
# #[1] 





###############################################################################
# --- PROJECT catch options

# Targets 2021
C0 <- FLQuant(0, dimnames = list(age = "all", year = fy))

Fiy <- FLQuants(fbar = append(Fsq, C0))
Ciy <- FLQuants(catch = append(tac, C0))

# RUN for Fsq
Fsqrun <- fwd(fut, sr = gmsrr, control = as(Fiy, "fwdControl"))

# TEST if catch <= TAC
if (all(catch(Fsqrun)[, ac(ay)] <= tac)) {
  itarget <- Fiy
} else {
  itarget <- Ciy
  Fsqrun <- fwd(fut, sr = gmsrr, control = as(Ciy, "fwdControl"))
}

# --- helper objects (FUERA de list)

rotac <- FLQuants(
  catch = FLCore::expand(tac, year = fy)
)

F0 <- FLQuants(
  fbar = FLQuant(0, dimnames = list(age = "all", year = fy + 1))
)

# --- DEFINE catch options
catch_options <- list(
  
  Fmsy = FLQuants(fbar = targets["Fmsy", ]),
  
  lFmsy = FLQuants(fbar = targets["Fmsylower", ]),
  
  uFmsy = FLQuants(fbar = targets["Fmsyupper", ]),
  
  F0 = FLQuants(
    fbar = FLQuant(0, dimnames = list(age = "all", year = fy))
  ),
  
  Fpa = FLQuants(fbar = targets["Fpa", ]),
  
  Flim = FLQuants(fbar = targets["Flim", ]),
  
  Bpa = FLQuants(ssb_flash = targets["Bpa", ]),
  
  Blim = FLQuants(ssb_flash = targets["Blim", ]),
  
  MSYBtrigger = FLQuants(ssb_flash = targets["MSYBtrigger", ]),
  
  Fsq = FLQuants(
    fbar = FLCore::expand(fbar(Fsqrun)[, ac(ay)], year = fy)
  ),
  
  SBsq = FLQuants(
    ssb_flash = FLCore::expand(ssb(Fsqrun)[, ac(ay + 1)], year = fy)
  ),
  
  rotac = rotac
)

# --- CONVERT to fwdControl

fctls <- lapply(catch_options, function(x) {
  as(
    FLQuants(c(window(itarget, end = ay), x, F0)),
    "fwdControl"
  )
})

# --- RUN

runs <- FLStocks(lapply(fctls, function(x)
  fwd(fut, sr = gmsrr, control = x)
))




# COMPARE

Map(compare, runs, fctls)


# --- PROJECT F levels

flevels <- seq(0, 0.50, 0.01)

control <- as(as(c(lapply(window(itarget, end=ay), propagate, length(flevels)),
                   FLQuants(fbar=FLQuant(flevels,  dimnames=list(year=fy, iter=seq(length(flevels))))),
                   lapply(F0, propagate, length(flevels))), "FLQuants"), "fwdControl")

f_runs <- divide(fwd(fut, sr=gmsrr, control=control), names=flevels)


ssb(runs$Fmsy)[, '2026'] # La SSB del a?o intermedio (2025) hay que incluirla en la tabla del advice donde se da la summary table pero este ultimo a?o no hay valor (NA).
# 12961


# SAVE

save(runs, f_runs, rec0gm, tac, advice, file="../out/fitldb/fitDef/model/runs.RData")

# McMC

