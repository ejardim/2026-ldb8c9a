##~--------------------------------------------------------------------------
# Code to take the assessment results from a provided FLStock object, 
# and run ICES standard EqSim reference point analyses
# D.C.M.Miller
##~--------------------------------------------------------------------------
## To Do:
# double check MSY Btrigger rules
# How many simulations (noSims) are needed? Do some comparison tests
# Change script to get SSB05 from the assessment results

###-------------------------------------------------------------------------------
### Clean slate
rm(list=ls())

# Load libraries
require(devtools)
devtools::install_github("fishfollower/SAM/stockassessment")  # run this once if stoassessment has not been installed before
library(stockassessment)
install_github("ices-tools-prod/msy") # run this once if msy has not been installed before
library(msy)
require(FLCore)

##~--------------------------------------------------------------------------
##        SECTION WHERE CHANGES NEED TO BE MADE   
##~--------------------------------------------------------------------------

##~--------------------------------------------------------------------------
## Directory info
path <- "D:/WorkingGroups/2026_WG/2026WGBIE/a4a/ldb8c9a/out/fitldb/fitDef/RefPoints/"   # folder were the code is and where results will be saved (in a subfolder)
runName <- "ldb8c9a_test" # (no spaces) Results and settings will be saved in a subfolder with this name (so make it descriptive)
## Save plots?
savePlots <- T

##~--------------------------------------------------------------------------
## Stock and assessment
stockName <- "ldb8c9a"                # Used only in plots (i.e. titles) and when saving data (i.e. file names)

# Load FLStock object:
# if .rdata file with FLSTOCK object is in the directory 'path' (specified above):
load(paste(path,"ldb8c9afitDef.Rdata", sep="")) #change name to match .rdata file with the FLStock object
stk1 <- stock + fit1
save(stk1,stock,tun.sel,fit1,file='ldb8c9afitDef.Rdata') 
stk <- stk1   # assign the FLStock object (may need to change the name form 'stock')  in the .rdata file to 'stk'

# Get range info from FLStock object
ages <- range(stk)["min"]:range(stk)["max"]
years <- range(stk)["minyear"]:range(stk)["maxyear"]
meanFages <- c(range(stk)["minfbar"],range(stk)["maxfbar"])

## Uncertainty last year
# need to specify these values if inputting an FLStock object
sigmaF <- 0.149 #NA                      # Gets from last year estimated in the assessment (for SAM), unless this is specified as a value i.e. !is.na()
sigmaSSB <- 0.129 #NA   
# Gets from last year estimated in the assessment (for SAM), unless this is specified as a value i.e. !is.na()
#sigmaF <- 0.2 #default                    
#sigmaSSB <- 0.2 #default  

##~--------------------------------------------------------------------------
## Create matrix for reference points
refPts <- matrix(NA,nrow=1,ncol=13, dimnames=list("value",c("MSYBtrigger","5thPerc_SSBmsy","Bpa","Blim","Fpa","Flim", "Fp05","Fmsy_unconstr","Fmsy","Fmsyupper_unconstr","Fmsyupper","Fmsylower_unconstr","Fmsylower")))  # "Fmsy_unconstr" is the Fmsy value without any precautionary considerations (i.e. ignore 5% P(SSB<Blim))
# Note: stores teh 'uncontrained Fmsy as well (i.e. without PA considerations)

## Enter Blim value
# Insert value for Blim, code below will calculate Bpa and MSY Btrigger
refPts[,"Blim"]  <- min(ssb(stk))  # Bloss                     

##~--------------------------------------------------------------------------
## Simulation settings
# Number of sims
noSims <- 5000                                # Choose a suitable number, final run should use at least 1000, test runs coudl be done with less to save time

# SR models to use
#appModels <- c("Segreg","Ricker","Bevholt")   # specify which SRR models to use  (other functions can be added)
appModels <- c("SegregBloss")  

# Which years (SSB years, not recruitment years) to exclude from the SRR fits 
rmSRRYrs <- c()                               # leave as 'c()' if the full time series is to be used (default)
#rmSRRYrs <- c(2015:2016)                     # Or specify here which other years (e.g. early period, most recent year) should be left out

# Autocorrelation in recruitment?
rhoRec <- F                                   # default=F

## Weight at age and selectivity
numAvgYrsB <- 5                               # Number of recent years to use for WAA
bioConst   <- TRUE                            # Constant/average WAA (TRUE) or resampling from the years specified (FALSE)
numAvgYrsS <- 5                               # Number of recent years to use for selectivity
selConst   <- TRUE                            # Constant/average selectivity (TRUE) or resampling from the years specified (FALSE)

## Forecast error (see Guidance document for details on calculation of these values)
# F
cvF  <- 0.212                                 # Default = 0.212
phiF <-	0.423                                 # Default = 0.423
# SSB
cvSSB <- 0                                    # Default = 0
phiSSB <- 0                                   # Default = 0

# 5th percentile of SSB in the final year of the assessment
SSB05<-0                                      # used in MSY Btrigger calculation. If set at 0, ignored


##~--------------------------------------------------------------------------
##        NO CHANGES NEED TO BE MADE BELOW THIS POINT   
##~--------------------------------------------------------------------------

##~--------------------------------------------------------------------------
## Set working directory
setwd(path)
# create subfolder
shell(paste("md", runName, sep=" "))
setwd(paste(path,runName,"/",sep=""))

##~--------------------------------------------------------------------------
##~--------------------------------------------------------------------------

## Plots to look at recent W@A and selectivity to inform on appropriate period to use
## Selectivity curves
if (savePlots) x11()
meanF <- apply(harvest(stk)[meanFages,],2, "mean")
sel <- sweep(harvest(stk),2,meanF,"/")
plot(ages,sel[,ac(max(years))], type="l", ylim=c(0,max(sel)), xlab="Age", ylab="Selectivity", main="Selectivity")
for (i in ac((max(years)-19):(max(years)-1))) lines(ages,sel[,i], col=i)
lines(ages,apply(sel[,ac((max(years)-2):max(years))],1,mean), col=1, lwd=5)
lines(ages,apply(sel[,ac((max(years)-4):max(years))],1,mean), col=2, lwd=5)
lines(ages,apply(sel[,ac((max(years)-9):max(years))],1,mean), col=3, lwd=5)
lines(ages,apply(sel[,ac((max(years)-19):max(years))],1,mean), col=4, lwd=5)
legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
#legend("bottomright", legend=c(1997:2016), lwd=1, col=1:20, bty="n")
if (savePlots) savePlot(paste("00_",stockName,"_Selectivity.png"),type="png")
if (savePlots) dev.off()

## Weight at age
if (savePlots) x11()
plot(ages,stock.wt(stk)[,ac(max(years)-1)], type="l", ylim=c(0,max(stock.wt(stk))), xlab="Age", ylab="Weight (kg)", main="Weight at Age")
for (i in ac((max(years)-19):(max(years)-1))) lines(ages,stock.wt(stk)[,i], col=i)
lines(ages,apply(stock.wt(stk)[,ac((max(years)-2):max(years))],1,mean), col=1, lwd=5)
lines(ages,apply(stock.wt(stk)[,ac((max(years)-4):max(years))],1,mean), col=2, lwd=5)
lines(ages,apply(stock.wt(stk)[,ac((max(years)-9):max(years))],1,mean), col=3, lwd=5)
lines(ages,apply(stock.wt(stk)[,ac((max(years)-19):max(years))],1,mean), col=4, lwd=5)
legend("topleft", legend=c("Mean last 3yrs","Mean last 5yrs","Mean last 10yrs","Mean last 20yrs"), lwd=5, col=1:4, bty="n")
#legend("bottomright", legend=c(1997:2016), lwd=1, col=1:20, bty="n")
if (savePlots) savePlot(paste("00_",stockName,"_WAA.png"),type="png")
if (savePlots) dev.off()

# year range
minYear <- range(stk)["minyear"]; maxYear <- range(stk)["maxyear"]

### Trim off last year of the stock object (only if incomplete data for last assessment year)
# origStk <- stk
# stk <- window(stk, start=minYear, end=(maxYear-1))

###-------------------------------------------------------------------------------
### Set SRR Models for the simulations
#Models: "segreg","ricker", "bevholt"; or specials: "SegregBlim/Bloss" (breakpt. Blim/Bloss)

## SRR years 
# Which years (SSB years) to exclude from the SRR fits
# Keep all except last 2 (poorly estimated rec/selec)
# rmSRRYrs <- union(rmSRRYrs, c(maxYear-1,maxYear))  # This removes last two years
srYears <- setdiff(c(minYear:(maxYear-1)),rmSRRYrs)

## determine segreg model with specified Blim breakpoint and (roughly) geomean rec above this
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= refPts[,"Blim"], ab$a * refPts[,"Blim"], ab$a * ssb))

## determine segreg model with Bloss breakpoint and (roughly) geomean rec above this
#SegregBloss  <- function(ab, ssb) log(ifelse(ssb >= min(ssb(stk)), ab$a * min(ssb(stk)), ab$a * ssb))
Bloss <- min(ssb(stk))
SegregBloss  <- function(ab, ssb) log(ifelse(ssb >= Bloss, ab$a * Bloss, ab$a * ssb))
###~~~~~~~~~~~~~
## autocorrelation
ACFrec <- acf(rec(stk)[,ac(srYears)])
acfRecLag1 <- round(ACFrec$acf[,,][2],2)
if (savePlots) x11()
acf(rec(stk), plot=T, main=paste("Autocor. in Rec, Lag1 =",acfRecLag1,sep=" "))
if (savePlots) savePlot(paste("04_",stockName,"_SRautocor.png"),type="png")
if (savePlots) dev.off()

# Set a max for AC? Shouldn't be more than 0.6 really

###-------------------------------------------------------------------------------
## Fit SRRs
FIT_segregBlim <- eqsr_fit(stk,nsamp=noSims, models = "SegregBlim", remove.years=rmSRRYrs)
#FIT_segregBloss <- eqsr_fit(stk,nsamp=noSims, models = "SegregBloss", remove.years=rmSRRYrs)
FIT_segreg <- eqsr_fit(stk,nsamp=noSims, models = "Segreg", remove.years=rmSRRYrs)
#FIT_segregAR1 <- eqsr_fit(stk,nsamp=noSims, models = "segregAR1", remove.years=rmSRRYrs)
FIT_All <- eqsr_fit(stk,nsamp=noSims, models = appModels, remove.years=rmSRRYrs)

# save model proportions and parameters:
write.csv(FIT_segregBlim$sr.det, paste(stockName,"_FIT_segregBlim_SRpars.csv",sep=""))
#write.csv(FIT_segregBloss$sr.det, paste(stockName,"_FIT_segregBloss_SRpars.csv",sep=""))
write.csv(FIT_segreg$sr.det, paste(stockName,"_FIT_segreg_SRpars.csv",sep=""))
write.csv(FIT_All$sr.det, paste(stockName,"_FIT_All_SRpars.csv",sep=""))

# Plot raw SRR results
# if (savePlots) x11()
# eqsr_plot(FIT_segregBlim,n=2e4)
# if (savePlots) savePlot(paste("05ai_",stockName,"_segregBlim.png"),type="png")
# if (savePlots) dev.off()

if (savePlots) x11()
eqsr_plot(FIT_segreg,n=2e4)
if (savePlots) savePlot(paste("05a_",stockName,"_segreg.png"),type="png")
if (savePlots) dev.off()

if (savePlots) x11()
eqsr_plot(FIT_All,n=2e4)
if (savePlots) savePlot(paste("05b_",stockName,"_SRRall.png"),type="png")
if (savePlots) dev.off()


###-------------------------------------------------------------------------------
## Run simulations
###-------------------------------------------------------------------------------

###-------------------------------------------------------------------------------
## Calculate Bpa based on sigmaSSB
refPts[,"Bpa"]  <- refPts[,"Blim"]*exp(sigmaSSB*1.645) 

###-------------------------------------------------------------------------------
## Simuation 1 - get Flim
# Flim is derived from Blim by simulating the stock with segmented regression S-R function with the point of inflection at Blim 
# Flim = the F that, in equilibrium, gives a 50% probability of SSB > Blim. 
# Note this simulation should be conducted with:
#  fixed F (i.e. without inclusion of a Btrigger)
#  without inclusion of assessment/advice errors. 

SIM_segregBlim <- eqsim_run(FIT_segregBlim,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = TRUE,
                            sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = TRUE,
                            Fcv=0, Fphi=0, SSBcv=0,
                            rhologRec=rhoRec,
                            Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                            Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY and lim values
tmp1 <- t(SIM_segregBlim$Refs2)
write.csv(tmp1, paste("EqSim_",stockName,"_SegregBlim_eqRes.csv",sep=""))
refPts[,"Flim"] <- tmp1["F50","catF"]

# Fpa is derived from Flim in the reverse of the way Bpa is derived from Blim. i.e.: 
#DM: not anymore, now Fpa=Fp05
#tmpFpa <- refPts[,"Flim"] * exp(-sigmaF * 1.645)
#if (tmpFpa<0.2) refPts[,"Fpa"] <- round(tmpFpa , 3) else refPts[,"Fpa"] <- round(tmpFpa , 2)
#if (refPts[,"Flim"]<0.2) refPts[,"Flim"] <- round(refPts[,"Flim"],3) else refPts[,"Flim"] <- round(refPts[,"Flim"],2)

###-------------------------------------------------------------------------------
## Simuation 2a - get initial Fmsy
# FMSY should initially be calculated based on:
#     a constant F evaluation 
#     with the inclusion of stochasticity in population and exploitation 
#     as well as assessment/advice error. 
#     Appropriate SRRs should be specified (here using all 3)

SIM_All_noTrig <- eqsim_run(FIT_All,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE,
                            sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                            Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                            rhologRec=rhoRec,
                            Btrigger = 0, Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                            Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY and lim values
tmp2 <- t(SIM_All_noTrig$Refs2)
write.csv(tmp2, paste("EqSim_",stockName,"_AllnoTrig_eqRes.csv",sep=""))
Fmsy_tmp <- tmp2["medianMSY","lanF"]
# FMSY range (low - upp) w/o Btrigger
lFmsy <- tmp2["Medlower", "lanF"]
uFmsy <- tmp2["Medupper", "lanF"]

# save Equilibrium plots
if (savePlots) x11()
eqsim_plot(SIM_All_noTrig,catch=TRUE)  
if (savePlots) savePlot(paste("06_",stockName,"_AllnoTrig_eqMSYplots.png"),type="png")
if (savePlots) dev.off()

if (savePlots) x11()
eqsim_plot_range(SIM_All_noTrig)  
if (savePlots) savePlot(paste("06_",stockName,"_AllnoTrig_eqMSYRANGEplot.png"),type="png")
if (savePlots) dev.off()

# To ensure consistency between the precautionary and MSY frameworks, FMSY is not allowed to be above Fpa
refPts[,"Fmsy_unconstr"] <- Fmsy_tmp 
#DM: not anymore, only check Fp05 later
# if (Fmsy_tmp > refPts[,"Fpa"]) {
#   print("WHOAAA, Fmsy > Fpa!") 
#   refPts[,"Fmsy"] <- refPts[,"Fpa"]
# } else {
refPts[,"Fmsy"] <- Fmsy_tmp
# }
refPts[,"Fmsylower_unconstr"] <- lFmsy 
refPts[,"Fmsylower"] <- lFmsy
refPts[,"Fmsyupper_unconstr"] <- uFmsy 
refPts[,"Fmsyupper"] <- uFmsy


###-------------------------------------------------------------------------------
## MSY Btrigger
data.05<-SIM_segregBlim$rbp
x.05 <- data.05[data.05$variable == "Spawning stock biomass", ]$Ftarget
b.05 <- data.05[data.05$variable == "Spawning stock biomass", ]$p05
plot(b.05~x.05, ylab="SSB", xlab="F")
b.lm <- loess(b.05 ~ x.05)
refPts[,"5thPerc_SSBmsy"] <- predict(b.lm, refPts[,"Fmsy"])
# check if F<Fmsy last 5 years (at least 3 times in last 5 years)
if (sum(as.numeric(fbar(stk)[,ac((maxYear-4):maxYear)])<=refPts[,"Fmsy"])<3) {
  refPts[,"MSYBtrigger"]  <- refPts[,"Bpa"]  
} else {
# Check if Bmsy_05>Bpa
  refPts[,"MSYBtrigger"] <-ifelse(refPts[,"5thPerc_SSBmsy"]>refPts[,"Bpa"],refPts[,"5thPerc_SSBmsy"],refPts[,"Bpa"])
# Check if Bmsy_05 > SSBcur_05
  refPts[,"MSYBtrigger"] <-ifelse(refPts[,"5thPerc_SSBmsy"] > SSB05,max(refPts[,"Bpa"],SSB05),refPts[,"5thPerc_SSBmsy"])  
  }


###-------------------------------------------------------------------------------
## Simuation 2b - get final Fmsy
# MSY Btrigger should be selected to safeguard against an undesirable or unexpected low SSB when fishing at FMSY
# The ICES MSY AR should be evaluated to check that the FMSY and MSY Btrigger combination adheres to precautionary considerations: 
#      in the long term, P(SSB<Blim)<5%
# The evaluation must include:
#      realistic assessment/advice error
#      stochasticity in population biology and fishery exploitation.
#      Appropriate SRRs should be specified (here using all 3)

SIM_All_Trig <- eqsim_run(FIT_All,  bio.years = c(maxYear-numAvgYrsB, maxYear-1), bio.const = FALSE,
                          sel.years = c(maxYear-numAvgYrsS, maxYear-1), sel.const = FALSE,
                          Fcv=cvF, Fphi=phiF, SSBcv=cvSSB,
                          rhologRec=rhoRec,
                          Btrigger = refPts[,"MSYBtrigger"], Blim=refPts[,"Blim"],Bpa=refPts[,"Bpa"],
                          Nrun=200, Fscan = seq(0,1.0,len=101),verbose=T)

# save MSY and lim values
tmp3 <- t(SIM_All_Trig$Refs2)
write.csv(tmp3, paste("EqSim_",stockName,"_AllTrig_eqRes.csv",sep=""))
refPts[,"Fpa"] <- refPts[,"Fp05"] <- tmp3["F05","catF"]

# save Equilibrium plots
if (savePlots) x11()
eqsim_plot(SIM_All_Trig,catch=TRUE)  
if (savePlots) savePlot(paste("07_",stockName,"_AllTrig_eqMSYplots.png"),type="png")
if (savePlots) dev.off()

# If the precautionary criterion (FMSY < Fp.05) evaluated is not met, then FMSY should be reduced to  Fp.05. 
# also applying ICES rounding to the F values
if (refPts[,"Fmsy"] > refPts[,"Fp05"]) {
  print("WHOAAA, Fmsy > Fp05!") # If Fmsy > Fp05, Fmsy = Fp05
  if (refPts[,"Fp05"]<0.2) refPts[,"Fmsy"] <- round(refPts[,"Fp05"],3) else refPts[,"Fmsy"] <- round(refPts[,"Fp05"],2)
} else {  
  if (refPts[,"Fmsy"]<0.2) refPts[,"Fmsy"] <- round(refPts[,"Fmsy"],3) else refPts[,"Fmsy"] <- round(refPts[,"Fmsy"],2) # Otherwise keep value from constant F simulation (which has been checked against Fpa)
}

if (refPts[,"Fmsyupper"] > refPts[,"Fp05"]) {
  if (refPts[,"Fp05"]<0.2) refPts[,"Fmsyupper"] <- round(refPts[,"Fp05"],3) else refPts[,"Fmsyupper"] <- round(refPts[,"Fp05"],2)
} else {  
  if (refPts[,"Fmsyupper"]<0.2) refPts[,"Fmsyupper"] <- round(refPts[,"Fmsyupper"],3) else refPts[,"Fmsyupper"] <- round(refPts[,"Fmsyupper"],2) # Otherwise keep value from constant F simulation (which has been checked against Fpa)
}  

if (refPts[,"Fmsylower"] > refPts[,"Fp05"]) {
  if (refPts[,"Fp05"]<0.2) refPts[,"Fmsylower"] <- round(refPts[,"Fp05"],3) else refPts[,"Fmsylower"] <- round(refPts[,"Fp05"],2)
} else {  
  if (refPts[,"Fmsylower"]<0.2) refPts[,"Fmsylower"] <- round(refPts[,"Fmsylower"],3) else refPts[,"Fmsylower"] <- round(refPts[,"Fmsylower"],2) # Otherwise keep value from constant F simulation (which has been checked against Fpa)
}  

if (refPts[,"Fp05"]<0.2) refPts[,"Fp05"] <- round(refPts[,"Fp05"],3) else refPts[,"Fp05"] <- round(refPts[,"Fp05"],2)  
refPts[,"Fpa"] <- refPts[,"Fp05"]

###-------------------------------------------------------------------------------
## Save reference points
write.csv(refPts, paste(stockName,"_RefPts.csv",sep=""))

###-------------------------------------------------------------------------------
## Save run settings
SRused <- appModels[1]
if (length(appModels)>1) for (i in 2:length(appModels)) SRused <- paste(SRused,appModels[i],sep="_")
SRyears_min <- min(srYears); SRyears_max <- max(srYears)
setList <- c("stockName", "runName", "sigmaF", "sigmaSSB", "noSims", "SRused", "SRyears_min", 
             "SRyears_max", "acfRecLag1","rhoRec", "numAvgYrsB", "numAvgYrsS", "cvF", "phiF", "cvSSB", "phiSSB")
runSet <- matrix(NA,ncol=1, nrow=length(setList), dimnames=list(setList,c("Value")))
for (i in setList) runSet[which(setList==i),] <- eval(parse(text = i))

write.csv(runSet, paste(stockName,"_RunSettings.csv",sep=""))

###-------------------------------------------------------------------------------
## Save workspace
save.image(file=paste(stockName,"_",maxYear,"_EqSim_Workspace.Rdata",sep=""))

