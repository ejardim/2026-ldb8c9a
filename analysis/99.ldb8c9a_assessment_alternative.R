#####################################################################
# 20260609 EJ
# Megrin in 8c9a assessment alternative for ADG
#####################################################################

library(FLa4a)
library(ggplotFL)
library(FLasher)

load("../out/ldb8c9ainputs.RData")
load("../out/ldb8c9aIndices.RData")

stock@catch.n['0',as.character(1986:1998)] <- NA

stk <- stock
stk06 <- setPlusGroup(stk, 6)
idx <- tun.sel[c(1,4)]

#====================================================================
# official assessment
#====================================================================
fmod <- ~factor(age) + factor(year)
qmod <- list(~I(1/(1 + exp(-age))), ~I(1/(1 + exp(-age))))
fit <- sca(stk, idx, fmodel=fmod, qmodel=qmod)
res <- residuals(fit, stk, idx)
plot(res)
cdiag <- computeCatchDiagnostics(fit, stk)
plot(cdiag)
plot(stk + simulate(fit, 250))

#====================================================================
# time varying fleet selectivity (scenario 01)
#====================================================================
fmod_tvs <- ~ s(age, k=4) + s(year, k=10) + te(age, year, k=c(4,10))
fit01 <- sca(stk, idx, fmodel=fmod_tvs, qmodel=qmod)
res01 <- residuals(fit01, stk, idx)
plot(res01)
cdiag01 <- computeCatchDiagnostics(fit01, stk)
plot(cdiag01)
plot(stk + simulate(fit01, 250))

#====================================================================
# time varying fleet selectivity + smooth qs (scenario 02)
#====================================================================
qmod_sq <- list(~s(age, k=8), ~te(age, year, k=c(6,3)))
fit02 <- sca(stk, idx, fmodel=fmod_tvs, qmodel=qmod_sq)
res02 <- residuals(fit02, stk, idx)
plot(res02)
cdiag02 <- computeCatchDiagnostics(fit02, stk)
plot(cdiag02)
plot(stk + simulate(fit02, 250))

#====================================================================
# time varying fleet selectivity + smooth qs + bevholt (scenario 03)
#====================================================================
srmod_bh <- ~bevholt(CV=0.2)
fit03 <- sca(stk, idx, fmodel=fmod_tvs, qmodel=qmod_sq, srmodel=srmod_bh)
res03 <- residuals(fit03, stk, idx)
plot(res03)
cdiag03 <- computeCatchDiagnostics(fit03, stk)
plot(cdiag03)
plot(stk + simulate(fit03, 250))

#====================================================================
# time varying fleet selectivity + recruitment F + smooth qs (scenario 04)
#====================================================================
fmod_tvs0 <- ~ s(age, k=5) + te(age, year, k=c(4,20)) + s(year, k=10, by=as.numeric(age==0))
fit04 <- sca(stk, idx, fmodel=fmod_tvs0, qmodel=qmod_sq)
res04 <- residuals(fit04, stk, idx)
plot(res04)
cdiag04 <- computeCatchDiagnostics(fit04, stk)
plot(cdiag04)
plot(stk + simulate(fit04, 250))

#====================================================================
# time varying fleet selectivity + recruitment F + smooth qs (scenario 04)
#====================================================================
fmod_by <- ~ s(age, k=7) + s(year, k=5, by=breakpts(age, 0:7-0.5))
fit05 <- sca(stk, idx, fmodel=fmod_by, qmodel=qmod_sq, srmodel=srmod_bh)
res05 <- residuals(fit05, stk, idx)
plot(res05)
cdiag05 <- computeCatchDiagnostics(fit05, stk)
plot(cdiag05)
plot(stk + simulate(fit05, 250))


