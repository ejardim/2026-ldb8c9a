# output.R - DESC
# /output.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
library(icesAdvice)
library(flextable)
library(writexl)
library(data.table)


setwd("D:/WorkingGroups/2026_WG/2026WGBIE/a4a/ldb8c9a/data")


source("utilities.R")

mkdir("output")

# LOAD assessment and forecast results

load("../out/fitldb/fitDef/model/runs.RData")

# MODEL, ADVICE and FORECAST year

dy <- dims(runs[[1]])$maxyear
ay <- dy + 1
fy <- ay + 1


# --- Advice sheet

#  - TABLE - Intermediate year assumptions

tab1 <- interimTable(runs$Fmsy)

write_xlsx(tab1, '../out/fitldb/fitDef/intermediate_assumptions.xlsx')

# - TABLE - Annual catch scenarios

tab2 <- catchOptionsTable(runs, advice=advice, tac=tac,
  ages=c(2,4), discards.ages=c(0,2))

write_xlsx(tab2, '../out/fitldb/fitDef/catch_optionsv2.xlsx')



