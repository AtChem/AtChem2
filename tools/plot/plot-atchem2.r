# -----------------------------------------------------------------------------
#
# Copyright (c) 2017 Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

## R plotting tool for AtChem2 model output
##
## SCRIPT ARGUMENT:
##   - model output directory
## ---------------------------------------------- ##
cmd_args <- commandArgs(trailingOnly = TRUE)

setwd(cmd_args[1]); getwd()

df1 <- read.table("concentration.output", header=T)
df2 <- read.table("envVar.output", header=T)
df3 <- read.table("photolysisRates.output", header=T)
df4 <- read.table("photoRateCalcParameters.output", header=T)

## ---------------------------------------------- ##
cairo_pdf("atchem2_output.pdf", onefile=T, width=14, height=7)
par(mfrow=c(3,2))

## concentration.output
for (v in 2:ncol(df1)) {
  plot(df1[[1]], df1[[v]], type="l",
       main=colnames(df1[v]), xlab="t", ylab="")
}

## envVar.output
for (v in 2:ncol(df2)) {
  plot(df2[[1]], df2[[v]], type="l",
       main=colnames(df2[v]), xlab="t", ylab="")
}

## photolysisRates.output
for (v in 2:ncol(df3)) {
  plot(df3[[1]], df3[[v]], type="l",
       main=colnames(df3[v]), xlab="t", ylab="")
}

## photoRateCalcParameters.output
for (v in 2:ncol(df4)) {
  plot(df4[[1]], df4[[v]], type="l",
       main=colnames(df4[v]), xlab="t", ylab="")
}

dev.off()
