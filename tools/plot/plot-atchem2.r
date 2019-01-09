# -----------------------------------------------------------------------------
#
# Copyright (c) 2017 Sam Cox, Roberto Sommariva
#
# This file is part of the AtChem2 software package.
#
# This file is covered by the MIT license which can be found in the file
# LICENSE.md at the top level of the AtChem2 distribution.
#
# -----------------------------------------------------------------------------

## plotting tool for the AtChem2 model output
## --> R version
##
## ARGUMENT:
## - directory with the model output
##
## USAGE:
##   Rscript --vanilla ./tools/plot/plot-atchem2.r ./model/output/
## ---------------------------------------------- ##
cmd_args <- commandArgs(trailingOnly = TRUE)
setwd(cmd_args[1])
getwd()

df1 <- read.table("speciesConcentrations.output", header=T)
df2 <- read.table("environmentVariables.output", header=T)
df3 <- read.table("photolysisRates.output", header=T)
df4 <- read.table("photolysisRatesParameters.output", header=T)

nc1 <- ncol(df1)
nc2 <- ncol(df2)
nc3 <- ncol(df3)
nc4 <- ncol(df4)

## ---------------------------- ##

cairo_pdf("atchem2_output.pdf", onefile=T, width=11, height=7)

## speciesConcentrations.output
par(mfrow=c(3,2))
for (i in 2:nc1) {
  plot(df1[[1]], df1[[i]], type="l",
       main=colnames(df1[i]), xlab="seconds", ylab="")
}

## environmentVariables.output
par(mfrow=c(3,2))
for (i in 2:nc2) {
  plot(df2[[1]], df2[[i]], type="l",
       main=colnames(df2[i]), xlab="seconds", ylab="")
}

## photolysisRates.output
par(mfrow=c(3,2))
for (i in 2:nc3) {
  plot(df3[[1]], df3[[i]], type="l",
       main=colnames(df3[i]), xlab="seconds", ylab="")
}

## photolysisRatesParameters.output
par(mfrow=c(3,2))
for (i in 2:nc4) {
  plot(df4[[1]], df4[[i]], type="l",
       main=colnames(df4[i]), xlab="seconds", ylab="")
}

dev.off()

## ---------------------------- ##

cat("\n===> atchem2_output.pdf created in directory:", cmd_args[1], "\n\n")
