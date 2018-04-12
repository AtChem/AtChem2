#!/cm/shared/apps/R/3.4.1/bin/Rscript --vanilla

setwd("modelOutput/")

df1 <- read.table("concentration.output", header=T)

plot.ts(df1)
