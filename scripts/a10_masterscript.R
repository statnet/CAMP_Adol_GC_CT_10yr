
################################################
# teen-SPARC 10-year extension master script
# 
# Notes: 
#   1. Change the setwd() line if needed


### Basics
setwd("C:/git/CAMP_10yr_proj/scripts/")
rm(list=ls())

# Get all inputs
source("CAMP_10yr_import.R")

# Get pop sizes
years <- seq(2007, 2017, by=2)
nyears <- length(years)

abspopsizes_f <- wts_f
for (i in 1:nyears) {
  abspopsizes_f[,,i] <- abspopsizes_f[,,i]* 
    unname(unlist(
      (schoolpops %>% filter(year==years[i]) %>% select("totschoolpop"))
    )) / 
    sum(wts_f[,,i])
}

abspopsizes_m <- wts_m
for (i in 1:nyears) {
  abspopsizes_m[,,i] <- abspopsizes_m[,,i]* 
    unname(unlist(
      (schoolpops %>% filter(year==years[i]) %>% select("totschoolpop"))
    )) / 
    sum(wts_m[,,i])
}

n_f <- apply(abspopsizes_f, 1:2, mean)
n_m <- apply(abspopsizes_m, 1:2, mean)

# Save image
save.image()
