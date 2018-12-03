
library(dplyr)
library(tidyverse)
library(magrittr)

# Make sure working directory is in /scripts/
setwd("C:/git/CAMP_10yr_proj/scripts")

datapath <- "../data"
#file_classes <- c("propsexrace.txt", "eversex.txt", "condom.txt", 
#                  "matrix1.txt", "matrix2.txt")

years <- seq(2007, 2017, by=2)
ages <- 13:18
eths <- c("Black", "Hispanic", "White")

nages <- length(ages)
nyears <- length(years)
neths <- length(eths)

##### Read in the pop size weights
wts_f <- wts_m <- array(dim=c(neths, nages, nyears))

for (i in 1:length(years)) {
    filename <- paste(datapath, "/propsexrace_", years[i], ".txt", sep="")
    temp <- read.csv(filename)
    for (j in 1:neths) {
      wts_f[j,,i] <- unname(unlist(
          temp %>% filter(sex=="Female", race==eths[j]) %>% select(starts_with("Age"))
      ))
      wts_m[j,,i] <- unname(unlist(
          temp %>% filter(sex=="Male", race==eths[j]) %>% select(starts_with("Age"))
      ))
    }
}

wts_f <- wts_f %>% replace_na(0)
wts_m <- wts_m %>% replace_na(0)

#### Read in the eversex numbers
# NB:the name of the sex column is different hear than for wts

eversex_f <- eversex_m <- array(dim=c(neths, nages, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/eversex_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    eversex_f[j,,i] <- unname(unlist(
      temp %>% filter(sex_active=="Female", race==eths[j]) %>% select(starts_with("Age"))
    ))
    eversex_m[j,,i] <- unname(unlist(
      temp %>% filter(sex_active=="Male", race==eths[j]) %>% select(starts_with("Age"))
    ))
  }
}

### Read in the condom numbers
## Condoms are expressed differently than previous values, as popsizes for no and yes separately

condom_f <- condom_m <- array(dim=c(neths, nages, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/condom_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    for (k in 1:nages) {
      if(nrow(temp %>% filter(sex_active=="Female", race==eths[j], age==ages[k], condomuse=="Yes"))==0) {
         condom_f[j,k,i] <- NA
      } else {
         condom_f[j,k,i] <- unname(unlist(
            temp %>% filter(sex_active=="Female", race==eths[j], age==ages[k], condomuse=="Yes") %>% select(freq) / 
              sum(temp %>% filter(sex_active=="Female", race==eths[j], age==ages[k]) %>% select(freq))
         ))
      }
    }
  }
}


### Read in matrix1 (number by race by current age by age of debut by year)
## notice stop-gap in terms of dim 3 size

AgeByDebutAge_num_f <- AgeByDebutAge_num_m <- array(dim=c(neths, nages, 7, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/matrix1_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    AgeByDebutAge_num_f[j,,,i] <- unname(as.matrix(temp %>% 
                        filter(sex_active=="Female", race==eths[j]) %>% 
                        select(starts_with("age1")), nages))
    AgeByDebutAge_num_m[j,,,i] <- unname(as.matrix(temp %>% 
                        filter(sex_active=="Male", race==eths[j]) %>% 
                        select(starts_with("age1")), nages))
  }
}

### Read in matrix2 (mean lifetime partners by race by current age by age of debut by year)

AgeByDebutAge_lp_f <- AgeByDebutAge_lp_m <- array(dim=c(neths, nages, 7, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/matrix2_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  for (j in 1:neths) {
    AgeByDebutAge_lp_f[j,,,i] <- unname(as.matrix(temp %>% 
                                 filter(sex_active=="Female", race==eths[j]) %>% 
                                 select(starts_with("mean1")), nages))
    AgeByDebutAge_lp_m[j,,,i] <- unname(as.matrix(temp %>% 
                                 filter(sex_active=="Male", race==eths[j]) %>% 
                                 select(starts_with("mean1")), nages))
  }
}

####################
# Total HS pop sizes
filename <- paste(datapath, "/schoolpops.csv", sep="")
schoolpops <- read.csv(filename)
plot(schoolpops, ylim=c(min(schoolpops$totschoolpop)*0.9,max(schoolpops$totschoolpop*1.1)), type='b')
abline(h=mean(schoolpops$totschoolpop))
(max(schoolpops$totschoolpop) - min(schoolpops$totschoolpop))/mean(schoolpops$totschoolpop)

### To be moved
bbb <- lm(condom_f[1,,1]~ages, weights = wts_f[1,,1])
plot(ages,condom_f[1,,1], ylim=c(0,1))
lines(14:18,predict(bbb))
ccc <- lm(condom_f[1,,6]~ages, weights = wts_f[1,,6])
points(ages,condom_f[1,,6], ylim=c(0,1))
lines(14:18,predict(ccc))
