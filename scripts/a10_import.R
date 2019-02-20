
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
eths_all <- c("Black", "Hispanic", "White", "Other")  # For popsizes, to remove Others from census estimates
infection <- "GC"

nages <- length(ages)
nyears <- length(years)
neths <- length(eths)
neths_all <- length(eths_all)

##### Read in the pop size weights
wts_f <- wts_m <- array(dim=c(neths_all, nages, nyears))

for (i in 1:length(years)) {
    filename <- paste(datapath, "/propsexrace_allrace_", years[i], ".txt", sep="")
    temp <- read.csv(filename)
    for (j in 1:neths_all) {
      wts_f[j,,i] <- unname(unlist(
          temp %>% filter(sex=="Female", race4==eths_all[j]) %>% select(starts_with("Age"))
      ))
      wts_m[j,,i] <- unname(unlist(
          temp %>% filter(sex=="Male", race4==eths_all[j]) %>% select(starts_with("Age"))
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

eversex_f <- eversex_f %>% replace_na(0)
eversex_m <- eversex_m %>% replace_na(0)

### Read in the condom numbers
## Condoms are expressed differently than previous values, as popsizes for no and yes separately

condom_f <- condom_m <- array(dim=c(neths, nages, nyears))
condom_wts_f <- condom_wts_m <- array(dim=c(neths, nages, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/condom_", years[i], ".txt", sep="")
  temp <- read.csv(filename)
  temp$freq <- temp$freq %>% replace_na(0)
  for (j in 1:neths) {
    for (k in 1:nages) {
      if(nrow(temp %>% filter(sex_active=="Female", race==eths[j], age==ages[k]))==0) {  ## Cases where row is missing altogether
         condom_f[j,k,i] <- 0
         condom_wts_f[j,k,i] <- 0
      } else {
        if(sum(temp %>% filter(sex_active=="Female", race==eths[j], age==ages[k]) %>% select(freq))==0) {  ## Cases where freq is 0 (either in the original data, or as a replacement for NA as done above)
          condom_f[j,k,i] <- 0
          condom_wts_f[j,k,i] <- 0
        } else {
          condom_f[j,k,i] <- unname(unlist(
            temp %>% filter(sex_active=="Female", race==eths[j], age==ages[k], condomuse=="Yes") %>% select(freq) / 
              sum(temp %>% filter(sex_active=="Female", race==eths[j], age==ages[k]) %>% select(freq))
            ))
          condom_wts_f[j,k,i] <- sum(temp %>% filter(sex_active=="Female", race==eths[j], age==ages[k]) %>% select(freq))
      }}
      if(nrow(temp %>% filter(sex_active=="Male", race==eths[j], age==ages[k]))==0) {  ## Cases where row is missing altogether
        condom_m[j,k,i] <- 0
        condom_wts_m[j,k,i] <- 0
      } else {
        if(sum(temp %>% filter(sex_active=="Male", race==eths[j], age==ages[k]) %>% select(freq))==0) {  ## Cases where freq is 0 (either in the original data, or as a replacement for NA as done above)
          condom_m[j,k,i] <- 0
          condom_wts_m[j,k,i] <- 0
        } else {
          condom_m[j,k,i] <- unname(unlist(
            temp %>% filter(sex_active=="Male", race==eths[j], age==ages[k], condomuse=="Yes") %>% select(freq) / 
              sum(temp %>% filter(sex_active=="Male", race==eths[j], age==ages[k]) %>% select(freq))
          ))
          condom_wts_m[j,k,i] <- sum(temp %>% filter(sex_active=="Male", race==eths[j], age==ages[k]) %>% select(freq))
      }}
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

####################
# Total pops 13-18

totpopyears <- c(2011, 2013, 2015, 2017)
filename <- paste(datapath, "/totalpops.csv", sep="")
temp <- read.csv(filename)
totpop_f <- totpop_m <- array(dim=c(neths, nages, nyears))

for (i in 1:length(totpopyears)) {
  for (j in 1:neths) {
    totpop_f[j,,which(years==totpopyears[i])] <- unname(unlist(
      temp %>% filter(Year==totpopyears[i], Sex=="Female", Race==eths[j]) %>% select(starts_with("Age"))
    ))
    totpop_m[j,,which(years==totpopyears[i])] <- unname(unlist(
      temp %>% filter(Year==totpopyears[i], Sex=="Male", Race==eths[j]) %>% select(starts_with("Age"))
    ))
  }
}

####################
# Diagnoses
# For now we input the diagnoses in the first year only then use the model to generate the rest
# Will eventually want to bring in all to compare
# But note that this gets tricky because CDC stopped imputing missing attributes in 2010,
# and race is missing for a large proportion of cases. So that needs to be dealt with.

dx_10_14_f <- dx_10_14_m <- dx_15_19_f <- dx_15_19_m <- array(dim=c(neths, 1, nyears))
dx_f <- dx_m <- array(dim=c(neths, nages, nyears))

for (i in 1:nyears) {
  filename <- paste(datapath, "/diagnoses_", years[1], ".csv", sep="")
  temp <- read.csv(filename)
  
  # No need to prorate for 2007 - CDC prorated the data in the reports until 2009
  for (j in 1:neths) {
    dx_10_14_f[j,,i] <- unname(unlist(
      temp %>% filter(Infection==infection, Sex=="F", Ethn==eths_all[j], Age=="10-14", !is.na(Ethn)) %>% 
        select(Rate)
    ))
    dx_15_19_f[j,,i] <- unname(unlist(
      temp %>% filter(Infection==infection, Sex=="F", Ethn==eths_all[j], Age=="15-19", !is.na(Ethn)) %>% 
        select(Rate)
    ))
    dx_10_14_m[j,,i] <- unname(unlist(
      temp %>% filter(Infection==infection, Sex=="M", Ethn==eths_all[j], Age=="10-14", !is.na(Ethn)) %>% 
        select(Rate)
    ))
    dx_15_19_m[j,,i] <- unname(unlist(
      temp %>% filter(Infection==infection, Sex=="M", Ethn==eths_all[j], Age=="15-19", !is.na(Ethn)) %>% 
        select(Rate)
    ))
  }  
}

dx_f[,1:2,1] <- dx_10_14_f[,,1]*(meanpop_13to18_f[,1:2,1])/1e5
dx_f[,3:6,1] <- dx_15_19_f[,,1]*(meanpop_13to18_f[,3:6,1])/1e5
dx_m[,1:2,1] <- dx_10_14_m[,,1]*(meanpop_13to18_m[,1:2,1])/1e5
dx_m[,3:6,1] <- dx_15_19_m[,,1]*(meanpop_13to18_m[,3:6,1])/1e5
