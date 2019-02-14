#years <- seq(2007, 2017, by=2)
#nyears <- 6
years <- 2007
nyears <- 1

infection <- "GC"

##### Read in the pop size weights
dx_10_14_f <- dx_10_14_m <- dx_15_19_f <- dx_15_19_m <- array(dim=c(neths, 1, nyears))
dx_f <- dx_m <- array(dim=c(neths, nages, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/diagnoses_", years[i], ".csv", sep="")
  temp <- read.csv(filename)

  # No need to prorate for 2007
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
