years <- seq(2007, 2017, by=2)
nyears <- 6
infection <- "CT"

##### Read in the pop size weights
diagnoses_f <- diagnoses_m <- 
  diagnoses_10_14_f <- diagnoses_10_14_m <- 
  diagnoses_15_19_f <- diagnoses_15_19_m <- 
  array(dim=c(neths, 1, nyears))

for (i in 1:length(years)) {
  filename <- paste(datapath, "/diagnoses_", years[i], ".csv", sep="")
  temp <- read.csv(filename)

  # No need to prorate for 2007
    diagnoses_10_14_f_temp <- temp %>% filter(Infection==infection, Sex=="F", Age=="10-14")
    diagnoses_15_19_f_temp <- temp %>% filter(Infection==infection, Sex=="F", Age=="15-19")
    
#    for (j in 1:neths) {
#      diagnoses_f[,,1] <- diagnoses_10_14_f[,,1]*

        diagnoses_10_14_f_temp %>% filter(Ethn==eths[j]) %>% select(Rate) 
        
        
#      diagnoses_15_19_f[,,1]
    

    
    
        
    for (j in 1:neths) {
      diagnoses_10_14_f[j,,i] <- unname(unlist(
      temp %>% filter(Infection==infection, Sex=="F", Ethn==eths[j], Age=="10-14") %>% select(Rate)))
    diagnoses_15_19_f[j,,i] <- unname(unlist(
      temp %>% filter(Infection==infection, Sex=="F", Ethn==eths[j], Age=="15-19") %>% select(Rate)))
    pop_10_14_f[j,,i] <- unname(unlist(
      temp %>% filter(Infection==infection, Sex=="F", Ethn==eths[j], Age=="10-14") %>% select(Rate)))
    pop_15_19_f[j,,i] <- unname(unlist(
      temp %>% filter(Infection==infection, Sex=="F", Ethn==eths[j], Age=="15-19") %>% select(Rate)))
  }
}



