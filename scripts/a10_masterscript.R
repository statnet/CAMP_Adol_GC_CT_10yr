
################################################
# teen-SPARC 10-year extension master script
# 
# Notes: 
#   1. Change the setwd() line if needed


### Basics
setwd("C:/git/CAMP_10yr_proj/scripts/")
rm(list=ls())

# Get all inputs
source("a10_import.R")

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

n_f <- array11(apply(abspopsizes_f, 1:2, mean))
n_m <- array11(apply(abspopsizes_m, 1:2, mean))


#
if(F) {
  a10_gc01 <- a10(n_f=n_f, n_m=n_m,
                    init_sexdeb_f=init_sexdeb_f,
                    init_sexdeb_m=init_sexdeb_m
#                    beta_m2f=beta_m2f,
#                    beta_f2m=beta_f2m,
#                    pc_debuting_f=pc_debuting_f,
#                    pc_debuting_m=pc_debuting_m,
#                    coital_acts_pp_f=coital_acts_pp_f,
#                    coital_acts_pp_m=coital_acts_pp_m,
#                    condom_use_f=condom_use_f,
#                    condom_use_m=condom_use_m,
#                    ann_chg_npartners=ann_chg_npartners,
#                    ann_chg_coital=ann_chg_coital,
#                    ann_chg_condoms=ann_chg_condoms
  )
}

# Save image
save.image()
