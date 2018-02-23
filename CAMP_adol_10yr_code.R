
##########################################################################
# Init conditions

nf_init <- c(1000, 1000, 1000, 1000, 1000, 1000)  # Num het femls by ages 13-18 at start of model
nm_init <- c(1000, 1000, 1000, 1000, 1000, 1000)  # Num het males by ages 13-18 at start of model

#hetpt_f <- 0.96     # Percent of femls who will behave all or primarily opposite-sex partners
#hetpt_m <- 0.96     # Percent of males who will behave all or primarily opposite-sex partners

sexac_init_f <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
sexac_init_m <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6) 

init_prev_gc_f <- c(0.003, 0.006, 0.011, 0.021, 0.038, 0.050)
init_prev_gc_m <- c(0.001, 0.002, 0.003, 0.006, 0.012, 0.020)

init_prev_ct_f <- c(0.003, 0.006, 0.011, 0.021, 0.038, 0.050)*2
init_prev_ct_m <- c(0.001, 0.002, 0.003, 0.006, 0.012, 0.020)*2

##########################################################################
# Vital dynamics

asmr_f <- c(0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001)
asmr_m <- c(0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001)
btype <- "deaths"

##########################################################################
# Trans probs

beta_m2f_gc <- 0.25
beta_f2m_gc <- 0.25
beta_m2f_ct <- 0.25
beta_f2m_gc <- 0.25

##########################################################################
# Sex behavior

pc_debuting_f <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
pc_debuting_m <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

coital_freq_f <- c(0.001, 0.002, 0.003, 0.005, 0.007, 0.010)
coital_freq_m <- c(0.001, 0.002, 0.003, 0.005, 0.007, 0.010)

condom_use <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)

##########################################################################
# Init bookkeeping

nf_init_hsa <- nf_init * hetpt_f * sexac_init_f
nm_init_hsa <- nm_init * hetpt_m * sexac_init_m 

num_f <- num_m <- matrix(11,6)
num_f[,1] <- nf_init
num_m[,1] <- nm_init

##########################################################################
# Advancement

for (i in 1:10) {
  num_f[2:6,i+1] <- num_f[1:5,i] + PERC_DEBUTING * NUM_HET_F_UNDEBUTED - num_f[,i]*asmr_f
  num_m[2:6,i+1] <- num_m[1:5,i] + PERC_DEBUTING * NUM_HET_M_UNDEBUTED - num_m[,i]*asmr_m
}






