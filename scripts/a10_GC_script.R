
##########################################################################
# Overview notes
# Parameters are all passed in as matrices, 
#   with rows representing race/ethnicity (B, H, W)

##########################################################################
# Init conditions

# Num femls by ages 13-18 at start of model
nf_init <- mat3(c(1000, 1000, 1000, 1000, 1000, 1000,
                  1000, 1000, 1000, 1000, 1000, 1000,
                  1000, 1000, 1000, 1000, 1000, 1000))
               

# Num males by ages 13-18 at start of model
nm_init <- mat3(c(1000, 1000, 1000, 1000, 1000, 1000,
                  1000, 1000, 1000, 1000, 1000, 1000,
                  1000, 1000, 1000, 1000, 1000, 1000))

# Proportion sexually debuted (as opposite-sex) at start of model

sexdeb_init_f <- mat3(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                        0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                        0.1, 0.2, 0.3, 0.4, 0.5, 0.6))  

sexdeb_init_m <- mat3(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                        0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                        0.1, 0.2, 0.3, 0.4, 0.5, 0.6))

# Initial prevalence among those who have sexually debuted

init_prev_f <- mat3(c(0.003, 0.006, 0.011, 0.021, 0.038, 0.050,
                      0.003, 0.006, 0.011, 0.021, 0.038, 0.050,
                      0.003, 0.006, 0.011, 0.021, 0.038, 0.050))

init_prev_m <- mat3(c(0.001, 0.002, 0.003, 0.006, 0.012, 0.020,
                      0.001, 0.002, 0.003, 0.006, 0.012, 0.020,
                      0.001, 0.002, 0.003, 0.006, 0.012, 0.020))

##########################################################################
# Trans probs

beta_m2f <- 0.25
beta_f2m <- 0.25

##########################################################################
# Sex behavior

pc_debuting_f <- mat3(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                        0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                        0.1, 0.1, 0.1, 0.1, 0.1, 0.1))

pc_debuting_m <- mat3(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                        0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                        0.1, 0.1, 0.1, 0.1, 0.1, 0.1))

##### New partners per year? Or somehow use the 

coital_acts_pp_f <- mat3(c(0.001, 0.002, 0.003, 0.005, 0.007, 0.010,
                        0.001, 0.002, 0.003, 0.005, 0.007, 0.010,
                        0.001, 0.002, 0.003, 0.005, 0.007, 0.010))

coital_acts_pp_m <- mat3(c(0.001, 0.002, 0.003, 0.005, 0.007, 0.010,
                        0.001, 0.002, 0.003, 0.005, 0.007, 0.010,
                        0.001, 0.002, 0.003, 0.005, 0.007, 0.010))

condom_use_f <- mat3(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

condom_use_m <- mat3(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                       0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                       0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

#######################################
# Change parameters

ann_chg_npartners <- c(-0.01, -0.01, -0.01)
ann_chg_coital <- c(0, 0, 0)
ann_chg_condoms <- c(0.01, 0.01, 0.01)

a10_gc01 <- a10(nf_init=nf_init, 
    nm_init=nm_init,
    sexdeb_init_f=sexdeb_init_f,
    sexdeb_init_m=sexdeb_init_m,
    init_prev_f=init_prev_f,
    init_prev_m=init_prev_m,
    beta_m2f=beta_m2f,
    beta_f2m=beta_f2m,
    pc_debuting_f=pc_debuting_f,
    pc_debuting_m=pc_debuting_m,
    coital_acts_pp_f=coital_acts_pp_f,
    coital_acts_pp_m=coital_acts_pp_m,
    condom_use_f=condom_use_f,
    condom_use_m=condom_use_m,
    ann_chg_npartners=ann_chg_npartners,
    ann_chg_coital=ann_chg_coital,
    ann_chg_condoms=ann_chg_condoms
)


