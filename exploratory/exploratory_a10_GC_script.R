
##########################################################################
# Overview notes
# Parameters are all passed in as matrices, 
#   with rows representing race/ethnicity (B, H, W)
#   and columns represeting ages (13:18)

##########################################################################
# Init conditions

# Num femls, by race/eth by age by year
init_n_f <- mat3(c(13000, 13000, 13000, 13000, 13000, 13000,
                  18000, 18000, 18000, 18000, 18000, 18000,
                  67000, 67000, 67000, 67000, 67000, 67000))

# Num males at start of model (by race/eth and sex)
init_n_m <- mat3(c(13000, 13000, 13000, 13000, 13000, 13000,
                  18000, 18000, 18000, 18000, 18000, 18000,
                  67000, 67000, 67000, 67000, 67000, 67000))

# Proportion F sexually debuted (as FSM) at start of model (by race/eth and sex)
init_sexdeb_f <- mat3(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                        0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                        0.1, 0.2, 0.3, 0.4, 0.5, 0.6))  

# Proportion M sexually debuted (as MSF) at start of model (by race/eth and sex)
init_sexdeb_m <- mat3(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                        0.1, 0.2, 0.3, 0.4, 0.5, 0.6,
                        0.1, 0.2, 0.3, 0.4, 0.5, 0.6))

# Initial STI prevalence among those who have sexually debuted (by race/eth by age)
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

## Percent debuting? is this how we will do this???
# Or use back-calcuation?
# These are initial values, then it gets changed with the change values below
# So may be worth calling these init
#pc_debuting_f <- mat3(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
#                        0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
#                        0.1, 0.1, 0.1, 0.1, 0.1, 0.1))
#
#pc_debuting_m <- mat3(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
#                        0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
#                        0.1, 0.1, 0.1, 0.1, 0.1, 0.1))

##### New partners per year? 
# Same question - I think use back-calculation

# Coital acts
# These are initial values, then it gets changed with the change values below
init_coital_acts_pp_f <- mat3(c(0.001, 0.002, 0.003, 0.005, 0.007, 0.010,
                        0.001, 0.002, 0.003, 0.005, 0.007, 0.010,
                        0.001, 0.002, 0.003, 0.005, 0.007, 0.010))

init_coital_acts_pp_m <- mat3(c(0.001, 0.002, 0.003, 0.005, 0.007, 0.010,
                        0.001, 0.002, 0.003, 0.005, 0.007, 0.010,
                        0.001, 0.002, 0.003, 0.005, 0.007, 0.010))

# Condom use
# These are initial values, then it gets changed with the change values below
init_condom_use_f <- mat3(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

init_condom_use_m <- mat3(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                       0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                       0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

#######################################
# Diagnoses and duration parameters

init_diags_f <- mat3(c(50, 100, 150, 200, 250, 300,
                  50, 100, 150, 200, 250, 300,
                  100, 200, 300, 400, 500, 600))

init_diags_m <- mat3(c(50, 100, 150, 200, 250, 300,
                  50, 100, 150, 200, 250, 300,
                  100, 200, 300, 400, 500, 600))

perc_diag_f <- mat3(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                      0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
  
perc_diag_m <- mat3(c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                      0.2, 0.2, 0.2, 0.2, 0.2, 0.2,
                      0.2, 0.2, 0.2, 0.2, 0.2, 0.2))

infdur_f <- mat3(c(0.48, 0.48, 0.48, 0.48, 0.48, 0.48,
                      0.48, 0.48, 0.48, 0.48, 0.48, 0.48,
                      0.48, 0.48, 0.48, 0.48, 0.48, 0.48))

infdur_m <- mat3(c(0.24, 0.24, 0.24, 0.24, 0.24, 0.24,
                   0.24, 0.24, 0.24, 0.24, 0.24, 0.24,
                   0.24, 0.24, 0.24, 0.24, 0.24, 0.24))

#######################################
# Change parameters

ann_chg_npartners <- c(-0.01, -0.01, -0.01)
ann_chg_coital <- c(0, 0, 0)
ann_chg_condoms <- c(0.01, 0.01, 0.01)

##############################################################
# Calculate initial prevalence form diagnoses and duration data

a10_init_prev_f <- initPrev_calc(init_diags_f, perc_diag_f,infdur_f)
a10_init_prev_m <- initPrev_calc(init_diags_m, perc_diag_m,infdur_m)

##############################################################
# Back-calculate new partners per year usung ppy_calc

#nnppy_f <-
#nnppy_m <-

##############################################################
# Run the main model

a10_gc01 <- a10(init_prev_f=init_prev_f, 
                init_prev_m=init_prev_m,
                init_sexdeb_f=init_sexdeb_f,
                init_sexdeb_m=init_sexdeb_m,
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

##### TO DO
# Fill in ppy_calc
# Do basic logic in a10
# Think about whether change parameters should be done that way, or just as absolutes
# How to do percent debuting from given data?
# So much more
