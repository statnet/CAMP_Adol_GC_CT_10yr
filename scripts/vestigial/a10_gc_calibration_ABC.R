
# library(EasyABC)
# rm(list=ls())
# setwd("C:/git/CAMP_10yr_proj/scripts/")
# source("a10_no_behav_change_script.R")

###############################################
## Check runs from first ABC

#boxplot(nbc_gc_ABC$param)
#source("a10_gc_calibration_ABC_check.R")
#calib_test_gc(nbc_gc_ABC, "calib_test_gc_step1_f.pdf", "calib_test_gc_step1_m.pdf")

###############################################
## Pick version to seed next round and determine the 
## distribution of diagnoses by age it implies

#minrun <- which(nbc_gc_ABC$stats==min(nbc_gc_ABC$stats))

#part_prev_ratio_f <- as.vector(nbc_gc_ABC$param[minrun,1:3])
#part_prev_ratio_m <- as.vector(nbc_gc_ABC$param[minrun,4:6])

a10_nbc_abc_step2 <- a10(n_f = n_f, 
                          n_m = n_m,
                          prop_eversex_f = pred_eversex_f,
                          prop_eversex_m = pred_eversex_m,
                          condom_use_f = pred_condom_f,
                          condom_use_m = pred_condom_m,
                          mean_new_part_f = pred_mnppy_f,
                          mean_new_part_m = pred_mnppy_m,
                          coital_acts_pp_f = capp_f,
                          coital_acts_pp_m = capp_m,
                          p_ethn_f = p_ethn_f,
                          p_ethn_m = p_ethn_m,
                          diag_init_f = diagnoses_init_tot_f_gc,
                          diag_init_m = diagnoses_init_tot_m_gc,
                          prop_diag_f = prop_diag_f_gc,
                          prop_diag_m = prop_diag_m_gc,
                          dur_inf_f = dur_f_gc,
                          dur_inf_m = dur_m_gc,
                          beta_f2m = beta_ipv_gc,
                          beta_m2f = beta_rpv_gc,
                          meanpop_tot_f = meanpop_13to18_f,
                          meanpop_tot_m = meanpop_13to18_m,
                          part_prev_ratio_f = part_prev_ratio_f,
                          part_prev_ratio_m = part_prev_ratio_m
        )

##########################################################
## Set diagnoses by age for next stage; correct it for any 
## discrapancy between total from the original nad the ABC

diagnoses_init_tot_f_gc_step1 <- diagnoses_init_tot_f_gc
diagnoses_init_tot_m_gc_step1 <- diagnoses_init_tot_m_gc

correction_f <- diagnoses_init_tot_f_gc/
  rowSums(a10_nbc_abc_step2$n_diag_total_f[,,max(cal_times)])
diagnoses_init_tot_f_gc <- correction_f *
  a10_nbc_abc_step2$n_diag_total_f[,,max(cal_times)]

correction_m <- diagnoses_init_tot_m_gc/
  rowSums(a10_nbc_abc_step2$n_diag_total_m[,,max(cal_times)])
diagnoses_init_tot_m_gc <- correction_m *
  a10_nbc_abc_step2$n_diag_total_m[,,max(cal_times)]

### NOTE: ONCE CODE IS RUN BEYOND HERE, ONE CANNOT RE-RUN ANY CODE
### ABOVE WITHOUT STARTING OVER, SINCE DIAGNOSES-INIT-TOT-F-GC HAS CHANGED
###  (This allows us not to have to rewrite the whole model argument for 
###  the ABC)


#nbc_gc_tolerance=c(10, 5, 2.5, 1, 0.5, 0.25#, 0.1, 0.05, 0.025, 
                   #0.01, 0.005, 0.0025, 0.001
#                   )

nbc_gc_ABC_step2 <- ABC_sequential(method="Beaumont",
                           model=nbc_gc_model,
                           prior=nbc_gc_priors,
                           nb_simul=100,
                           summary_stat_target=0,
                           tolerance_tab=nbc_gc_tolerance,
                           verbose=TRUE,
                           progress_bar=TRUE)

### Check Round 2 results
boxplot(nbc_gc_ABC_step2$param)
calib_test_gc(nbc_gc_ABC_step2, "calib_test_gc_step2_f.pdf", "calib_test_gc_step2_m.pdf")

###############################################
## Check one final run 

minrun <- which(nbc_gc_ABC_step2$stats==min(nbc_gc_ABC_step2$stats))

part_prev_ratio_f <- as.vector(nbc_gc_ABC_step2$param[minrun,1:3])
part_prev_ratio_m <- as.vector(nbc_gc_ABC_step2$param[minrun,4:6])

a10_nbc_abc_step3 <- a10(n_f = n_f, 
                         n_m = n_m,
                         prop_eversex_f = pred_eversex_f,
                         prop_eversex_m = pred_eversex_m,
                         condom_use_f = pred_condom_f,
                         condom_use_m = pred_condom_m,
                         mean_new_part_f = pred_mnppy_f,
                         mean_new_part_m = pred_mnppy_m,
                         coital_acts_pp_f = capp_f,
                         coital_acts_pp_m = capp_m,
                         p_ethn_f = p_ethn_f,
                         p_ethn_m = p_ethn_m,
                         diag_init_f = diagnoses_init_tot_f_gc,
                         diag_init_m = diagnoses_init_tot_m_gc,
                         prop_diag_f = prop_diag_f_gc,
                         prop_diag_m = prop_diag_m_gc,
                         dur_inf_f = dur_f_gc,
                         dur_inf_m = dur_m_gc,
                         beta_f2m = beta_ipv_gc,
                         beta_m2f = beta_rpv_gc,
                         meanpop_tot_f = meanpop_13to18_f,
                         meanpop_tot_m = meanpop_13to18_m,
                         part_prev_ratio_f = part_prev_ratio_f,
                         part_prev_ratio_m = part_prev_ratio_m
)

apply(a10_nbc_abc_step3$n_diag_insch_f, c(1,3), sum)

part_prev_ratio_f_gc_calib <- part_prev_ratio_f
part_prev_ratio_m_gc_calib <- part_prev_ratio_m
save(part_prev_ratio_f_gc_calib, 
     part_prev_ratio_m_gc_calib, 
     file="part_prev_ratios_gc_calib.rda")

diagnoses_init_tot_f_gc_calib <- 
  a10_nbc_abc_step3$n_diag_total_f[,,11] * 
  diagnoses_init_tot_f_gc_step1/
  rowSums(diagnoses_init_tot_f_gc_calib)

diagnoses_init_tot_m_gc_calib <- 
  a10_nbc_abc_step3$n_diag_total_m[,,11] * 
  diagnoses_init_tot_m_gc_step1/
  rowSums(diagnoses_init_tot_m_gc_calib)

save(diagnoses_init_tot_f_gc_calib, 
     diagnoses_init_tot_m_gc_calib, 
     file="diagnoses_init_tot_gc_calib.rda")
