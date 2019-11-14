firstrun <- T

if(firstrun) {
  source("a10_masterscript.R")
  n_f = n_f 
  n_m = n_m
  prop_eversex_f = pred_eversex_f
  prop_eversex_m = pred_eversex_m
  condom_use_f = pred_condom_f
  condom_use_m = pred_condom_m
  mean_new_part_f = pred_mnppy_f
  mean_new_part_m = pred_mnppy_m
  coital_acts_pp_f = capp_f
  coital_acts_pp_m = capp_m
  diag_init_f = diagnoses_init_tot_f_gc
  diag_init_m = diagnoses_init_tot_m_gc
  prop_diag_f = prop_diag_f_gc
  prop_diag_m = prop_diag_m_gc
  dur_inf_f = dur_f_gc
  dur_inf_m = dur_m_gc
  beta_f2m = beta_ipv_gc
  beta_m2f = beta_rpv_gc
  save(n_f, n_m, prop_eversex_f, prop_eversex_m,
       condom_use_f, condom_use_m,
       mean_new_part_f, mean_new_part_m,
       coital_acts_pp_f, coital_acts_pp_m,
       diag_init_f, diag_init_m, 
       prop_diag_f, prop_diag_m, 
       dur_inf_f, dur_inf_m,beta_f2m, beta_m2f, 
       file = 'temp_inputs_for_testing_a10.rda')
}

rm(list=ls())
load("temp_inputs_for_testing_a10.rda")
