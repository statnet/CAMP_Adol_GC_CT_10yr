
nruns <- 5

### Draw coefficient samples

coefs_condom_f <- mvrnorm(n = nruns, 
          mu = condom_f_reg$coefficients, Sigma = vcov(condom_f_reg))

coefs_condom_m <- mvrnorm(n = nruns, 
          mu = condom_m_reg$coefficients, Sigma = vcov(condom_m_reg))

coefs_eversex_f <- mvrnorm(n = nruns, 
          mu = eversex_f_reg$coefficients, Sigma = vcov(eversex_f_reg))

coefs_eversex_m <- mvrnorm(n = nruns, 
          mu = eversex_m_reg$coefficients, Sigma = vcov(eversex_m_reg))

coefs_mnppy_f <- mvrnorm(n = nruns, 
          mu = mnppy_f_reg$coefficients, Sigma = vcov(mnppy_f_reg))

coefs_mnppy_m <- mvrnorm(n = nruns, 
          mu = mnppy_m_reg$coefficients, Sigma = vcov(mnppy_m_reg))

a10_gc_loop <- a10_ct_nbc_loop <- list()    # Lists of 100 outcomes
  
##### Run loop - each model over 100 coefficient sets
  
for (i in 1:nruns) {
  
  ## Determine predicted values for each of the six regressions
  condom_f_reg$coefficients <- coefs_condom_f[i,]
  pred_condom_f <- pred_condom_f_dyn <- 
    array(predict(condom_f_reg, type='response', 
    newdata= pred_condom_f_df_indep), dim=c(3,6,11))
  
  condom_m_reg$coefficients <- coefs_condom_m[i,]
  pred_condom_m <- pred_condom_m_dyn <-
    array(predict(condom_m_reg, type='response', 
    newdata= pred_condom_m_df_indep), dim=c(3,6,11))
  
  eversex_f_reg$coefficients <- coefs_eversex_f[i,]
  pred_eversex_f <- pred_eversex_f_dyn <-
    array(predict(eversex_f_reg, type='response', 
    newdata= pred_eversex_f_df_indep), dim=c(3,6,11))
  
  eversex_m_reg$coefficients <- coefs_eversex_m[i,]
  pred_eversex_m <- pred_eversex_m_dyn  <-
    array(predict(eversex_m_reg, type='response', 
    newdata= pred_eversex_m_df_indep), dim=c(3,6,11))
  
  mnppy_f_reg$coefficients <- coefs_mnppy_f[i,]
  pred_mnppy_f <- pred_mnppy_f_dyn <-
    array(predict(mnppy_f_reg, type='response', 
    newdata= pred_mnppy_f_df_indep), dim=c(3,6,11))
  
  mnppy_m_reg$coefficients <- coefs_mnppy_m[i,]
  pred_mnppy_m <- pred_mnppy_m_dyn <-  
    array(predict(mnppy_m_reg, type='response', 
    newdata= pred_mnppy_m_df_indep), dim=c(3,6,11))
  
  ### Calibrate GC model
  
  source("a10_looped_ABC_gc_calibration_pt1.R")              # GC calib pt 1 (starting with non-age-specific dx)
  source("a10_looped_ABC_gc_calibration_pt1_sim.R")          # simulate from GC calib pt 1 to get starting age-specific dx
  source("a10_looped_ABC_gc_calibration_pt2.R")              # GC calib pt 2 (starting with non-age-specific dx)
  source("a10_looped_ABC_gc_calibration_pt2_sim.R")          # GC calib pt 2 (starting with age-specific dx)
  
  ### Run GC obs model
  source("a10_looped_ABC_gc_obs_behav_change.R")             # Observed behavior change
  a10_gc_obs_loop[[i]] <- a10_gc_nbc

  
  
  source("a10_looped_ABC_gc_no_behav_change_script.R")       # No behavior change
  ## Assign predicted values of 2007 to all other years for NBC
  pred_condom_f_nbc <- array(dim=c(3,6,11), 
      data = rep(pred_condom_f_obs[,,1],11))
  pred_condom_m_nbc <- array(dim=c(3,6,11), 
      data = rep(pred_condom_m_obs[,,1],11))
  pred_eversex_f_nbc <- array(dim=c(3,6,11), 
      data = rep(pred_eversex_f_obs[,,1],11))
  pred_eversex_m_nbc <- array(dim=c(3,6,11), 
      data = rep(pred_eversex_m_obs[,,1],11))
  pred_mnppy_f_nbc <- array(dim=c(3,6,11), 
      data = rep(pred_mnppy_f_obs[,,1],11))
  pred_mnppy_m_nbc <- array(dim=c(3,6,11), 
      data = rep(pred_mnppy_m_obs[,,1],11))
  
  ### Run GC obs model
  a10_gc_nbc_100[[i]] <- a10(
    n_f = n_f, n_m = n_m,
    prop_eversex_f = pred_eversex_f_nbc,
    prop_eversex_m = pred_eversex_m_nbc,
    condom_use_f = pred_condom_f_nbc,
    condom_use_m = pred_condom_m_nbc,
    mean_new_part_f = pred_mnppy_f_nbc,
    mean_new_part_m = pred_mnppy_m_nbc,
    coital_acts_pp_f = capp_f,
    coital_acts_pp_m = capp_m,
    p_ethn_f = p_ethn_f,
    p_ethn_m = p_ethn_m,
    diag_init_f = dx_gc_init_tot_f,
    diag_init_m = dx_gc_init_tot_m,
    prop_diag_f = prop_diag_f_gc,
    prop_diag_m = prop_diag_m_gc,
    dur_inf_f = dur_f_gc,
    dur_inf_m = dur_m_gc,
    beta_f2m = beta_ipv_gc,
    beta_m2f = beta_rpv_gc,
    meanpop_tot_f = meanpop_13to18_f,
    meanpop_tot_m = meanpop_13to18_m,
    part_prev_ratio_f = part_prev_ratio_gc_f,
    part_prev_ratio_m = part_prev_ratio_gc_m
  )
}

save(a10_gc_nbc_100, a10_gc_obs_100, file='../../output/looped_ABC/a100_gc_ci.rda')
