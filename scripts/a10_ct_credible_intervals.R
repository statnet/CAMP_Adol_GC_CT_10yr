
##########################################################
### Determining credibile intervals for CT


### Draw coefficient samples (NB: one could use the same draws for GC and CT)

coefs_condom_f_ct <- mvrnorm(n = 100, 
          mu = condom_f_reg$coefficients,
          Sigma = vcov(condom_f_reg))

coefs_condom_m_ct <- mvrnorm(n = 100, 
          mu = condom_m_reg$coefficients,
          Sigma = vcov(condom_m_reg))

coefs_eversex_f_ct <- mvrnorm(n = 100, 
          mu = eversex_f_reg$coefficients,
          Sigma = vcov(eversex_f_reg))

coefs_eversex_m_ct <- mvrnorm(n = 100, 
          mu = eversex_m_reg$coefficients,
          Sigma = vcov(eversex_m_reg))

coefs_mnppy_f_ct <- mvrnorm(n = 100, 
          mu = mnppy_f_reg$coefficients,
          Sigma = vcov(mnppy_f_reg))

coefs_mnppy_m_ct <- mvrnorm(n = 100, 
          mu = mnppy_m_reg$coefficients,
          Sigma = vcov(mnppy_m_reg))


##### Set up temporary objects

condom_f_reg_temp <- condom_f_reg             # glm objects for each coefficient 
condom_m_reg_temp <- condom_m_reg
eversex_f_reg_temp <- eversex_f_reg
eversex_m_reg_temp <- eversex_m_reg
mnppy_f_reg_temp <- mnppy_f_reg
mnppy_m_reg_temp <- mnppy_m_reg

a10_ct_obs_100 <- a10_ct_nbc_100 <- list()    # Lists of 100 outcomes


##### Run loop - each model over 100 coefficient sets

for (i in 1:100) {
  
  ## Determine predicted values for each of the six regressions
  condom_f_reg_temp$coefficients <- coefs_condom_f_ct[i,]
  pred_condom_f_temp_obs <- 
    array(predict(condom_f_reg_temp, type='response', 
    newdata= pred_condom_f_df_indep), dim=c(3,6,11))
  
  condom_m_reg_temp$coefficients <- coefs_condom_m_ct[i,]
  pred_condom_m_temp_obs <- 
    array(predict(condom_m_reg_temp, type='response', 
    newdata= pred_condom_m_df_indep), dim=c(3,6,11))
  
  eversex_f_reg_temp$coefficients <- coefs_eversex_f_ct[i,]
  pred_eversex_f_temp_obs <- 
    array(predict(eversex_f_reg_temp, type='response', 
    newdata= pred_eversex_f_df_indep), dim=c(3,6,11))
  
  eversex_m_reg_temp$coefficients <- coefs_eversex_m_ct[i,]
  pred_eversex_m_temp_obs <- 
    array(predict(eversex_m_reg_temp, type='response', 
    newdata= pred_eversex_m_df_indep), dim=c(3,6,11))
  
  mnppy_f_reg_temp$coefficients <- coefs_mnppy_f_ct[i,]
  pred_mnppy_f_temp_obs <- 
    array(predict(mnppy_f_reg_temp, type='response', 
    newdata= pred_mnppy_f_df_indep), dim=c(3,6,11))
  
  mnppy_m_reg_temp$coefficients <- coefs_mnppy_m_ct[i,]
  pred_mnppy_m_temp_obs <- 
    array(predict(mnppy_m_reg_temp, type='response', 
    newdata= pred_mnppy_m_df_indep), dim=c(3,6,11))
  
  ### Run ct obs model
  a10_ct_obs_100[[i]] <- a10(
      n_f = n_f, n_m = n_m,
      prop_eversex_f = pred_eversex_f_temp_obs,
      prop_eversex_m = pred_eversex_m_temp_obs,
      condom_use_f = pred_condom_f_temp_obs,
      condom_use_m = pred_condom_m_temp_obs,
      mean_new_part_f = pred_mnppy_f_temp_obs,
      mean_new_part_m = pred_mnppy_m_temp_obs,
      coital_acts_pp_f = capp_f,
      coital_acts_pp_m = capp_m,
      p_ethn_f = p_ethn_f,
      p_ethn_m = p_ethn_m,
      diag_init_f = dx_ct_init_tot_f,
      diag_init_m = dx_ct_init_tot_m,
      prop_diag_f = prop_diag_f_ct,
      prop_diag_m = prop_diag_m_ct,
      dur_inf_f = dur_f_ct,
      dur_inf_m = dur_m_ct,
      beta_f2m = beta_ipv_ct,
      beta_m2f = beta_rpv_ct,
      meanpop_tot_f = meanpop_13to18_f,
      meanpop_tot_m = meanpop_13to18_m,
      part_prev_ratio_f = part_prev_ratio_ct_f,
      part_prev_ratio_m = part_prev_ratio_ct_m
  )
  
  ## Assign predicted values of 2007 to all other years for NBC
  pred_condom_f_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_condom_f_temp_obs[,,1],11))
  pred_condom_m_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_condom_m_temp_obs[,,1],11))
  pred_eversex_f_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_eversex_f_temp_obs[,,1],11))
  pred_eversex_m_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_eversex_m_temp_obs[,,1],11))
  pred_mnppy_f_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_mnppy_f_temp_obs[,,1],11))
  pred_mnppy_m_nbc_temp <- array(dim=c(3,6,11), 
      data = rep(pred_mnppy_m_temp_obs[,,1],11))
  
  ### Run ct obs model
  a10_ct_nbc_100[[i]] <- a10(
    n_f = n_f, n_m = n_m,
    prop_eversex_f = pred_eversex_f_nbc_temp,
    prop_eversex_m = pred_eversex_m_nbc_temp,
    condom_use_f = pred_condom_f_nbc_temp,
    condom_use_m = pred_condom_m_nbc_temp,
    mean_new_part_f = pred_mnppy_f_nbc_temp,
    mean_new_part_m = pred_mnppy_m_nbc_temp,
    coital_acts_pp_f = capp_f,
    coital_acts_pp_m = capp_m,
    p_ethn_f = p_ethn_f,
    p_ethn_m = p_ethn_m,
    diag_init_f = dx_ct_init_tot_f,
    diag_init_m = dx_ct_init_tot_m,
    prop_diag_f = prop_diag_f_ct,
    prop_diag_m = prop_diag_m_ct,
    dur_inf_f = dur_f_ct,
    dur_inf_m = dur_m_ct,
    beta_f2m = beta_ipv_ct,
    beta_m2f = beta_rpv_ct,
    meanpop_tot_f = meanpop_13to18_f,
    meanpop_tot_m = meanpop_13to18_m,
    part_prev_ratio_f = part_prev_ratio_ct_f,
    part_prev_ratio_m = part_prev_ratio_ct_m
  )
}

#plot(sapply(1:100, function(x) sum(a10_ct_nbc_temp[[x]]$n_inc_insch_f[,,11])), 
#     sapply(1:100, function(x) sum(a10_ct_obs_100[[x]]$n_inc_insch_f[,,11])))

save(a10_ct_nbc_100, a10_ct_obs_100, file='../output/a100_ct_ci.rda')