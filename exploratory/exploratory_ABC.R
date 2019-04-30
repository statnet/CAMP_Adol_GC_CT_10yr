nbc_gc_model <- function(x) {
  part_prev_ratio_f <- as.vector(x[1:3])
  part_prev_ratio_m <- as.vector(x[4:6])
  a10_output <- a10(n_f = n_f, 
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
  
  result <- sum(
    sapply(cal_times, function(x) {
      sum(abs(rowSums(a10_output$n_diag_f[,,x]) - diagnoses_init_tot_f_gc)) + 
        sum(abs(rowSums(a10_output$n_diag_m[,,x]) - diagnoses_init_tot_m_gc)) 
    })
  )
  return(result)
}

