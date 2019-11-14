

### The main ABC function for CT. This really oughta be in the R folder as a package 
###   function, but at the moment I'm too lazy to carry over all the arguments 

a10_ABC_ct <- function(part_prev_ratio) {
  part_prev_ratio_f <- as.vector(part_prev_ratio[1:3])
  part_prev_ratio_m <- as.vector(part_prev_ratio[4:6])
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
                    prop_diag_f = prop_diag_f_ct,
                    prop_diag_m = prop_diag_m_ct,
                    dur_inf_f = dur_f_ct,
                    dur_inf_m = dur_m_ct,
                    beta_f2m = beta_ipv_ct,
                    beta_m2f = beta_rpv_ct,
                    meanpop_tot_f = meanpop_13to18_f,
                    meanpop_tot_m = meanpop_13to18_m,
                    part_prev_ratio_f = part_prev_ratio_f,
                    part_prev_ratio_m = part_prev_ratio_m,
                    diag_init_f = dx_ct_init_tot_f,
                    diag_init_m = dx_ct_init_tot_m
  )
  
  if(is.vector(dx_ct_init_tot_f) & length(dx_ct_init_tot_f)==3) {
    result <- sum(
      sapply(cal_times, function(x) {
        sum(abs(rowSums(a10_output$n_diag_total_f[,,x]) - dx_ct_init_tot_f)) + 
          sum(abs(rowSums(a10_output$n_diag_total_m[,,x]) - dx_ct_init_tot_m)) 
      })
    )
  }
  if(is.matrix(dx_ct_init_tot_f) & sum(dim(dx_ct_init_tot_f)==c(3,6))==2) {
    result <- sum(
      sapply(cal_times, function(x) {
        sum(abs(rowSums(a10_output$n_diag_total_f[,,x]) - rowSums(dx_ct_init_tot_f))) + 
          sum(abs(rowSums(a10_output$n_diag_total_m[,,x]) - rowSums(dx_ct_init_tot_m))) 
      })
    )
  }
  return(result)
}

