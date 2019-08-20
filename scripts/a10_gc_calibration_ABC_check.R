
calib_test_gc <- function(obj, filename_f, filename_m) {

  nsims <- nrow(obj$param)
  
  calib_test_gc_f <- calib_test_gc_m <- array(dim=c(3,12,nsims))
  
  for (i in 1:nsims) {
    part_prev_ratio_f <- as.vector(obj$param[i,1:3])
    part_prev_ratio_m <- as.vector(obj$param[i,4:6])
    temp <- a10(n_f = n_f, 
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
                      part_prev_ratio_f = part_prev_ratio_f,
                      part_prev_ratio_m = part_prev_ratio_m
    )
    calib_test_gc_f[,,i] <- apply(temp$n_diag_total_f, c(1,3), sum)
    if (is.vector(dx_gc_init_tot_f)) {
      calib_test_gc_f[,1,i] <- dx_gc_init_tot_f
    } else {
      calib_test_gc_f[,1,i] <- rowSums(dx_gc_init_tot_f)
    }
  
    calib_test_gc_m[,,i] <- apply(temp$n_diag_total_m, c(1,3), sum)
    if (is.vector(dx_gc_init_tot_m)) {
      calib_test_gc_m[,1,i] <- dx_gc_init_tot_m
    } else {
      calib_test_gc_m[,1,i] <- rowSums(dx_gc_init_tot_m)
    }
    
  }
  
  pdf(filename_f)
  for(i in 1:nsims) matplot(t(calib_test_gc_f[,,i]), type='b')
  dev.off()
  
  pdf(filename_m)
  for(i in 1:nsims) matplot(t(calib_test_gc_m[,,i]), type='b')
  dev.off()
  
}