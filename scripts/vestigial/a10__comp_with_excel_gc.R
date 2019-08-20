rm(list=ls())
totschpop <-  16985786 

n_f <- array11(mat3(c(
          rep(totschpop*0.1794/3, 3), rep(totschpop*0.2430/2, 2), totschpop*0.0650, rep(0,12)
       )))

n_m <- array11(mat3(c(
          rep(totschpop*0.1834/3, 3), rep(totschpop*0.2455/2, 2), totschpop*0.0835, rep(0,12)
        )))

pred_eversex_f <- array11(mat3(c(
          rep(0.1842, 3), rep(0.4123, 2), 0.5266, rep(0,12)
        )))

pred_eversex_m <- array11(mat3(c(
          rep(0.2237, 3), rep(0.4165, 2), 0.5002, rep(0,12)
        )))

pred_condom_f <- array11(mat3(c(
          rep(0.6113, 3), rep(0.5669, 2), 0.5204, rep(0,12)
        )))

pred_condom_m <- array11(mat3(c(
          rep(0.6930, 3), rep(0.6526, 2), 0.6091, rep(0,12)
        )))

pred_mnppy_f <- array11(mat3(c(
          rep(1.38, 3), rep(1.31, 2), 1.31, rep(0,12)
        )))

pred_mnppy_m <- array11(mat3(c(
          rep(1.32, 3), rep(1.32, 2), 1.32, rep(0,12)
        )))

capp_f <- array11(mat3(c(
          rep(9.4, 3), rep(24.7, 2), 46.7, rep(0,12)
        )))

capp_m <- array11(mat3(c(
          rep(11.9, 3), rep(19.3, 2), 29.3, rep(0,12)
        )))

p_ethn_f <- p_ethn_m <- mat3(c(1,0,0,0,1,0,0,0,1))

beta_rpv_gc <- 0.58556
beta_ipv_gc <- 0.11638

prop_diag_f_gc <- 0.523
prop_diag_m_gc <- 0.490

dur_f_gc <- 0.46
dur_m_gc <- 0.23


totalpop_f <- array11(mat3(c(
          rep(6075700/3, 3), rep(4138668/2, 2), 2060801, rep(0,12)
        )))

totalpop_m <- array11(mat3(c(
          rep(6324725/3, 3), rep(4331557/2, 2), 2158502, rep(0,12)
        )))

diagnoses_init_f_gc <- c(40562, 0, 0)
diagnoses_init_m_gc <- c(20198, 0, 0)

part_prev_ratio_f <- part_prev_ratio_m <- c(1,1,1)

a10_gc_sync <- a10(n_f = n_f, 
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
                diag_init_f = diagnoses_init_f_gc,
                diag_init_m = diagnoses_init_m_gc,
                prop_diag_f = prop_diag_f_gc,
                prop_diag_m = prop_diag_m_gc,
                dur_inf_f = dur_f_gc,
                dur_inf_m = dur_m_gc,
                beta_f2m = beta_ipv_gc,
                beta_m2f = beta_rpv_gc,
                meanpop_tot_f = totalpop_f,
                meanpop_tot_m = totalpop_m,
                part_prev_ratio_f = part_prev_ratio_f,
                part_prev_ratio_m = part_prev_ratio_m
)

