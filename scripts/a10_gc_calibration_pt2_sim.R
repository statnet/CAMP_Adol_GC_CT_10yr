#######################################################
### Pick run with smallest error, and simulate from it

minrun <- which(a10_calib_gc_pt2$stats==min(a10_calib_gc_pt2$stats))
part_prev_ratio_gc_f <- as.vector(a10_calib_gc_pt2$param[minrun,1:3])
part_prev_ratio_gc_m <- as.vector(a10_calib_gc_pt2$param[minrun,4:6])

a10_calib_gc_pt2_sim <- a10(n_f = n_f, 
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
                            part_prev_ratio_f = part_prev_ratio_gc_f,
                            part_prev_ratio_m = part_prev_ratio_gc_m
)

#apply(a10_calib_gc_pt2_sim$n_diag_insch_f, c(1,3), sum)

part_prev_ratio_gc_f_calib <- part_prev_ratio_gc_f
part_prev_ratio_gc_m_calib <- part_prev_ratio_gc_m
save(part_prev_ratio_gc_f_calib, 
     part_prev_ratio_gc_m_calib, 
     file="../output/part_prev_ratios_gc_calib.rda")

#dx_gc_init_tot_f_calib <- a10_calib_gc_pt2_sim$n_diag_total_f[,,11] * 
#              dx_gc_init_tot_f_step1 / rowSums(dx_gc_init_tot_f_calib)

#dx_gc_init_tot_m_calib <- a10_nbc_abc_step3$n_diag_total_m[,,11] * 
#              dx_gc_init_tot_m_step1 / rowSums(dx_gc_init_tot_m_calib)

save(dx_gc_init_tot_f, 
     dx_gc_init_tot_m, 
     file="../output/a10_init_dx_age_gc.rda")
