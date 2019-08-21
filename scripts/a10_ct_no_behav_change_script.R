
#########################################################################
### Call main function

a10_ct_nbc <- a10(n_f = n_f, 
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

#########################################################################
### Process results

save(a10_ct_nbc, file='../output/a10_ct_nbc.rda')

