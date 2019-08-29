##########################################################
## CT calibration pt 2

##########################################################
## Set diagnoses by age for next stage; correct it for any 
## discrepancy between total from the original nad the ABC

correction_f <- dx_ct_init_tot_f/
  rowSums(a10_calib_ct_pt1_sim$n_diag_total_f[,,max(cal_times)])
dx_ct_init_tot_f <- correction_f *
  a10_calib_ct_pt1_sim$n_diag_total_f[,,max(cal_times)]

correction_m <- dx_ct_init_tot_m/
  rowSums(a10_calib_ct_pt1_sim$n_diag_total_m[,,max(cal_times)])
dx_ct_init_tot_m <- correction_m *
  a10_calib_ct_pt1_sim$n_diag_total_m[,,max(cal_times)]


###  Could choose to change priors and tolerance for CT here if necessary

calib_ct_pt2_tolerance <- calib_ct_pt1_tolerance
calib_ct_pt2_priors <- calib_ct_pt1_priors 

### ABC run
a10_calib_ct_pt2 <- ABC_sequential(method="Beaumont",
                           model=a10_ABC_ct,
                           prior=calib_ct_pt2_priors,
                           nb_simul=100,
                           summary_stat_target=0,
                           tolerance_tab=calib_ct_pt2_tolerance,
                           verbose=TRUE,
                           progress_bar=TRUE)

save(a10_calib_ct_pt2, file='../output/a10_calib_ct_pt2.rda')
