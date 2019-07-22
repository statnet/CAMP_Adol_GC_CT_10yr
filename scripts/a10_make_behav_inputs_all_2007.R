#########################################################################
### Put the actual dynamic numbers separate objects so they can be recalled later

pred_eversex_f_dyn <- pred_eversex_f
pred_eversex_m_dyn <- pred_eversex_m
pred_condom_f_dyn <- pred_condom_f
pred_condom_m_dyn <- pred_condom_m
pred_mnppy_f_dyn <- pred_mnppy_f
pred_mnppy_m_dyn <- pred_mnppy_m

#########################################################################
### Change all post-2007 numbers to match 2007 (ie as if no behavior change)

# n_f and n_m already equal across ages
# meanpop_13to18_f and meanpop_13to18_m already equal across ages
# capp_f and capp_m already equal across ages
# diagnoses_init_tot only for 2007


for (i in 2:dim(pred_eversex_f)[3]) pred_eversex_f[,,i] <- pred_eversex_f[,,1]
for (i in 2:dim(pred_eversex_m)[3]) pred_eversex_m[,,i] <- pred_eversex_m[,,1]
for (i in 2:dim(pred_condom_f)[3]) pred_condom_f[,,i] <- pred_condom_f[,,1]
for (i in 2:dim(pred_condom_m)[3]) pred_condom_m[,,i] <- pred_condom_m[,,1]
for (i in 2:dim(pred_mnppy_f)[3]) pred_mnppy_f[,,i] <- pred_mnppy_f[,,1]
for (i in 2:dim(pred_mnppy_m)[3]) pred_mnppy_m[,,i] <- pred_mnppy_m[,,1]

save.image("../output/a10_inputs_all_2007.rda")
