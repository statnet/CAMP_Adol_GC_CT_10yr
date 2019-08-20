
load("a10_results_2019_08_05.rda")

eversex_prop_f <- array(apply(pred_eversex_f_dyn[,,2:11], 3, "/", pred_eversex_f_dyn[,,1]), dim=c(3,6,10))
eversex_prop_m <- array(apply(pred_eversex_m_dyn[,,2:11], 3, "/", pred_eversex_m_dyn[,,1]), dim=c(3,6,10))

no_condom_amt_f <- array(apply(pred_condom_f_dyn[,,2:11], 3, "/", pred_condom_f_dyn[,,1]), dim=c(3,6,10))
no_condom_amt_m <- array(apply(pred_condom_m_dyn[,,2:11], 3, "/", pred_condom_m_dyn[,,1]), dim=c(3,6,10))

mnppy_prop_f <- array(apply(pred_mnppy_f_dyn[,,2:11], 3, "/", pred_mnppy_f_dyn[,,1]), dim=c(3,6,10))
mnppy_prop_m <- array(apply(pred_mnppy_m_dyn[,,2:11], 3, "/", pred_mnppy_m_dyn[,,1]), dim=c(3,6,10))

combined_f <- eversex_prop_f*mnppy_prop_f*no_condom_amt_f
combined_m <- eversex_prop_m*mnppy_prop_m*no_condom_amt_m

matplot2 <- function(x, ...) matplot(t(x), type='b', ...)
  
matplot2(eversex_prop_f[,,10], ylim=c(0,2))
matplot2(eversex_prop_m[,,10], ylim=c(0,2))
matplot2(no_condom_amt_f[,,10], ylim=c(0,2))
matplot2(no_condom_amt_m[,,10], ylim=c(0,2))
matplot2(mnppy_prop_f[,,10], ylim=c(0,2))
matplot2(mnppy_prop_m[,,10], ylim=c(0,2))

matplot2(combined_f[,,10], ylim=c(0,2))
matplot2(combined_m[,,10], ylim=c(0,2))

sum(combined_f[,,10] * n_f[,,11]) / sum(n_f[,,11])
sum(combined_m[,,10] * n_m[,,11]) / sum(n_m[,,11])


oneyear_redux_f <- (rowSums(combined_f[,,1] * n_f[,,2]) / rowSums(n_f[,,2]))
oneyear_redux_m <- rowSums(combined_m[,,1] * n_f[,,2]) / rowSums(n_m[,,2])

oneyear_redux_f^10
oneyear_redux_m^10
rowSums(combined_f[,,10] * n_f[,,11]) / rowSums(n_f[,,11])
rowSums(combined_m[,,10] * n_f[,,11]) / rowSums(n_m[,,11])

inc_redux <- a10_gc01$n_inc_insch_f[,,11] / a10_gc_nbc$n_inc_insch_f[,,11]

rowSums(inc_redux * n_f[,,11]) / rowSums(n_f[,,11])
  