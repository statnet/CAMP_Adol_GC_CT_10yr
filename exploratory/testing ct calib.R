steps_gc1 <- steps_gc2 <- 13
steps_ct1 <- 13
steps_ct2 <- 12

pdf('a10_calib_gc_pt1.pdf')
for (i in 1:steps_gc1) {
  boxplot(a10_calib_gc_pt1$intermediary[[i]]$posterior[,2:7], ylim=c(0,5))
}
dev.off()

pdf('a10_calib_gc_pt2.pdf')
for (i in 1:steps_gc2) {
  boxplot(a10_calib_gc_pt2$intermediary[[i]]$posterior[,2:7], ylim=c(0,5))
}
dev.off()

pdf('a10_calib_ct_pt1.pdf')
for (i in 1:steps_ct1) {
  boxplot(a10_calib_ct_pt1$intermediary[[i]]$posterior[,2:7], ylim=c(0,15))
}
dev.off()

pdf('a10_calib_ct_pt2.pdf')
for (i in 1:steps_ct2) {
  boxplot(a10_calib_ct_pt2$intermediary[[i]]$posterior[,2:7], ylim=c(0,15))
}
dev.off()


###########################################################
#### Does stats represent the absolute distance between 
#### input diagnoses and diagnoses at time 2?

aaa <- a10_calib_gc_pt2$param[1,]
part_prev_ratio_gc_f <- aaa[1:3]
part_prev_ratio_gc_m <- aaa[4:6]

bbb <- a10(n_f = n_f, 
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

sum(abs(rowSums(bbb$n_diag_total_f[,,2]) - rowSums(dx_gc_init_tot_f))) + 
  sum(abs(rowSums(bbb$n_diag_total_m[,,2]) - rowSums(dx_gc_init_tot_m))) 
a10_calib_gc_pt2$stats[1]

#### Yes, it does.

#######################################################

# Parameters through time

means_gc_1 <- matrix(NA, steps_gc1,6)
for(i in 1:steps_gc1) means_gc_1[i,] <- colMeans(a10_calib_gc_pt1$intermediary[[i]]$posterior)[2:7]
matplot(means_gc_1, type='l', ylim=c(0,5))

means_gc_2 <- matrix(NA, steps_gc2,6)
for(i in 1:steps_gc2) means_gc_2[i,] <- colMeans(a10_calib_gc_pt2$intermediary[[i]]$posterior)[2:7]
matplot(means_gc_2, type='l', ylim=c(0,5), add=T)

means_ct_1 <- matrix(NA, steps_ct1,6)
for(i in 1:steps_ct1) means_ct_1[i,] <- colMeans(a10_calib_ct_pt1$intermediary[[i]]$posterior)[2:7]
matplot(means_ct_1, type='l', ylim=c(0,15))

means_ct_2 <- matrix(NA, steps_ct1,6)
for(i in 1:steps_ct2) means_ct_2[i,] <- colMeans(a10_calib_ct_pt2$intermediary[[i]]$posterior)[2:7]
matplot(means_ct_2, type='l', ylim=c(0,15))

### Dist through time

meandist_gc_1 <- rep(NA,steps_gc1)
for(i in 1:steps_gc1) meandist_gc_1[i] <- mean(a10_calib_gc_pt1$intermediary[[i]]$posterior[,8])
plot(meandist_gc_1, type='l', log='y')

meandist_gc_2 <- rep(NA,steps_gc2)
for(i in 1:steps_gc2) meandist_gc_2[i] <- mean(a10_calib_gc_pt2$intermediary[[i]]$posterior[,8])
plot(meandist_gc_2, type='l', log='y')

mindist_gc_1 <- rep(NA,steps_gc1)
for(i in 1:steps_gc1) mindist_gc_1[i] <- min(a10_calib_gc_pt1$intermediary[[i]]$posterior[,8])
plot(mindist_gc_1, type='l', log='y')

mindist_gc_2 <- rep(NA,steps_gc2)
for(i in 1:steps_gc2) mindist_gc_2[i] <- min(a10_calib_gc_pt2$intermediary[[i]]$posterior[,8])
plot(mindist_gc_2, type='l', log='y')

meandist_ct_1 <- rep(NA,steps_ct1)
for(i in 1:steps_ct1) meandist_ct_1[i] <- mean(a10_calib_ct_pt1$intermediary[[i]]$posterior[,8])
plot(meandist_ct_1, type='l', log='y')

mindist_ct_1 <- rep(NA,steps_ct1)
for(i in 1:steps_ct1) mindist_ct_1[i] <- min(a10_calib_ct_pt1$intermediary[[i]]$posterior[,8])
plot(mindist_ct_1, type='l', log='y')

meandist_ct_2 <- rep(NA,steps_ct2)
for(i in 1:steps_ct2) meandist_ct_2[i] <- mean(a10_calib_ct_pt2$intermediary[[i]]$posterior[,8])
plot(meandist_ct_2, type='l', log='y')

mindist_ct_2 <- rep(NA,steps_ct2)
for(i in 1:steps_ct2) mindist_ct_2[i] <- min(a10_calib_ct_pt2$intermediary[[i]]$posterior[,8])
plot(mindist_ct_2, type='l', log='y')

