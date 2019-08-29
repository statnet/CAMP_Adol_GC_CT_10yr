
#############################################################
#### Results for paper "XXXX"

#### Table 1: regression coefficients

round(summary(eversex_f_reg)$coef[,c(1,2,4)],3)
round(summary(eversex_m_reg)$coef[,c(1,2,4)],3)
round(summary(mnppy_f_reg)$coef[,c(1,2,4)],3)
round(summary(mnppy_m_reg)$coef[,c(1,2,4)],3)
round(summary(condom_f_reg)$coef[,c(1,2,4)],3)
round(summary(condom_m_reg)$coef[,c(1,2,4)],3)


#### Figure 1: predicted values
surveyyears <- seq(2007, 2017, by=2)

bmp("../output/predicted.bmp")

  matplot(surveyyears, t(prop_eversex_f[2,,]), pch=16, ylim=c(0,1), 
          xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(6),
          xlab= "Survey year", 
          ylab="Prop. reporting ever having had sexual intercourse")
  
  matplot(surveyyears, t(pred_eversex_f_dyn[2,,c(1,3,5,7,9,11)]), 
          pch=16, type='b', add=T, lty=2, col=rainbow(6))
  
  legend(2011, y=1, 
         legend=c('Age 13 reported', 'Age 14 reported', 'Age 15 reported',
                  'Age 16 reported', 'Age 17 reported', 'Age 18 reported',
                  'Age 13 predicted', 'Age 14 predicted', 'Age 15 predicted',
                  'Age 16 predicted', 'Age 17 predicted', 'Age 18 predicted'),
         lty=rep(1:2,each=6),
         col=rep(rainbow(6),2), cex=0.75, ncol=2)
dev.off()


############################################################
### Short names for ease

fns_gc <- a10_gc_nbc$n_inc_insch_f  # Female no behavior change, school
fcs_gc <- a10_gc_obs$n_inc_insch_f    # etc.
fnt_gc <- a10_gc_nbc$n_inc_total_f
fct_gc <- a10_gc_obs$n_inc_total_f
mns_gc <- a10_gc_nbc$n_inc_insch_m
mcs_gc <- a10_gc_obs$n_inc_insch_m
mnt_gc <- a10_gc_nbc$n_inc_total_m
mct_gc <- a10_gc_obs$n_inc_total_m

fns_ct <- a10_ct_nbc$n_inc_insch_f  # Female no behavior change, school
fcs_ct <- a10_ct_obs$n_inc_insch_f    # etc.
fnt_ct <- a10_ct_nbc$n_inc_total_f
fct_ct <- a10_ct_obs$n_inc_total_f
mns_ct <- a10_ct_nbc$n_inc_insch_m
mcs_ct <- a10_ct_obs$n_inc_insch_m
mnt_ct <- a10_ct_nbc$n_inc_total_m
mct_ct <- a10_ct_obs$n_inc_total_m

plotyears <- 2008:2017

############################################################
## Helper functions
asum <- function(x, y) apply(x, y, sum)

sum_nbc_f_gc <- function(x, dim) asum(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_nbc_m_gc <- function(x, dim) asum(a10_gc_nbc_100[[x]]$n_inc_insch_m[,,3:12], dim)
sum_obs_f_gc <- function(x, dim) asum(a10_gc_obs_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_obs_m_gc <- function(x, dim) asum(a10_gc_obs_100[[x]]$n_inc_insch_m[,,3:12], dim)

sum_nbc_f_ct <- function(x, dim) asum(a10_ct_nbc_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_nbc_m_ct <- function(x, dim) asum(a10_ct_nbc_100[[x]]$n_inc_insch_m[,,3:12], dim)
sum_obs_f_ct <- function(x, dim) asum(a10_ct_obs_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_obs_m_ct <- function(x, dim) asum(a10_ct_obs_100[[x]]$n_inc_insch_m[,,3:12], dim)

#array(sapply(1:100, function(x) sum_nbc_f(x,c(1,3))), c(3,10,100))
errbar <- function(x, up, low) arrows(x, low, x, up, length=0.05, angle=90, code=3)


##############################################################
## Total NIAs with CIs

# Have to do colSums because helper fxn can't sum across all dimesions at once
temp_nbc_gc <- colSums(sapply(1:100, function(x) sum_nbc_f_gc(x,1) + sum_nbc_m_gc(x,1))) 
temp_obs_gc <- colSums(sapply(1:100, function(x) sum_obs_f_gc(x,1) + sum_obs_m_gc(x,1))) 
nia_gc_tot_pt <- sum(fns_gc[,,3:12]) - sum(fcs_gc[,,3:12]) + 
                 sum(mns_gc[,,3:12]) - sum(mcs_gc[,,3:12])
nia_gc_tot_lb <- quantile(temp_nbc_gc - temp_obs_gc, 0.025)
nia_gc_tot_ub <- quantile(temp_nbc_gc - temp_obs_gc, 0.975)
round(c(nia_gc_tot_pt, nia_gc_tot_lb, nia_gc_tot_ub),1)

temp_nbc_ct <- colSums(sapply(1:100, function(x) sum_nbc_f_ct(x,1) + sum_nbc_m_ct(x,1))) 
temp_obs_ct <- colSums(sapply(1:100, function(x) sum_obs_f_ct(x,1) + sum_obs_m_ct(x,1))) 
nia_ct_tot_pt <- sum(fns_ct[,,3:12]) - sum(fcs_ct[,,3:12]) + 
                 sum(mns_ct[,,3:12]) - sum(mcs_ct[,,3:12])
nia_ct_tot_lb <- quantile(temp_nbc_ct - temp_obs_ct, 0.025)
nia_ct_tot_ub <- quantile(temp_nbc_ct - temp_obs_ct, 0.975)
round(c(nia_ct_tot_pt, nia_ct_tot_lb, nia_ct_tot_ub),1)


##############################################################
## Total PIAs with CIs

temp_nbc_gc <- colSums(sapply(1:100, function(x) sum_nbc_f_gc(x,1) + sum_nbc_m_gc(x,1))) 
temp_obs_gc <- colSums(sapply(1:100, function(x) sum_obs_f_gc(x,1) + sum_obs_m_gc(x,1))) 
pia_gc_tot_pt <- (sum(fns_gc[,,3:12]) - sum(fcs_gc[,,3:12]) + 
                  sum(mns_gc[,,3:12]) - sum(mcs_gc[,,3:12])) / 
                 (sum(fns_gc[,,3:12]) + sum(mns_gc[,,3:12]))
pia_gc_tot_lb <- quantile((temp_nbc_gc-temp_obs_gc)/temp_nbc_gc, 0.025)
pia_gc_tot_ub <- quantile((temp_nbc_gc-temp_obs_gc)/temp_nbc_gc, 0.975)
round(100*c(pia_gc_tot_pt, pia_gc_tot_lb, pia_gc_tot_ub),1)

temp_nbc_ct <- colSums(sapply(1:100, function(x) sum_nbc_f_ct(x,1) + sum_nbc_m_ct(x,1))) 
temp_obs_ct <- colSums(sapply(1:100, function(x) sum_obs_f_ct(x,1) + sum_obs_m_ct(x,1))) 
pia_ct_tot_pt <- (sum(fns_ct[,,3:12]) - sum(fcs_ct[,,3:12]) + 
                    sum(mns_ct[,,3:12]) - sum(mcs_ct[,,3:12])) / 
  (sum(fns_ct[,,3:12]) + sum(mns_ct[,,3:12]))
pia_ct_tot_lb <- quantile((temp_nbc_ct-temp_obs_ct)/temp_nbc_ct, 0.025)
pia_ct_tot_ub <- quantile((temp_nbc_ct-temp_obs_ct)/temp_nbc_ct, 0.975)
round(100*c(pia_ct_tot_pt, pia_ct_tot_lb, pia_ct_tot_ub),1)


##############################################################
## Total NIAs by year and sex (Table 2)

temp_nbc_gc <- sapply(1:100, function(x) sum_nbc_f_gc(x,3)) 
temp_obs_gc <- sapply(1:100, function(x) sum_obs_f_gc(x,3)) 
nia_gc_f_year_pt <- (asum(fns_gc[,,3:12],3) - asum(fcs_gc[,,3:12],3))
nia_gc_f_year_lb <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.025))
nia_gc_f_year_ub <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.975))
table2a <- round(cbind(nia_gc_f_year_pt, nia_gc_f_year_lb, nia_gc_f_year_ub),1)

temp_nbc_gc <- sapply(1:100, function(x) sum_nbc_m_gc(x,3)) 
temp_obs_gc <- sapply(1:100, function(x) sum_obs_m_gc(x,3)) 
nia_gc_m_year_pt <- (asum(mns_gc[,,3:12],3) - asum(mcs_gc[,,3:12],3))
nia_gc_m_year_lb <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.025))
nia_gc_m_year_ub <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.975))
table2b <- round(cbind(nia_gc_m_year_pt, nia_gc_m_year_lb, nia_gc_m_year_ub),1)

#colSums(table2a * costs$GC_F) + colSums(table2b * costs$GC_M)

# CT

temp_nbc_ct <- sapply(1:100, function(x) sum_nbc_f_ct(x,3)) 
temp_obs_ct <- sapply(1:100, function(x) sum_obs_f_ct(x,3)) 
nia_ct_f_year_pt <- (asum(fns_ct[,,3:12],3) - asum(fcs_ct[,,3:12],3))
nia_ct_f_year_lb <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.025))
nia_ct_f_year_ub <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.975))
table2c <- round(cbind(nia_ct_f_year_pt, nia_ct_f_year_lb, nia_ct_f_year_ub),1)

temp_nbc_ct <- sapply(1:100, function(x) sum_nbc_m_ct(x,3)) 
temp_obs_ct <- sapply(1:100, function(x) sum_obs_m_ct(x,3)) 
nia_ct_m_year_pt <- (asum(mns_ct[,,3:12],3) - asum(mcs_ct[,,3:12],3))
nia_ct_m_year_lb <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.025))
nia_ct_m_year_ub <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.975))
table2d <- round(cbind(nia_ct_m_year_pt, nia_ct_m_year_lb, nia_ct_m_year_ub),1)

colSums(table2c * costs$CT_F) + colSums(table2d * costs$CT_M)
annual_savings_ct <- table2c * costs$CT_F + table2d * costs$CT_M


bmp("../output/costs.gc.bmp")
plot(plotyears, annual_savings_gc[,1]/1e6, pch=15, 
     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), 
     ylim=c(min(annual_savings_gc/1e6), max(annual_savings_gc/1e6)), 
     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
     xlab="Year", ylab="Costs saved (US $mil)",
     main="Costs saved, Gonorrhea"
)
errbar(plotyears, annual_savings_gc[,2]/1e6, annual_savings_gc[,3]/1e6)

plot(plotyears, annual_savings_ct[,1]/1e6, pch=15, 
     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), 
     ylim=c(min(annual_savings_ct/1e6), max(annual_savings_ct/1e6)), 
     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
     xlab="Year", ylab="Costs saved (US $mil)",
     main="Costs saved, Chlamydia"
)
errbar(plotyears, annual_savings_ct[,2]/1e6, annual_savings_ct[,3]/1e6)

dev.off()


if(F) {
##############################################################
## Total PIAs with CIs

temp_nbc <- sapply(1:100, function(x) 
  apply(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,3:12], 1, sum))

temp_obs <- sapply(1:100, function(x) 
  apply(a10_gc_obs_100[[x]]$n_inc_insch_f[,,3:12], 1, sum))

nia_gc_by_race_f_pt <- (asum(fns[,,3:12], 1) - asum(fcs[,,3:12], 1))
nia_gc_by_race_f_lb <- apply((temp_nbc-temp_obs), 1, quantile, c(0.025))
nia_gc_by_race_f_ub <- apply((temp_nbc-temp_obs), 1, quantile, c(0.975))

round(cbind(nia_gc_by_race_f_pt,
            nia_gc_by_race_f_lb,
            nia_gc_by_race_f_ub),
      1)

round(colSums(cbind(nia_gc_by_race_f_pt,
            nia_gc_by_race_f_lb,
            nia_gc_by_race_f_ub)
            ), 1)

##############################################################








pia_gc_by_race_sex_pt <- (asum(fns[,,3:12], c(1,3)) - asum(fcs[,,3:12], c(1,3)))/
  asum(fns[,,3:12], c(1,3))



#########################################
## race-specific year-specific PIAs      

aaa <- array(sapply(1:100, function(x) 
  apply(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,3:12], c(1,3), sum)), 
  dim=c(3,10,100))

bbb <- array(sapply(1:100, function(x) 
  apply(a10_gc_obs_100[[x]]$n_inc_insch_f[,,3:12], c(1,3), sum)), 
  dim=c(3,10,100))

ptest <- (asum(fns[,,3:12], c(1,3)) - asum(fcs[,,3:12], c(1,3)))/
          asum(fns[,,3:12], c(1,3))

ub <- apply((aaa-bbb)/aaa, 1:2, quantile, c(0.975))
lb <- apply((aaa-bbb)/aaa, 1:2, quantile, c(0.025))


plot(plotyears-0.1, ptest[1,], pch=15, 
     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), ylim=c(0,1), 
     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
     xlab="Year", ylab="PIA mean and CI",
     main="PIA by ethnicity and year"
)
errbar(plotyears-0.1, ub[1,], lb[1,])
points(plotyears, ptest[2,], pch=16)
errbar(plotyears, ub[2,], lb[2,])
points(plotyears+0.1, ptest[3,], pch=17)
errbar(plotyears+0.1, ub[3,], lb[3,])

## race-specific year-specific NIAs      

aaa <- array(sapply(1:100, function(x) apply(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,3:12], c(1,3), sum)), dim=c(3,10,100))
bbb <- array(sapply(1:100, function(x) apply(a10_gc_obs_100[[x]]$n_inc_insch_f[,,3:12], c(1,3), sum)), dim=c(3,10,100))

ptest <- (asum(fns[,,3:12], c(1,3)) - asum(fcs[,,3:12], c(1,3)))
ub <- apply((aaa-bbb), 1:2, quantile, c(0.975))
lb <- apply((aaa-bbb), 1:2, quantile, c(0.025))

plotyears <- 2008:2017
errbar <- function(x, up, low) arrows(x, low, x, up, length=0.05, angle=90, code=3)

plot(plotyears-0.1, ptest[1,], pch=15, 
     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), ylim=c(min(lb), max(ub)), 
     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
     xlab="Year", ylab="PIA mean and CI",
     main="PIA by ethnicity and year"
)
errbar(plotyears-0.1, ub[1,], lb[1,])
points(plotyears, ptest[2,], pch=16)
errbar(plotyears, ub[2,], lb[2,])
points(plotyears+0.1, ptest[3,], pch=17)
errbar(plotyears+0.1, ub[3,], lb[3,])
abline(h=0)

}