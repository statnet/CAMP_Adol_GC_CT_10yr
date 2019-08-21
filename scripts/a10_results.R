
#############################################################
#### Results for paper "XXXX"

#### Table 1: regression coefficients

round(summary(eversex_f_reg)$coef[,c(1,2,4)],3)
round(summary(eversex_m_reg)$coef[,c(1,2,4)],3)
round(summary(mnppy_f_reg)$coef[,c(1,2,4)],3)
round(summary(mnppy_m_reg)$coef[,c(1,2,4)],3)
round(summary(condom_f_reg)$coef[,c(1,2,4)],3)
round(summary(condom_m_reg)$coef[,c(1,2,4)],3)


############################################################
### Short names for ease

fns <- a10_gc_nbc$n_inc_insch_f  # Female no behavior change, school
fcs <- a10_gc_obs$n_inc_insch_f    # etc.
fnt <- a10_gc_nbc$n_inc_total_f
fct <- a10_gc_obs$n_inc_total_f
mns <- a10_gc_nbc$n_inc_insch_m
mcs <- a10_gc_obs$n_inc_insch_m
mnt <- a10_gc_nbc$n_inc_total_m
mct <- a10_gc_obs$n_inc_total_m


############################################################
## Helper functions
asum <- function(x, y) apply(x, y, sum)

sum_nbc_f <- function(x, dim) asum(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_nbc_m <- function(x, dim) asum(a10_gc_nbc_100[[x]]$n_inc_insch_m[,,3:12], dim)
sum_obs_f <- function(x, dim) asum(a10_gc_obs_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_obs_m <- function(x, dim) asum(a10_gc_obs_100[[x]]$n_inc_insch_m[,,3:12], dim)

#array(sapply(1:100, function(x) sum_nbc_f(x,c(1,3))), c(3,10,100))

##############################################################
## Total NIAs with CIs

# Have to do colSums because helper fxn can't sum across all dimesions at once
temp_nbc <- colSums(sapply(1:100, function(x) sum_nbc_f(x,1) + sum_nbc_m(x,1))) 
temp_obs <- colSums(sapply(1:100, function(x) sum_obs_f(x,1) + sum_obs_m(x,1))) 
nia_gc_tot_pt <- sum(fns[,,3:12]) - sum(fcs[,,3:12]) + sum(mns[,,3:12]) - sum(mcs[,,3:12])
nia_gc_tot_lb <- quantile(temp_nbc - temp_obs, 0.025)
nia_gc_tot_ub <- quantile(temp_nbc - temp_obs, 0.975)
c(nia_gc_tot_pt, nia_gc_tot_lb, nia_gc_tot_ub)

##############################################################
## Total PIAs with CIs

temp_nbc <- colSums(sapply(1:100, function(x) sum_nbc_f(x,1) + sum_nbc_m(x,1))) 
temp_obs <- colSums(sapply(1:100, function(x) sum_obs_f(x,1) + sum_obs_m(x,1))) 
pia_gc_tot_pt <- (sum(fns[,,3:12]) - sum(fcs[,,3:12]) + sum(mns[,,3:12]) - sum(mcs[,,3:12])) / 
                   (sum(fns[,,3:12]) + sum(mns[,,3:12]))
pia_gc_tot_lb <- quantile((temp_nbc-temp_obs)/temp_nbc, 0.025)
pia_gc_tot_ub <- quantile((temp_nbc-temp_obs)/temp_nbc, 0.975)
round(100*c(pia_gc_tot_pt, pia_gc_tot_lb, pia_gc_tot_ub),1)


##############################################################
## Total NIAs by year and sex

temp_nbc <- sapply(1:100, function(x) sum_nbc_f(x,3)) 
temp_obs <- sapply(1:100, function(x) sum_obs_f(x,3)) 
nia_gc_f_year_pt <- (asum(fns[,,3:12],3) - asum(fcs[,,3:12],3))
nia_gc_f_year_lb <- apply((temp_nbc-temp_obs), 1, quantile, c(0.025))
nia_gc_f_year_ub <- apply((temp_nbc-temp_obs), 1, quantile, c(0.975))
table2a <- round(cbind(nia_gc_f_year_pt, nia_gc_f_year_lb, nia_gc_f_year_ub),1)
matplot(table2a, type='l')

temp_nbc <- sapply(1:100, function(x) sum_nbc_m(x,3)) 
temp_obs <- sapply(1:100, function(x) sum_obs_m(x,3)) 
nia_gc_m_year_pt <- (asum(fns[,,3:12],3) - asum(fcs[,,3:12],3))
nia_gc_m_year_lb <- apply((temp_nbc-temp_obs), 1, quantile, c(0.025))
nia_gc_m_year_ub <- apply((temp_nbc-temp_obs), 1, quantile, c(0.975))
table2b <- round(cbind(nia_gc_m_year_pt, nia_gc_m_year_lb, nia_gc_m_year_ub),1)
matplot(table2b, type='l')
































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

plotyears <- 2008:2017
errbar <- function(x, up, low) arrows(x, low, x, up, length=0.05, angle=90, code=3)

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

