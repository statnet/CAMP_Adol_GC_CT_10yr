
#############################################################
#### Results for paper "XXXX"

load("../output/a10_ct_nbc.rda")
load("../output/a10_ct_obs.rda")
load("../output/a10_gc_nbc.rda")
load("../output/a10_gc_obs.rda")

#################################################################################
#### Table 1: regression coefficients

round(summary(eversex_f_reg)$coef[,c(1,2,4)],3)
round(summary(eversex_m_reg)$coef[,c(1,2,4)],3)
round(summary(condom_f_reg)$coef[,c(1,2,4)],3)
round(summary(condom_m_reg)$coef[,c(1,2,4)],3)
round(summary(mnppy_f_reg)$coef[,c(1,2,4)],3)
round(summary(mnppy_m_reg)$coef[,c(1,2,4)],3)



#################################################################################
#### Figure 1: predicted values for eversex
surveyyears <- seq(2007, 2017, by=2)

bmp("../output/predicted.bmp", width=920, height=700)

layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))

par(mar=c(0, 0, 0, 0))
plot(0:1, 0:1, xaxt = 'n', yaxt = 'n', bty = 'n', 
     pch = '', ylab = '', xlab = '')
legend(x=0.18, y=0.15, 
   legend=c('Age 13 reported', 'Age 14 reported', 'Age 15 reported',
            'Age 16 reported', 'Age 17 reported', 'Age 18 reported',
            'Age 13 predicted', 'Age 14 predicted', 'Age 15 predicted',
            'Age 16 predicted', 'Age 17 predicted', 'Age 18 predicted'),
   lty=rep(1:2,each=6), col=rep(rainbow(6),2), ncol=4, cex=1.5)

par(mar=c(5.1, 5.1, 2.1, 2.1))
matplot(surveyyears, t(prop_eversex_f[1,,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(6),
        xlab= "Survey year", 
        ylab="Prop. reporting previous sexual intercourse",
        main = "Black females", cex.axis=1.75, cex.lab = 1.75, cex.main=1.75)
matplot(surveyyears, t(pred_eversex_f_dyn[1,,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(6))

par(mar=c(5.1, 1.5, 2.1, 2.1))
matplot(surveyyears, t(prop_eversex_f[2,,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(6),
        xlab= "Survey year", 
        ylab="",
        main = "Hispanic females", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_eversex_f_dyn[2,,c(1,3,5,7,9,11)]), 
          pch=16, type='b', add=T, lty=2, col=rainbow(6))
  
matplot(surveyyears, t(prop_eversex_f[3,,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(6),
        xlab= "Survey year", 
        ylab="",
        main = "White females", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_eversex_f_dyn[3,,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(6))

dev.off()


#################################################################################
### Short names for ease

fns_ct <- a10_ct_nbc$n_inc_insch_f  # Female no behavior change, school
fcs_ct <- a10_ct_obs$n_inc_insch_f    # etc.
fnt_ct <- a10_ct_nbc$n_inc_total_f
fct_ct <- a10_ct_obs$n_inc_total_f
mns_ct <- a10_ct_nbc$n_inc_insch_m
mcs_ct <- a10_ct_obs$n_inc_insch_m
mnt_ct <- a10_ct_nbc$n_inc_total_m
mct_ct <- a10_ct_obs$n_inc_total_m

fns_gc <- a10_gc_nbc$n_inc_insch_f  # Female no behavior change, school
fcs_gc <- a10_gc_obs$n_inc_insch_f    # etc.
fnt_gc <- a10_gc_nbc$n_inc_total_f
fct_gc <- a10_gc_obs$n_inc_total_f
mns_gc <- a10_gc_nbc$n_inc_insch_m
mcs_gc <- a10_gc_obs$n_inc_insch_m
mnt_gc <- a10_gc_nbc$n_inc_total_m
mct_gc <- a10_gc_obs$n_inc_total_m

plotyears <- 2008:2017

#################################################################################
## Helper functions

asum <- function(x, y) apply(x, y, sum)
sum_nbc_f_ct <- function(x, dim) asum(a10_ct_nbc_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_nbc_m_ct <- function(x, dim) asum(a10_ct_nbc_100[[x]]$n_inc_insch_m[,,3:12], dim)
sum_obs_f_ct <- function(x, dim) asum(a10_ct_obs_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_obs_m_ct <- function(x, dim) asum(a10_ct_obs_100[[x]]$n_inc_insch_m[,,3:12], dim)
sum_nbc_f_gc <- function(x, dim) asum(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_nbc_m_gc <- function(x, dim) asum(a10_gc_nbc_100[[x]]$n_inc_insch_m[,,3:12], dim)
sum_obs_f_gc <- function(x, dim) asum(a10_gc_obs_100[[x]]$n_inc_insch_f[,,3:12], dim)
sum_obs_m_gc <- function(x, dim) asum(a10_gc_obs_100[[x]]$n_inc_insch_m[,,3:12], dim)

#array(sapply(1:100, function(x) sum_nbc_f(x,c(1,3))), c(3,10,100))
errbar <- function(x, up, low, ...) arrows(x, low, x, up, 
                                      length=0.00, angle=90, code=3, ...)



##############################################################
## Text: Total NIAs with CIs

# Have to do colSums because helper fxn can't sum across all dimesions at once
temp_nbc_ct <- colSums(sapply(1:100, function(x) sum_nbc_f_ct(x,1) + sum_nbc_m_ct(x,1))) 
temp_obs_ct <- colSums(sapply(1:100, function(x) sum_obs_f_ct(x,1) + sum_obs_m_ct(x,1))) 
nia_ct_tot_pt <- sum(fns_ct[,,3:12]) - sum(fcs_ct[,,3:12]) + 
                 sum(mns_ct[,,3:12]) - sum(mcs_ct[,,3:12])
nia_ct_tot_lb <- quantile(temp_nbc_ct - temp_obs_ct, 0.025)
nia_ct_tot_ub <- quantile(temp_nbc_ct - temp_obs_ct, 0.975)
round(c(nia_ct_tot_pt, nia_ct_tot_lb, nia_ct_tot_ub),1)

temp_nbc_gc <- colSums(sapply(1:100, function(x) sum_nbc_f_gc(x,1) + sum_nbc_m_gc(x,1))) 
temp_obs_gc <- colSums(sapply(1:100, function(x) sum_obs_f_gc(x,1) + sum_obs_m_gc(x,1))) 
nia_gc_tot_pt <- sum(fns_gc[,,3:12]) - sum(fcs_gc[,,3:12]) + 
  sum(mns_gc[,,3:12]) - sum(mcs_gc[,,3:12])
nia_gc_tot_lb <- quantile(temp_nbc_gc - temp_obs_gc, 0.025)
nia_gc_tot_ub <- quantile(temp_nbc_gc - temp_obs_gc, 0.975)
round(c(nia_gc_tot_pt, nia_gc_tot_lb, nia_gc_tot_ub),1)


##############################################################
## Text: Total PIAs with CIs

temp_nbc_ct <- colSums(sapply(1:100, function(x) sum_nbc_f_ct(x,1) + sum_nbc_m_ct(x,1))) 
temp_obs_ct <- colSums(sapply(1:100, function(x) sum_obs_f_ct(x,1) + sum_obs_m_ct(x,1))) 
pia_ct_tot_pt <- (sum(fns_ct[,,3:12]) - sum(fcs_ct[,,3:12]) + 
                    sum(mns_ct[,,3:12]) - sum(mcs_ct[,,3:12])) / 
  (sum(fns_ct[,,3:12]) + sum(mns_ct[,,3:12]))
pia_ct_tot_lb <- quantile((temp_nbc_ct-temp_obs_ct)/temp_nbc_ct, 0.025)
pia_ct_tot_ub <- quantile((temp_nbc_ct-temp_obs_ct)/temp_nbc_ct, 0.975)
round(100*c(pia_ct_tot_pt, pia_ct_tot_lb, pia_ct_tot_ub),1)

temp_nbc_gc <- colSums(sapply(1:100, function(x) sum_nbc_f_gc(x,1) + sum_nbc_m_gc(x,1))) 
temp_obs_gc <- colSums(sapply(1:100, function(x) sum_obs_f_gc(x,1) + sum_obs_m_gc(x,1))) 
pia_gc_tot_pt <- (sum(fns_gc[,,3:12]) - sum(fcs_gc[,,3:12]) + 
                    sum(mns_gc[,,3:12]) - sum(mcs_gc[,,3:12])) / 
  (sum(fns_gc[,,3:12]) + sum(mns_gc[,,3:12]))
pia_gc_tot_lb <- quantile((temp_nbc_gc-temp_obs_gc)/temp_nbc_gc, 0.025)
pia_gc_tot_ub <- quantile((temp_nbc_gc-temp_obs_gc)/temp_nbc_gc, 0.975)
round(100*c(pia_gc_tot_pt, pia_gc_tot_lb, pia_gc_tot_ub),1)


##############################################################
## Table 2 and Table S5: Total NIAs by year and sex

temp_nbc_ct <- sapply(1:100, function(x) sum_nbc_f_ct(x,3)) 
temp_obs_ct <- sapply(1:100, function(x) sum_obs_f_ct(x,3)) 
nia_ct_f_year_pt <- (asum(fns_ct[,,3:12],3) - asum(fcs_ct[,,3:12],3))
nia_ct_f_year_lb <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.025))
nia_ct_f_year_ub <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.975))
table2a <- round(cbind(nia_ct_f_year_pt, nia_ct_f_year_lb, nia_ct_f_year_ub),1)
table2a

temp_nbc_ct <- sapply(1:100, function(x) sum_nbc_m_ct(x,3)) 
temp_obs_ct <- sapply(1:100, function(x) sum_obs_m_ct(x,3)) 
nia_ct_m_year_pt <- (asum(mns_ct[,,3:12],3) - asum(mcs_ct[,,3:12],3))
nia_ct_m_year_lb <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.025))
nia_ct_m_year_ub <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.975))
table2b <- round(cbind(nia_ct_m_year_pt, nia_ct_m_year_lb, nia_ct_m_year_ub),1)
table2b

temp_nbc_gc <- sapply(1:100, function(x) sum_nbc_f_gc(x,3)) 
temp_obs_gc <- sapply(1:100, function(x) sum_obs_f_gc(x,3)) 
nia_gc_f_year_pt <- (asum(fns_gc[,,3:12],3) - asum(fcs_gc[,,3:12],3))
nia_gc_f_year_lb <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.025))
nia_gc_f_year_ub <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.975))
table2c <- round(cbind(nia_gc_f_year_pt, nia_gc_f_year_lb, nia_gc_f_year_ub),1)
table2c

temp_nbc_gc <- sapply(1:100, function(x) sum_nbc_m_gc(x,3)) 
temp_obs_gc <- sapply(1:100, function(x) sum_obs_m_gc(x,3)) 
nia_gc_m_year_pt <- (asum(mns_gc[,,3:12],3) - asum(mcs_gc[,,3:12],3))
nia_gc_m_year_lb <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.025))
nia_gc_m_year_ub <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.975))
table2d <- round(cbind(nia_gc_m_year_pt, nia_gc_m_year_lb, nia_gc_m_year_ub),1)
table2d

(annual_savings_ct_f <- table2a * costs$CT_F)
(annual_savings_ct_m <- table2b * costs$CT_M)
(annual_savings_gc_f <- table2c * costs$GC_F)
(annual_savings_gc_m <- table2d * costs$GC_M)
colSums(annual_savings_ct_f) # Double check Excel (rounding error OK)
colSums(annual_savings_ct_m) # Double check Excel (rounding error OK)
colSums(annual_savings_gc_f) # Double check Excel (rounding error OK)
colSums(annual_savings_gc_m) # Double check Excel (rounding error OK)



#########################################
## Figure 2: race-specific year-specific NIAs and PIAs      

temp_nbc_ct <- array(sapply(1:100, function(x) 
  apply(a10_ct_nbc_100[[x]]$n_inc_insch_f[,,3:12], c(1,3), sum) + 
  apply(a10_ct_nbc_100[[x]]$n_inc_insch_m[,,3:12], c(1,3), sum)), 
  dim=c(3,10,100))

temp_obs_ct <- array(sapply(1:100, function(x) 
  apply(a10_ct_obs_100[[x]]$n_inc_insch_f[,,3:12], c(1,3), sum) + 
  apply(a10_ct_obs_100[[x]]$n_inc_insch_m[,,3:12], c(1,3), sum)), 
  dim=c(3,10,100))

temp_nbc_gc <- array(sapply(1:100, function(x) 
  apply(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,3:12], c(1,3), sum) + 
  apply(a10_gc_nbc_100[[x]]$n_inc_insch_m[,,3:12], c(1,3), sum)), 
  dim=c(3,10,100))

temp_obs_gc <- array(sapply(1:100, function(x) 
  apply(a10_gc_obs_100[[x]]$n_inc_insch_f[,,3:12], c(1,3), sum) + 
  apply(a10_gc_obs_100[[x]]$n_inc_insch_m[,,3:12], c(1,3), sum)), 
  dim=c(3,10,100))

nia_ct_raceyr_pt <- 
  (asum(fns_ct[,,3:12], c(1,3)) - asum(fcs_ct[,,3:12], c(1,3)) +
     asum(mns_ct[,,3:12], c(1,3)) - asum(mcs_ct[,,3:12], c(1,3)))
pia_ct_raceyr_pt <- nia_ct_raceyr_pt/
  (asum(fns_ct[,,3:12], c(1,3)) + asum(mns_ct[,,3:12], c(1,3)))

nia_gc_raceyr_pt <- 
  (asum(fns_gc[,,3:12], c(1,3)) - asum(fcs_gc[,,3:12], c(1,3)) +
     asum(mns_gc[,,3:12], c(1,3)) - asum(mcs_gc[,,3:12], c(1,3)))
pia_gc_raceyr_pt <- nia_gc_raceyr_pt/
  (asum(fns_gc[,,3:12], c(1,3)) + asum(mns_gc[,,3:12], c(1,3)))

nia_ct_raceyr_ub <- apply((temp_nbc_ct-temp_obs_ct), 
                          1:2, quantile, c(0.975))
nia_ct_raceyr_lb <- apply((temp_nbc_ct-temp_obs_ct), 
                          1:2, quantile, c(0.025))
nia_gc_raceyr_ub <- apply((temp_nbc_gc-temp_obs_gc), 
                          1:2, quantile, c(0.975))
nia_gc_raceyr_lb <- apply((temp_nbc_gc-temp_obs_gc), 
                          1:2, quantile, c(0.025))
pia_ct_raceyr_ub <- apply((temp_nbc_ct-temp_obs_ct)/temp_nbc_ct, 
                          1:2, quantile, c(0.975))
pia_ct_raceyr_lb <- apply((temp_nbc_ct-temp_obs_ct)/temp_nbc_ct, 
                          1:2, quantile, c(0.025))
pia_gc_raceyr_ub <- apply((temp_nbc_gc-temp_obs_gc)/temp_nbc_gc, 
                          1:2, quantile, c(0.975))
pia_gc_raceyr_lb <- apply((temp_nbc_gc-temp_obs_gc)/temp_nbc_gc, 
                          1:2, quantile, c(0.025))

bmp("../output/a10_race.bmp", width=800, height=800)
plotoffset = 0.07
par(mfrow=c(2,1))
plot(plotyears-plotoffset*3, nia_ct_raceyr_pt[1,], pch=16, col='red',
     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), ylim=c(0,3e5), 
     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
     yaxt = 'n',
     xlab="Year", ylab="NIA (mean and CI)",
     main="NIA by race/ethnicity and year"
)
axis(2, at = seq(0,3e5,5e4), las=1,
     labels=c('0k', '50k', '100k', '150k', '200k', '250k', '300k'))
errbar(plotyears-plotoffset*3, nia_ct_raceyr_ub[1,], nia_ct_raceyr_lb[1,], col='red')
points(plotyears-plotoffset*2, nia_ct_raceyr_pt[2,], pch=16, col='blue')
errbar(plotyears-plotoffset*2, nia_ct_raceyr_ub[2,], nia_ct_raceyr_lb[2,], col='blue')
points(plotyears-plotoffset*1, nia_ct_raceyr_pt[3,], pch=16, col='green')
errbar(plotyears-plotoffset*1, nia_ct_raceyr_ub[3,], nia_ct_raceyr_lb[3,], col='green')
points(plotyears+plotoffset*1, nia_gc_raceyr_pt[1,], pch=4, col='red')
errbar(plotyears+plotoffset*1, nia_gc_raceyr_ub[1,], nia_gc_raceyr_lb[1,], col='red')
points(plotyears+plotoffset*2, nia_gc_raceyr_pt[2,], pch=4, col='blue')
errbar(plotyears+plotoffset*2, nia_gc_raceyr_ub[2,], nia_gc_raceyr_lb[2,], col='blue')
points(plotyears+plotoffset*3, nia_gc_raceyr_pt[3,], pch=4, col='green')
errbar(plotyears+plotoffset*3, nia_gc_raceyr_ub[3,], nia_gc_raceyr_lb[3,], col='green')
legend(2008, 3e5, 
       legend=c('Black, chlamydia', 'Hispanic, chlamydia', 'White, chlamdyia',
                'Black, gonorrhea', 'Hispanic, gonorrhea', 'White, gonorrhea'),
       pch = rep(c(16,4), each=3),
       col = rep(c('red', 'blue', 'green'),2),
       ncol=2)

plot(plotyears-plotoffset*3, pia_ct_raceyr_pt[1,], pch=16, col='red',
     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), ylim=c(0,0.8), 
     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
     xlab="Year", ylab="PIA (mean and CI)",
     main="PIA by race/ethnicity and year"
)
errbar(plotyears-plotoffset*3, pia_ct_raceyr_ub[1,], pia_ct_raceyr_lb[1,], col='red')
points(plotyears-plotoffset*2, pia_ct_raceyr_pt[2,], pch=16, col='blue')
errbar(plotyears-plotoffset*2, pia_ct_raceyr_ub[2,], pia_ct_raceyr_lb[2,], col='blue')
points(plotyears-plotoffset*1, pia_ct_raceyr_pt[3,], pch=16, col='green')
errbar(plotyears-plotoffset*1, pia_ct_raceyr_ub[3,], pia_ct_raceyr_lb[3,], col='green')
points(plotyears+plotoffset*1, pia_gc_raceyr_pt[1,], pch=4, col='red')
errbar(plotyears+plotoffset*1, pia_gc_raceyr_ub[1,], pia_gc_raceyr_lb[1,], col='red')
points(plotyears+plotoffset*2, pia_gc_raceyr_pt[2,], pch=4, col='blue')
errbar(plotyears+plotoffset*2, pia_gc_raceyr_ub[2,], pia_gc_raceyr_lb[2,], col='blue')
points(plotyears+plotoffset*3, pia_gc_raceyr_pt[3,], pch=4, col='green')
errbar(plotyears+plotoffset*3, pia_gc_raceyr_ub[3,], pia_gc_raceyr_lb[3,], col='green')
legend(2008, 0.8, 
       legend=c('Black, chlamydia', 'Hispanic, chlamydia', 'White, chlamdyia',
                         'Black, gonorrhea', 'Hispanic, gonorrhea', 'White, gonorrhea'),
       pch = rep(c(16,4), each=3),
       col = rep(c('red', 'blue', 'green'),2),
       ncol=2)
dev.off()

#########################################
# Text: numbers summarizing Figure 2

rowSums(nia_ct_raceyr_pt)
prop.table(rowSums(nia_ct_raceyr_pt))
rowSums(nia_gc_raceyr_pt)
prop.table(rowSums(nia_gc_raceyr_pt))
rowSums(nia_ct_raceyr_pt + nia_gc_raceyr_pt)
prop.table(rowSums(nia_ct_raceyr_pt + nia_gc_raceyr_pt))

### For PAA
nia_ct_race_pt <- 
  rowSums((asum(fns_ct[,,3:12], c(1,3)) - asum(fcs_ct[,,3:12], c(1,3)) +
             asum(mns_ct[,,3:12], c(1,3)) - asum(mcs_ct[,,3:12], c(1,3))))

### For PAA
nia_gc_race_pt <- 
  rowSums((asum(fns_gc[,,3:12], c(1,3)) - asum(fcs_gc[,,3:12], c(1,3)) +
             asum(mns_gc[,,3:12], c(1,3)) - asum(mcs_gc[,,3:12], c(1,3))))

pia_ct_race_pt <- 
  rowSums((asum(fns_ct[,,3:12], c(1,3)) - asum(fcs_ct[,,3:12], c(1,3)) +
     asum(mns_ct[,,3:12], c(1,3)) - asum(mcs_ct[,,3:12], c(1,3)))) / 
  rowSums((asum(fns_ct[,,3:12], c(1,3)) + asum(mns_ct[,,3:12], c(1,3))))

pia_gc_race_pt <- 
  rowSums((asum(fns_gc[,,3:12], c(1,3)) - asum(fcs_gc[,,3:12], c(1,3)) +
             asum(mns_gc[,,3:12], c(1,3)) - asum(mcs_gc[,,3:12], c(1,3)))) / 
  rowSums((asum(fns_gc[,,3:12], c(1,3)) + asum(mns_gc[,,3:12], c(1,3))))

pia_gcct_race_pt <- 
  (rowSums((asum(fns_ct[,,3:12], c(1,3)) - asum(fcs_ct[,,3:12], c(1,3)) +
             asum(mns_ct[,,3:12], c(1,3)) - asum(mcs_ct[,,3:12], c(1,3)))) +
  rowSums((asum(fns_gc[,,3:12], c(1,3)) - asum(fcs_gc[,,3:12], c(1,3)) +
             asum(mns_gc[,,3:12], c(1,3)) - asum(mcs_gc[,,3:12], c(1,3)))) ) /
    
  (rowSums((asum(fns_ct[,,3:12], c(1,3)) + asum(mns_ct[,,3:12], c(1,3)))) +
  rowSums((asum(fns_gc[,,3:12], c(1,3)) + asum(mns_gc[,,3:12], c(1,3)))) )



#########################################
## Figure 3: age-specific NIAs and PIAs      

temp_nbc_ct <- array(sapply(1:100, function(x) 
  apply(a10_ct_nbc_100[[x]]$n_inc_insch_f[,,3:12], c(2), sum) + 
    apply(a10_ct_nbc_100[[x]]$n_inc_insch_m[,,3:12], c(2), sum)), 
  dim=c(6,1,100))

temp_obs_ct <- array(sapply(1:100, function(x) 
  apply(a10_ct_obs_100[[x]]$n_inc_insch_f[,,3:12], c(2), sum) + 
    apply(a10_ct_obs_100[[x]]$n_inc_insch_m[,,3:12], c(2), sum)), 
  dim=c(6,1,100))

temp_nbc_gc <- array(sapply(1:100, function(x) 
  apply(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,3:12], c(2), sum) + 
    apply(a10_gc_nbc_100[[x]]$n_inc_insch_m[,,3:12], c(2), sum)), 
  dim=c(6,1,100))

temp_obs_gc <- array(sapply(1:100, function(x) 
  apply(a10_gc_obs_100[[x]]$n_inc_insch_f[,,3:12], c(2), sum) + 
    apply(a10_gc_obs_100[[x]]$n_inc_insch_m[,,3:12], c(2), sum)), 
  dim=c(6,1,100))

nia_ct_age_pt <- 
  (asum(fns_ct[,,3:12], c(2)) - asum(fcs_ct[,,3:12], c(2)) +
     asum(mns_ct[,,3:12], c(2)) - asum(mcs_ct[,,3:12], c(2)))
pia_ct_age_pt <- nia_ct_age_pt/
  (asum(fns_ct[,,3:12], c(2)) + asum(mns_ct[,,3:12], c(2)))

nia_gc_age_pt <- 
  (asum(fns_gc[,,3:12], c(2)) - asum(fcs_gc[,,3:12], c(2)) +
     asum(mns_gc[,,3:12], c(2)) - asum(mcs_gc[,,3:12], c(2)))
pia_gc_age_pt <- nia_gc_age_pt/
  (asum(fns_gc[,,3:12], c(2)) + asum(mns_gc[,,3:12], c(2)))

nia_ct_age_ub <- apply((temp_nbc_ct-temp_obs_ct), 
                          1:2, quantile, c(0.975))
nia_ct_age_lb <- apply((temp_nbc_ct-temp_obs_ct), 
                          1:2, quantile, c(0.025))
nia_gc_age_ub <- apply((temp_nbc_gc-temp_obs_gc), 
                          1:2, quantile, c(0.975))
nia_gc_age_lb <- apply((temp_nbc_gc-temp_obs_gc), 
                          1:2, quantile, c(0.025))
pia_ct_age_ub <- apply((temp_nbc_ct-temp_obs_ct)/temp_nbc_ct, 
                          1:2, quantile, c(0.975))
pia_ct_age_lb <- apply((temp_nbc_ct-temp_obs_ct)/temp_nbc_ct, 
                          1:2, quantile, c(0.025))
pia_gc_age_ub <- apply((temp_nbc_gc-temp_obs_gc)/temp_nbc_gc, 
                          1:2, quantile, c(0.975))
pia_gc_age_lb <- apply((temp_nbc_gc-temp_obs_gc)/temp_nbc_gc, 
                          1:2, quantile, c(0.025))

bmp("../output/a10_age.bmp", width=500, height=800)
plotoffset = 0.07
par(mfrow=c(2,1))
plot((13:18)-plotoffset, nia_ct_age_pt, pch=16, col='red',
     ylim=c(0,8e5), 
     yaxt = 'n',
     xlab="Age", ylab="NIA (mean and CI)",
     main="NIA by age"
)
axis(2, at = seq(0,8e5,1e5), las=1,
     labels=c('0k', '100k', '200k', '300k', '400k', '500k', '600k', '700k', '800k'))
errbar((13:18)-plotoffset, nia_ct_age_ub, nia_ct_age_lb, col='red')
points((13:18)+plotoffset, nia_gc_age_pt, pch=4, col='blue')
errbar((13:18)+plotoffset, nia_gc_age_ub, nia_gc_age_lb, col='blue')

legend(13, 8e5, 
       legend=c('Chlamydia', 'Gonorrhea'),
       pch = c(16,4),
       col = c('red', 'blue')
)

plot((13:18)-plotoffset, pia_ct_age_pt, pch=16, col='red',
     ylim=c(0, 0.5),
     xlab="Age", ylab="PIA (mean and CI)",
     main="PIA by age"
)
errbar((13:18)-plotoffset, pia_ct_age_ub, pia_ct_age_lb, col='red')
points((13:18)+plotoffset, pia_gc_age_pt, pch=4, col='blue')
errbar((13:18)+plotoffset, pia_gc_age_ub, pia_gc_age_lb, col='blue')

legend(17, 0.5, 
       legend=c('Chlamydia', 'Gonorrhea'),
       pch = c(16,4),
       col = c('red', 'blue')
)

dev.off()



#########################################
# Text: numbers summarizing Figure 3

nia_ct_age_pt
prop.table(nia_ct_age_pt)
nia_gc_age_pt
prop.table(nia_gc_age_pt)
nia_ct_age_pt + nia_gc_age_pt
prop.table(nia_ct_age_pt + nia_gc_age_pt)

pia_ct_age_pt <- 
  (asum(fns_ct[,,3:12], c(2)) - asum(fcs_ct[,,3:12], c(2)) +
             asum(mns_ct[,,3:12], c(2)) - asum(mcs_ct[,,3:12], c(2))) / 
  (asum(fns_ct[,,3:12], c(2)) + asum(mns_ct[,,3:12], c(2)))

pia_gc_age_pt <- 
  (asum(fns_gc[,,3:12], c(2)) - asum(fcs_gc[,,3:12], c(2)) +
             asum(mns_gc[,,3:12], c(2)) - asum(mcs_gc[,,3:12], c(2))) / 
  (asum(fns_gc[,,3:12], c(2)) + asum(mns_gc[,,3:12], c(2)))

pia_gcct_age_pt <- 
  ((asum(fns_ct[,,3:12], c(2)) - asum(fcs_ct[,,3:12], c(2)) +
              asum(mns_ct[,,3:12], c(2)) - asum(mcs_ct[,,3:12], c(2))) +
     (asum(fns_gc[,,3:12], c(2)) - asum(fcs_gc[,,3:12], c(2)) +
                asum(mns_gc[,,3:12], c(2)) - asum(mcs_gc[,,3:12], c(2))) ) /
  
  ((asum(fns_ct[,,3:12], c(2)) + asum(mns_ct[,,3:12], c(2))) +
     (asum(fns_gc[,,3:12], c(2)) + asum(mns_gc[,,3:12], c(2))) )


pia_ct_age_pt
pia_gc_age_pt
pia_gcct_age_pt


#####################################
#  Boneyard

#bmp("../output/costs.gc.bmp")
#plot(plotyears, annual_savings_gc[,1]/1e6, pch=15, 
#     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), 
#     ylim=c(min(annual_savings_gc/1e6), max(annual_savings_gc/1e6)), 
#     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
#     xlab="Year", ylab="Costs saved (US $mil)",
#     main="Costs saved, Gonorrhea"
#)
#errbar(plotyears, annual_savings_gc[,2]/1e6, annual_savings_gc[,3]/1e6)
#
#plot(plotyears, annual_savings_ct[,1]/1e6, pch=15, 
#     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), 
#     ylim=c(min(annual_savings_ct/1e6), max(annual_savings_ct/1e6)), 
#     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
#     xlab="Year", ylab="Costs saved (US $mil)",
#     main="Costs saved, Chlamydia"
#)
#errbar(plotyears, annual_savings_ct[,2]/1e6, annual_savings_ct[,3]/1e6)
#
#dev.off()


