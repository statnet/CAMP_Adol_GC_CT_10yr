
#############################################################
#### Results for paper "XXXX"

load("../output/a10_ct_nbc.rda")
load("../output/a10_ct_obs.rda")
load("../output/a10_gc_nbc.rda")
load("../output/a10_gc_obs.rda")

#################################################################################
#### Table Sx: regression coefficients

round(summary(eversex_f_reg)$coef[,c(1,2,4)],3)
round(summary(eversex_m_reg)$coef[,c(1,2,4)],3)
round(summary(condom_f_reg)$coef[,c(1,2,4)],3)
round(summary(condom_m_reg)$coef[,c(1,2,4)],3)
round(summary(mnppy_f_reg)$coef[,c(1,2,4)],3)
round(summary(mnppy_m_reg)$coef[,c(1,2,4)],3)

#################################################################################
#### Figure 1: predicted values for eversex
surveyyears <- seq(2007, 2017, by=2)

tiff("../output/predicted_eversex_f.tif", width=9*300, height=7*300,
     units = "px", res = 300, pointsize = 8,  compression = "lzw")

layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))

par(mar=c(0, 0, 0, 0))
plot(0:1, 0:1, xaxt = 'n', yaxt = 'n', bty = 'n', 
     pch = '', ylab = '', xlab = '')
legend(x=0.18, y=0.15, 
   legend=c('Age 14 reported', 'Age 15 reported', 'Age 16 reported', 
            'Age 17 reported', 'Age 18 reported', '',
            'Age 14 predicted', 'Age 15 predicted', 'Age 16 predicted', 
            'Age 17 predicted', 'Age 18 predicted', ''),
   lty=rep(1:2,each=6), col=rep(c(rainbow(5),"white"),2), ncol=4, cex=1.5)

par(mar=c(5.1, 5.1, 2.1, 2.1))
matplot(surveyyears, t(prop_eversex_f[1,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="Prop. reporting previous sexual intercourse",
        main = "Black female students", cex.axis=1.75, cex.lab = 1.75, cex.main=1.75)
matplot(surveyyears, t(pred_eversex_f_dyn[1,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

par(mar=c(5.1, 1.5, 2.1, 2.1))
matplot(surveyyears, t(prop_eversex_f[2,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="",
        main = "Hispanic female students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_eversex_f_dyn[2,2:6,c(1,3,5,7,9,11)]), 
          pch=16, type='b', add=T, lty=2, col=rainbow(5))
  
matplot(surveyyears, t(prop_eversex_f[3,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="",
        main = "White female students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_eversex_f_dyn[3,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

dev.off()

#################################################################################
### Short names for ease

fns_ct <- a10_ct_nbc$n_inc_insch_f[,2:6,]  # Female no behavior change, school # Removing 13-year-olds
fcs_ct <- a10_ct_obs$n_inc_insch_f[,2:6,]    # etc.
fnt_ct <- a10_ct_nbc$n_inc_total_f[,2:6,]
fct_ct <- a10_ct_obs$n_inc_total_f[,2:6,]
mns_ct <- a10_ct_nbc$n_inc_insch_m[,2:6,]
mcs_ct <- a10_ct_obs$n_inc_insch_m[,2:6,]
mnt_ct <- a10_ct_nbc$n_inc_total_m[,2:6,]
mct_ct <- a10_ct_obs$n_inc_total_m[,2:6,]

fns_gc <- a10_gc_nbc$n_inc_insch_f[,2:6,]  # Female no behavior change, school
fcs_gc <- a10_gc_obs$n_inc_insch_f[,2:6,]    # etc.
fnt_gc <- a10_gc_nbc$n_inc_total_f[,2:6,]
fct_gc <- a10_gc_obs$n_inc_total_f[,2:6,]
mns_gc <- a10_gc_nbc$n_inc_insch_m[,2:6,]
mcs_gc <- a10_gc_obs$n_inc_insch_m[,2:6,]
mnt_gc <- a10_gc_nbc$n_inc_total_m[,2:6,]
mct_gc <- a10_gc_obs$n_inc_total_m[,2:6,]

plotyears <- 2008:2017

#################################################################################
## Helper functions

asum <- function(x, y) apply(x, y, sum)
sum_nbc_f_ct <- function(x, dim) asum(a10_ct_nbc_100[[x]]$n_inc_insch_f[,2:6,3:12], dim)
sum_nbc_m_ct <- function(x, dim) asum(a10_ct_nbc_100[[x]]$n_inc_insch_m[,2:6,3:12], dim)
sum_obs_f_ct <- function(x, dim) asum(a10_ct_obs_100[[x]]$n_inc_insch_f[,2:6,3:12], dim)
sum_obs_m_ct <- function(x, dim) asum(a10_ct_obs_100[[x]]$n_inc_insch_m[,2:6,3:12], dim)
sum_nbc_f_gc <- function(x, dim) asum(a10_gc_nbc_100[[x]]$n_inc_insch_f[,2:6,3:12], dim)
sum_nbc_m_gc <- function(x, dim) asum(a10_gc_nbc_100[[x]]$n_inc_insch_m[,2:6,3:12], dim)
sum_obs_f_gc <- function(x, dim) asum(a10_gc_obs_100[[x]]$n_inc_insch_f[,2:6,3:12], dim)
sum_obs_m_gc <- function(x, dim) asum(a10_gc_obs_100[[x]]$n_inc_insch_m[,2:6,3:12], dim)

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
table2a <- round(cbind(nia_ct_f_year_pt, nia_ct_f_year_lb, nia_ct_f_year_ub),0)
table2a

temp_nbc_ct <- sapply(1:100, function(x) sum_nbc_m_ct(x,3)) 
temp_obs_ct <- sapply(1:100, function(x) sum_obs_m_ct(x,3)) 
nia_ct_m_year_pt <- (asum(mns_ct[,,3:12],3) - asum(mcs_ct[,,3:12],3))
nia_ct_m_year_lb <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.025))
nia_ct_m_year_ub <- apply((temp_nbc_ct-temp_obs_ct), 1, quantile, c(0.975))
table2b <- round(cbind(nia_ct_m_year_pt, nia_ct_m_year_lb, nia_ct_m_year_ub),0)
table2b

temp_nbc_gc <- sapply(1:100, function(x) sum_nbc_f_gc(x,3)) 
temp_obs_gc <- sapply(1:100, function(x) sum_obs_f_gc(x,3)) 
nia_gc_f_year_pt <- (asum(fns_gc[,,3:12],3) - asum(fcs_gc[,,3:12],3))
nia_gc_f_year_lb <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.025))
nia_gc_f_year_ub <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.975))
table2c <- round(cbind(nia_gc_f_year_pt, nia_gc_f_year_lb, nia_gc_f_year_ub),0)
table2c

temp_nbc_gc <- sapply(1:100, function(x) sum_nbc_m_gc(x,3)) 
temp_obs_gc <- sapply(1:100, function(x) sum_obs_m_gc(x,3)) 
nia_gc_m_year_pt <- (asum(mns_gc[,,3:12],3) - asum(mcs_gc[,,3:12],3))
nia_gc_m_year_lb <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.025))
nia_gc_m_year_ub <- apply((temp_nbc_gc-temp_obs_gc), 1, quantile, c(0.975))
table2d <- round(cbind(nia_gc_m_year_pt, nia_gc_m_year_lb, nia_gc_m_year_ub),0)
table2d

cbind(table2a[,1], table2b[,1], table2c[,1], table2d[,1])







(annual_savings_ct_f <- table2a * costs$CT_F)
(annual_savings_ct_m <- table2b * costs$CT_M)
(annual_savings_gc_f <- table2c * costs$GC_F)
(annual_savings_gc_m <- table2d * costs$GC_M)
colSums(annual_savings_ct_f) # Double check Excel (rounding error OK)
colSums(annual_savings_ct_m) # Double check Excel (rounding error OK)
colSums(annual_savings_gc_f) # Double check Excel (rounding error OK)
colSums(annual_savings_gc_m) # Double check Excel (rounding error OK)

colSums(annual_savings_ct_f)[1] + 
  colSums(annual_savings_ct_m)[1] + 
  colSums(annual_savings_gc_f)[1] + 
  colSums(annual_savings_gc_m)[1]

colSums(annual_savings_ct_f)[1] + 
  colSums(annual_savings_gc_f)[1]

colSums(annual_savings_ct_m)[1] +
  colSums(annual_savings_gc_m)[1]

colSums(annual_savings_ct_f)[1] + 
  colSums(annual_savings_ct_m)[1] 

colSums(annual_savings_gc_f)[1] + 
  colSums(annual_savings_gc_m)[1]

(colSums(annual_savings_ct_f)[1] + 
  colSums(annual_savings_ct_m)[1]) / 
  (
    colSums(annual_savings_ct_f)[1] + 
      colSums(annual_savings_ct_m)[1] + 
      colSums(annual_savings_gc_f)[1] + 
      colSums(annual_savings_gc_m)[1]
  )





#########################################
## Figure 2: race-specific year-specific NIAs and PIAs      

temp_nbc_ct <- array(sapply(1:100, function(x) 
  apply(a10_ct_nbc_100[[x]]$n_inc_insch_f[,2:6,3:12], c(1,3), sum) + 
  apply(a10_ct_nbc_100[[x]]$n_inc_insch_m[,2:6,3:12], c(1,3), sum)), 
  dim=c(3,10,100))

temp_obs_ct <- array(sapply(1:100, function(x) 
  apply(a10_ct_obs_100[[x]]$n_inc_insch_f[,2:6,3:12], c(1,3), sum) + 
  apply(a10_ct_obs_100[[x]]$n_inc_insch_m[,2:6,3:12], c(1,3), sum)), 
  dim=c(3,10,100))

temp_nbc_gc <- array(sapply(1:100, function(x) 
  apply(a10_gc_nbc_100[[x]]$n_inc_insch_f[,2:6,3:12], c(1,3), sum) + 
  apply(a10_gc_nbc_100[[x]]$n_inc_insch_m[,2:6,3:12], c(1,3), sum)), 
  dim=c(3,10,100))

temp_obs_gc <- array(sapply(1:100, function(x) 
  apply(a10_gc_obs_100[[x]]$n_inc_insch_f[,2:6,3:12], c(1,3), sum) + 
  apply(a10_gc_obs_100[[x]]$n_inc_insch_m[,2:6,3:12], c(1,3), sum)), 
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

mycolors <- c('black', 'gray40', 'gray60')
mylty <- 3:1

#bmp("../output/a10_race.bmp", width=800, height=800)

tiff("../output/a10_race.tif", width=6*300, height=6*300,
     units = "px", res = 300, pointsize = 8,  compression = "lzw")

plotoffset = 0.07
par(mfrow=c(2,1))
plot(plotyears-plotoffset*3, nia_ct_raceyr_pt[1,], pch=16, col=mycolors[1], 
     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), ylim=c(0,3e5), 
     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
     yaxt = 'n',
     xlab="Year", ylab="NIA (mean and CI)",
     main="NIA by race/ethnicity and year"
)
axis(2, at = seq(0,3e5,5e4), las=1,
     labels=c('0k', '50k', '100k', '150k', '200k', '250k', '300k'))
errbar(plotyears-plotoffset*3, nia_ct_raceyr_ub[1,], nia_ct_raceyr_lb[1,], col=mycolors[1], lty=mylty[1])
points(plotyears-plotoffset*2, nia_ct_raceyr_pt[2,], pch=16, col=mycolors[2])
errbar(plotyears-plotoffset*2, nia_ct_raceyr_ub[2,], nia_ct_raceyr_lb[2,], col=mycolors[2], lty=mylty[2])
points(plotyears-plotoffset*1, nia_ct_raceyr_pt[3,], pch=16, col=mycolors[3])
errbar(plotyears-plotoffset*1, nia_ct_raceyr_ub[3,], nia_ct_raceyr_lb[3,], col=mycolors[3], lty=mylty[3])
points(plotyears+plotoffset*1, nia_gc_raceyr_pt[1,], pch=4, col=mycolors[1])
errbar(plotyears+plotoffset*1, nia_gc_raceyr_ub[1,], nia_gc_raceyr_lb[1,], col=mycolors[1], lty=mylty[1])
points(plotyears+plotoffset*2, nia_gc_raceyr_pt[2,], pch=4, col=mycolors[2])
errbar(plotyears+plotoffset*2, nia_gc_raceyr_ub[2,], nia_gc_raceyr_lb[2,], col=mycolors[2], lty=mylty[2])
points(plotyears+plotoffset*3, nia_gc_raceyr_pt[3,], pch=4, col=mycolors[3])
errbar(plotyears+plotoffset*3, nia_gc_raceyr_ub[3,], nia_gc_raceyr_lb[3,], col=mycolors[3], lty=mylty[3])
legend(2008, 3e5, 
       legend=c('Black, chlamydia', 'Hispanic, chlamydia', 'White, chlamdyia',
                'Black, gonorrhea', 'Hispanic, gonorrhea', 'White, gonorrhea'),
       pch = rep(c(16,4), each=3),
       col = rep(mycolors, 2),
       lty = rep(mylty, 2),
       ncol=2)

plot(plotyears-plotoffset*3, pia_ct_raceyr_pt[1,], pch=16, col=mycolors[1],
     xlim=c(min(plotyears)-0.5, max(plotyears)+0.5), ylim=c(0,0.8), 
     xaxp=c(min(plotyears),max(plotyears),length(plotyears)-1),
     xlab="Year", ylab="PIA (mean and CI)",
     main="PIA by race/ethnicity and year"
)
errbar(plotyears-plotoffset*3, pia_ct_raceyr_ub[1,], pia_ct_raceyr_lb[1,], col=mycolors[1], lty=mylty[1])
points(plotyears-plotoffset*2, pia_ct_raceyr_pt[2,], pch=16, col=mycolors[2])
errbar(plotyears-plotoffset*2, pia_ct_raceyr_ub[2,], pia_ct_raceyr_lb[2,], col=mycolors[2], lty=mylty[2])
points(plotyears-plotoffset*1, pia_ct_raceyr_pt[3,], pch=16, col=mycolors[3])
errbar(plotyears-plotoffset*1, pia_ct_raceyr_ub[3,], pia_ct_raceyr_lb[3,], col=mycolors[3], lty=mylty[3])
points(plotyears+plotoffset*1, pia_gc_raceyr_pt[1,], pch=4, col=mycolors[1])
errbar(plotyears+plotoffset*1, pia_gc_raceyr_ub[1,], pia_gc_raceyr_lb[1,], col=mycolors[1], lty=mylty[1])
points(plotyears+plotoffset*2, pia_gc_raceyr_pt[2,], pch=4, col=mycolors[2])
errbar(plotyears+plotoffset*2, pia_gc_raceyr_ub[2,], pia_gc_raceyr_lb[2,], col=mycolors[2], lty=mylty[2])
points(plotyears+plotoffset*3, pia_gc_raceyr_pt[3,], pch=4, col=mycolors[3])
errbar(plotyears+plotoffset*3, pia_gc_raceyr_ub[3,], pia_gc_raceyr_lb[3,], col=mycolors[3], lty=mylty[3])
legend(2008, 0.8, 
       legend=c('Black, chlamydia', 'Hispanic, chlamydia', 'White, chlamdyia',
                         'Black, gonorrhea', 'Hispanic, gonorrhea', 'White, gonorrhea'),
       pch = rep(c(16,4), each=3),
       col = rep(mycolors, 2),
       lty = rep(mylty, 2),
       ncol=2)
dev.off()

#########################################
# Text: numbers summarizing Figure 2

rowSums(nia_ct_raceyr_pt)
#prop.table(rowSums(nia_ct_raceyr_pt))
rowSums(nia_gc_raceyr_pt)
#prop.table(rowSums(nia_gc_raceyr_pt))
rowSums(nia_ct_raceyr_pt + nia_gc_raceyr_pt)
#prop.table(rowSums(nia_ct_raceyr_pt + nia_gc_raceyr_pt))

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

pia_gc_race_pt
pia_ct_race_pt
pia_gcct_race_pt

#########################################
## Figure 3: age-specific NIAs and PIAs      

temp_nbc_ct <- array(sapply(1:100, function(x) 
  apply(a10_ct_nbc_100[[x]]$n_inc_insch_f[,2:6,3:12], c(2), sum) + 
    apply(a10_ct_nbc_100[[x]]$n_inc_insch_m[,2:6,3:12], c(2), sum)), 
  dim=c(5,1,100))

temp_obs_ct <- array(sapply(1:100, function(x) 
  apply(a10_ct_obs_100[[x]]$n_inc_insch_f[,2:6,3:12], c(2), sum) + 
    apply(a10_ct_obs_100[[x]]$n_inc_insch_m[,2:6,3:12], c(2), sum)), 
  dim=c(5,1,100))

temp_nbc_gc <- array(sapply(1:100, function(x) 
  apply(a10_gc_nbc_100[[x]]$n_inc_insch_f[,2:6,3:12], c(2), sum) + 
    apply(a10_gc_nbc_100[[x]]$n_inc_insch_m[,2:6,3:12], c(2), sum)), 
  dim=c(5,1,100))

temp_obs_gc <- array(sapply(1:100, function(x) 
  apply(a10_gc_obs_100[[x]]$n_inc_insch_f[,2:6,3:12], c(2), sum) + 
    apply(a10_gc_obs_100[[x]]$n_inc_insch_m[,2:6,3:12], c(2), sum)), 
  dim=c(5,1,100))

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

# Number in text re percent of cases in 16-18 yo

casesbyage <- asum(fns_ct[,,3:12], c(2)) + 
              asum(mns_ct[,,3:12], c(2)) +
              asum(fns_gc[,,3:12], c(2)) +
              asum(mns_gc[,,3:12], c(2))

sum(casesbyage[3:5])/sum(casesbyage)
    
bmp("../output/a10_age.bmp", width=500, height=800)

tiff("../output/a10_age.tif", width=4*300, height=6*300,
     units = "px", res = 300, pointsize = 8,  compression = "lzw")

mycolors <- c('black', 'gray50')
mylty = 2:1
  
plotoffset = 0.07
par(mfrow=c(2,1))
plot((14:18)-plotoffset, nia_ct_age_pt, pch=16, col=mycolors[1],
     ylim=c(0,8e5), xlim=c(13.8,18.2),
     yaxt = 'n',
     xlab="Age", ylab="NIA (mean and CI)",
     main="NIA by age"
)
axis(2, at = seq(0,8e5,1e5), las=1,
     labels=c('0k', '100k', '200k', '300k', '400k', '500k', '600k', '700k', '800k'))
errbar((14:18)-plotoffset, nia_ct_age_ub, nia_ct_age_lb, col=mycolors[1], lty=mylty[1])
points((14:18)+plotoffset, nia_gc_age_pt, pch=4, col=mycolors[2])
errbar((14:18)+plotoffset, nia_gc_age_ub, nia_gc_age_lb, col=mycolors[2], lty=mylty[2])

legend(14, 8e5, 
       legend=c('Chlamydia', 'Gonorrhea'),
       pch = c(16,4),
       col = mycolors, 
       lty= mylty
)

plot((14:18)-plotoffset, pia_ct_age_pt, pch=16, col=mycolors[1],
     ylim=c(0, 0.5), xlim=c(13.8,18.2),
     xlab="Age", ylab="PIA (mean and CI)",
     main="PIA by age"
)
errbar((14:18)-plotoffset, pia_ct_age_ub, pia_ct_age_lb, col=mycolors[1], lty=mylty[1])
points((14:18)+plotoffset, pia_gc_age_pt, pch=4, col=mycolors[2])
errbar((14:18)+plotoffset, pia_gc_age_ub, pia_gc_age_lb, col=mycolors[2], lty=mylty[2])

legend(16, 0.5, 
       legend=c('Chlamydia', 'Gonorrhea'),
       pch = c(16,4),
       col = mycolors, 
       lty= mylty
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


########################################################################
# Supplement
########################################################################

#####################
# Figure S1

surveyyears <- seq(2007, 2017, by=2)

bmp("../output/predicted_eversex_m.bmp", width=920, height=700)

layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))

par(mar=c(0, 0, 0, 0))
plot(0:1, 0:1, xaxt = 'n', yaxt = 'n', bty = 'n', 
     pch = '', ylab = '', xlab = '')
legend(x=0.18, y=0.15, 
       legend=c('Age 14 reported', 'Age 15 reported', 'Age 16 reported', 
                'Age 17 reported', 'Age 18 reported', '',
                'Age 14 predicted', 'Age 15 predicted', 'Age 16 predicted', 
                'Age 17 predicted', 'Age 18 predicted', ''),
       lty=rep(1:2,each=6), col=rep(c(rainbow(5),"white"),2), ncol=4, cex=1.5)

par(mar=c(5.1, 5.1, 2.1, 2.1))
matplot(surveyyears, t(prop_eversex_m[1,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="Prop. reporting previous sexual intercourse",
        main = "Black male students", cex.axis=1.75, cex.lab = 1.75, cex.main=1.75)
matplot(surveyyears, t(pred_eversex_m_dyn[1,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

par(mar=c(5.1, 1.5, 2.1, 2.1))
matplot(surveyyears, t(prop_eversex_m[2,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="",
        main = "Hispanic male students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_eversex_m_dyn[2,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

matplot(surveyyears, t(prop_eversex_m[3,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="",
        main = "White male students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_eversex_m_dyn[3,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

dev.off()

###

surveyyears <- seq(2007, 2017, by=2)

bmp("../output/predicted_condom_f.bmp", width=920, height=700)

layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))

par(mar=c(0, 0, 0, 0))
plot(0:1, 0:1, xaxt = 'n', yaxt = 'n', bty = 'n', 
     pch = '', ylab = '', xlab = '')
legend(x=0.18, y=0.15, 
       legend=c('Age 14 reported', 'Age 15 reported', 'Age 16 reported', 
                'Age 17 reported', 'Age 18 reported', '',
                'Age 14 predicted', 'Age 15 predicted', 'Age 16 predicted', 
                'Age 17 predicted', 'Age 18 predicted', ''),
       lty=rep(1:2,each=6), col=rep(c(rainbow(5),"white"),2), ncol=4, cex=1.5)

par(mar=c(5.1, 5.1, 2.1, 2.1))
matplot(surveyyears, t(condom_f[1,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="Prop. reporting condom use at last sexual intercourse",
        main = "Black female students", cex.axis=1.75, cex.lab = 1.75, cex.main=1.75)
matplot(surveyyears, t(pred_condom_f_dyn[1,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

par(mar=c(5.1, 1.5, 2.1, 2.1))
matplot(surveyyears, t(condom_f[2,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="",
        main = "Hispanic female students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_condom_f_dyn[2,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

matplot(surveyyears, t(condom_f[3,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="",
        main = "White female students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_condom_f_dyn[3,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

dev.off()


###

surveyyears <- seq(2007, 2017, by=2)

bmp("../output/predicted_condom_m.bmp", width=920, height=700)

layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))

par(mar=c(0, 0, 0, 0))
plot(0:1, 0:1, xaxt = 'n', yaxt = 'n', bty = 'n', 
     pch = '', ylab = '', xlab = '')
legend(x=0.18, y=0.15, 
       legend=c('Age 14 reported', 'Age 15 reported', 'Age 16 reported', 
                'Age 17 reported', 'Age 18 reported', '',
                'Age 14 predicted', 'Age 15 predicted', 'Age 16 predicted', 
                'Age 17 predicted', 'Age 18 predicted', ''),
       lty=rep(1:2,each=6), col=rep(c(rainbow(5),"white"),2), ncol=4, cex=1.5)

par(mar=c(5.1, 5.1, 2.1, 2.1))
matplot(surveyyears, t(condom_m[1,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="Prop. reporting condom use at last sexual intercourse",
        main = "Black male students", cex.axis=1.75, cex.lab = 1.75, cex.main=1.75)
matplot(surveyyears, t(pred_condom_m_dyn[1,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

par(mar=c(5.1, 1.5, 2.1, 2.1))
matplot(surveyyears, t(condom_m[2,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="",
        main = "Hispanic male students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_condom_m_dyn[2,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

matplot(surveyyears, t(condom_m[3,2:6,]), pch=16, ylim=c(0,1), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5),
        xlab= "Survey year", 
        ylab="",
        main = "White male students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_condom_m_dyn[3,2:6,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5))

dev.off()

###

surveyyears <- seq(2007, 2017, by=2)

bmp("../output/predicted_mnnppy_f.bmp", width=920, height=700)

layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))

par(mar=c(0, 0, 0, 0))
plot(0:1, 0:1, xaxt = 'n', yaxt = 'n', bty = 'n', 
     pch = '', ylab = '', xlab = '')
legend(x=0.18, y=0.15, 
       legend=c('Age 14 reported', 'Age 15 reported', 'Age 16 reported', 
                'Age 17 reported', '', '',
                'Age 14 predicted', 'Age 15 predicted', 'Age 16 predicted', 
                'Age 17 predicted', '', ''),
       lty=rep(1:2,each=6), col=rep(c(rainbow(5)[1:4],"white","white"),2), ncol=4, cex=1.5)

par(mar=c(5.1, 5.1, 2.1, 2.1))
matplot(surveyyears, t(mnppy_f[1,2:6,]), pch=16, ylim=c(0,2), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5)[1:4],
        xlab= "Survey year", 
        ylab="Est. mean number of new partners per year",
        main = "Black female students", cex.axis=1.75, cex.lab = 1.75, cex.main=1.75)
matplot(surveyyears, t(pred_mnppy_f_dyn[1,2:5,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5)[1:4])

par(mar=c(5.1, 1.5, 2.1, 2.1))
matplot(surveyyears, t(mnppy_f[2,2:6,]), pch=16, ylim=c(0,2), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5)[1:4],
        xlab= "Survey year", 
        ylab="",
        main = "Hispanic female students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_mnppy_f_dyn[2,2:5,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5)[1:4])

matplot(surveyyears, t(mnppy_f[3,2:6,]), pch=16, ylim=c(0,2), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5)[1:4],
        xlab= "Survey year", 
        ylab="",
        main = "White female students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_mnppy_f_dyn[3,2:5,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5)[1:4])

dev.off()

###

surveyyears <- seq(2007, 2017, by=2)

bmp("../output/predicted_mnnppy_m.bmp", width=920, height=700)

layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE))

par(mar=c(0, 0, 0, 0))
plot(0:1, 0:1, xaxt = 'n', yaxt = 'n', bty = 'n', 
     pch = '', ylab = '', xlab = '')
legend(x=0.18, y=0.15, 
       legend=c('Age 14 reported', 'Age 15 reported', 'Age 16 reported', 
                'Age 17 reported', '', '',
                'Age 14 predicted', 'Age 15 predicted', 'Age 16 predicted', 
                'Age 17 predicted', '', ''),
       lty=rep(1:2,each=6), col=rep(c(rainbow(5)[1:4],"white","white"),2), ncol=4, cex=1.5)

par(mar=c(5.1, 5.1, 2.1, 2.1))
matplot(surveyyears, t(mnppy_m[1,2:6,]), pch=16, ylim=c(0,2), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5)[1:4],
        xlab= "Survey year", 
        ylab="Est. mean number of new partners per year",
        main = "Black male students", cex.axis=1.75, cex.lab = 1.75, cex.main=1.75)
matplot(surveyyears, t(pred_mnppy_m_dyn[1,2:5,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5)[1:4])

par(mar=c(5.1, 1.5, 2.1, 2.1))
matplot(surveyyears, t(mnppy_m[2,2:6,]), pch=16, ylim=c(0,2), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5)[1:4],
        xlab= "Survey year", 
        ylab="",
        main = "Hispanic male students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_mnppy_m_dyn[2,2:5,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5)[1:4])

matplot(surveyyears, t(mnppy_m[3,2:6,]), pch=16, ylim=c(0,2), 
        xaxp=c(2007, 2017, 5), type='b', lty=1, col=rainbow(5)[1:4],
        xlab= "Survey year", 
        ylab="",
        main = "White male students", cex.axis=1.75, cex.lab=1.75, cex.main=1.75)
matplot(surveyyears, t(pred_mnppy_m_dyn[3,2:5,c(1,3,5,7,9,11)]), 
        pch=16, type='b', add=T, lty=2, col=rainbow(5)[1:4])

dev.off()

######################################################################

bmp("../output/incidence.bmp", width=800, height=800)

par(mfrow=c(2,1))

#matplot(plotyears, t(apply(fns_ct[,,2:12], c(1,3), sum)), type='b', ylim = c(0, 3e5), 
#        pch=16, xaxp=c(2007, 2017, 10), lty=1, col=c('black', 'red', 'blue'),
#        xlab= "Year", 
#        yaxt = 'n',
#        ylab="Incident cases",
#        main = "Chlamydia"
#        )
#axis(2, at = seq(0,3e5,5e4), las=1,
#     labels=c('0k', '50k', '100k', '150k', '200k', '250k', '300k'))
#
#matplot(plotyears, t(apply(fcs_ct[,,2:12], c(1,3), sum)), type='b', ylim = c(0, 3e5), 
#        pch=21, xaxp=c(2007, 2017, 10), lty=1, col=c('black', 'red', 'blue'),
#        xlab= "Year", 
#        ylab="",
#        main = "Chlamydia", cex.axis=1.75, cex.lab=1.75, cex.main=1.75,
#        add=TRUE
#        )

#matplot(2007:2017, t(apply(mns_ct[,,2:12], c(1,3), sum)), type='b', ylim = c(0, 3e5), 
#        pch=17, xaxp=c(2007, 2017, 10), lty=1, col=c('black', 'red', 'blue'),
#        xlab= "Year", 
#        ylab="",
#        main = "Chlamydia", cex.axis=1.75, cex.lab=1.75, cex.main=1.75,
#        add=TRUE
#        )

#matplot(2007:2017, t(apply(mcs_ct[,,2:12], c(1,3), sum)), type='b', ylim = c(0, 3e5), 
#        pch=2, xaxp=c(2007, 2017, 10), lty=1, col=c('black', 'red', 'blue'),
#        xlab= "Year", 
#        ylab="",
#        main = "Chlamydia", cex.axis=1.75, cex.lab=1.75, cex.main=1.75,
#        add=TRUE
#        )

matplot(2007:2017, t(apply(fns_gc[,,2:12], c(1,3), sum)), type='b', ylim = c(0, 8e4), 
        pch=16, xaxp=c(2007, 2017, 10), lty=1, col=c('black', 'red', 'blue'),
        xlab= "Year", 
        yaxt = 'n',
        ylab="Incident cases of Gonorrhea",
        main = "Female students"
)
axis(2, at = seq(0,8e4,1e4), las=1,
     labels=c('0k', '10k', '20k', '30k', '40k', '50k', '60k', '70k', '80k'))

matplot(2007:2017, t(apply(fcs_gc[,,2:12], c(1,3), sum)), type='b',
        pch=21, lty=1, col=c('black', 'red', 'blue'),
        add=TRUE
)
legend(x=2007, y=8e4, 
       legend=c('Black female students, no behavior change',
                'Hisp. female students, no behavior change',
                'White female students, no behavior change',
                'Black female students, est. behavior change',
                'Hisp. female students, est. behavior change',
                'White female students, est. behavior change'),
       pch = rep(c(16,21),each=3), 
       col=rep(c('black', 'red', 'blue'),2), ncol=2)

matplot(2007:2017, t(apply(mns_gc[,,2:12], c(1,3), sum)), type='b', ylim = c(0, 8e4), 
        pch=16, xaxp=c(2007, 2017, 10), lty=1, col=c('black', 'red', 'blue'),
        xlab= "Year", 
        yaxt = 'n',
        ylab="Incident cases of Gonorrhea",
        main = "Male students",
        add=FALSE
)
axis(2, at = seq(0,8e4,1e4), las=1,
     labels=c('0k', '10k', '20k', '30k', '40k', '50k', '60k', '70k', '80k'))

matplot(2007:2017, t(apply(mcs_gc[,,2:12], c(1,3), sum)), type='b', 
        pch=21, lty=1, col=c('black', 'red', 'blue'),
        add=TRUE
)
legend(x=2007, y=8e4, 
       legend=c('Black male students, no behavior change',
                'Hisp. male students, no behavior change',
                'White male students, no behavior change',
                'Black male students, est. behavior change',
                'Hisp. male students, est. behavior change',
                'White male students, est. behavior change'),
       pch = rep(c(16,21),each=3), 
       col=rep(c('black', 'red', 'blue'),2), ncol=2)




dev.off()


