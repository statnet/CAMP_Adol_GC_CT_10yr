
# Test to confirm pp)_backcalc yields expected answers
#popsizes <- read.csv("../exploratory/popsizes_default.csv", header = FALSE)
#lifeparts <- read.csv("../exploratory/lifeparts_default.csv", header = FALSE)
#rowages <- 13:18
#colages <- 11:17
#returnages <- 13:18
#temp <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)


mnppy_f <- array(dim=c(3,6,6))
mnppy_wts_f <- array(dim=c(3,6,6))
rowages <- 13:18
colages <- 11:17
returnages <- 13:18
for (i in 1:6) {
  for (j in 1:3) {
    popsizes <- AgeByDebutAge_num_f[j,,,i]
    lifeparts <- AgeByDebutAge_lp_f[j,,,i]
    temp <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)
    mnppy_f[j,,i] <- temp$mnppy
    mnppy_wts_f[j,,i] <- temp$wts
  }  
}

mnppy_m <- array(dim=c(3,6,6))
mnppy_wts_m <- array(dim=c(3,6,6))
rowages <- 13:18
colages <- 11:17
returnages <- 13:18
for (i in 1:6) {
  for (j in 1:3) {
    popsizes <- AgeByDebutAge_num_m[j,,,i]
    lifeparts <- AgeByDebutAge_lp_m[j,,,i]
    temp <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)
    mnppy_m[j,,i] <- temp$mnppy
    mnppy_wts_m[j,,i] <- temp$wts
  }  
}

matplot(13:18, t(mnppy_f[,,1]),type='l',ylim=c(0,2), col=c("black", ' blue', 'red'), lty=1)
matplot(13:18, t(mnppy_f[,,2]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=2)
matplot(13:18, t(mnppy_f[,,3]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=3)
matplot(13:18, t(mnppy_f[,,4]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=4)
matplot(13:18, t(mnppy_f[,,5]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=5)
matplot(13:18, t(mnppy_f[,,6]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=6)

matplot(years, t(mnppy_f[,1,]),type='l', ylim=c(0,2))  # MNPPY for 13yo over time - declining
matplot(years, t(mnppy_f[,2,]),type='l', ylim=c(0,2))  # MNPPY for 14yo over time - declining
matplot(years, t(mnppy_f[,3,]),type='l', ylim=c(0,2))  # MNPPY for 15yo over time - declining?
matplot(years, t(mnppy_f[,4,]),type='l', ylim=c(0,2))  # MNPPY for 16yo over time - declining for H?
matplot(years, t(mnppy_f[,5,]),type='l', ylim=c(0,2))  # MNPPY for 17yo over time - ?
#matplot(years, t(mnppy_f[,6,]),type='l', ylim=c(0,2))  # MNPPY for 18yo over time

matplot(years, t(mnppy_m[,1,]),type='l', ylim=c(0,2))  # MNPPY for 13yo over time - declining
matplot(years, t(mnppy_m[,2,]),type='l', ylim=c(0,2))  # MNPPY for 14yo over time - declining
matplot(years, t(mnppy_m[,3,]),type='l', ylim=c(0,2))  # MNPPY for 15yo over time - declining?
matplot(years, t(mnppy_m[,4,]),type='l', ylim=c(0,2))  # MNPPY for 16yo over time - declining for H?
matplot(years, t(mnppy_m[,5,]),type='l', ylim=c(0,2))  # MNPPY for 17yo over time - declining for H?
#matplot(years, t(mnppy_m[,6,]),type='l', ylim=c(0,2))  # MNPPY for 18yo over time

###############################################

mnppy_f_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(mnppy_f_df) <- c('ethn', 'age', 'year')
mnppy_f_df$mnppy <- as.vector(mnppy_f)
mnppy_f_df$wts <- as.vector(mnppy_wts_f)
mnppy_f_df$agefac <- relevel(as.factor(mnppy_f_df$age), ref='16')
mnppy_f_df$y2k <- mnppy_f_df$year - 2000

mnppy_m_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(mnppy_m_df) <- c('ethn', 'age', 'year')
mnppy_m_df$mnppy <- as.vector(mnppy_m)
mnppy_m_df$wts <- as.vector(mnppy_wts_m)
mnppy_m_df$agefac <- relevel(as.factor(mnppy_m_df$age), ref='16')
mnppy_m_df$y2k <- mnppy_m_df$year - 2000

mnppy_f_reg_1 <- glm(mnppy ~ ethn + y2k + ethn*y2k + age + I(age^2) +age*y2k + I(age^2)*y2k,
                      data=mnppy_f_df, weights=wts, na.action=na.exclude,
                      family="poisson")
pred_mnppy_f_1 <- array(predict(mnppy_f_reg_1, type='response', newdata = mnppy_f_df), 
                        dim=c(3,6,6))

matplot(t(mnppy_f[1,,]), type='l', ylim=c(0,2), col=rainbow(6), lty=1, lwd=1.5)
matplot(t(pred_mnppy_f_1[1,,]), type='l', add=TRUE, col=rainbow(6), lty=2, lwd=0.5)
matplot(t(mnppy_f[2,,]), type='l', ylim=c(0,2), col=rainbow(6), lty=1, lwd=1.5)
matplot(t(pred_mnppy_f_1[2,,]), type='l', add=TRUE, col=rainbow(6), lty=2, lwd=0.5)
matplot(t(mnppy_f[3,,]), type='l', ylim=c(0,2), col=rainbow(6), lty=1, lwd=1.5)
matplot(t(pred_mnppy_f_1[3,,]), type='l', add=TRUE, col=rainbow(6), lty=2, lwd=0.5)

matplot((mnppy_f[1,,]), type='l', ylim=c(0,2), col=rainbow(6), lty=1, lwd=1.5)
matplot(pred_mnppy_f_1[1,,], type='l', add=TRUE, col=rainbow(6), lty=2, lwd=0.5)
matplot((mnppy_f[2,,]), type='l', ylim=c(0,2), col=rainbow(6), lty=1, lwd=1.5)
matplot(pred_mnppy_f_1[2,,], type='l', add=TRUE, col=rainbow(6), lty=2, lwd=0.5)
matplot((mnppy_f[3,,]), type='l', ylim=c(0,2), col=rainbow(6), lty=1, lwd=1.5)
matplot(pred_mnppy_f_1[3,,], type='l', add=TRUE, col=rainbow(6), lty=2, lwd=0.5)

matplot(t(pred_mnppy_f_1[1,,]), type='l', col=rainbow(6), lty=1, lwd=0.5, ylim=c(0,2))
matplot(t(pred_mnppy_f_1[2,,]), type='l', add=TRUE, col=rainbow(6), lty=2, lwd=0.5)
matplot(t(pred_mnppy_f_1[3,,]), type='l', add=TRUE, col=rainbow(6), lty=3, lwd=0.5)


## Look here
par(mfrow=c(1,3))
matplot(t(pred_mnppy_f_1[1,,]), type='l', col=rainbow(6), lty=2, lwd=0.5, xlim=c(1,6), ylim=c(0,2))
matplot(t(pred_mnppy_f_1[2,,]), type='l', col=rainbow(6), lty=2, lwd=0.5, xlim=c(1,6), ylim=c(0,2))
matplot(t(pred_mnppy_f_1[3,,]), type='l', col=rainbow(6), lty=2, lwd=0.5, xlim=c(1,6), ylim=c(0,2))
