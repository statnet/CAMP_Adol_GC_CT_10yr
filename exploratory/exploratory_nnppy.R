
# Test to confirm pp)_backcalc yields expected answers
#popsizes <- read.csv("../exploratory/popsizes_default.csv", header = FALSE)
#lifeparts <- read.csv("../exploratory/lifeparts_default.csv", header = FALSE)
#rowages <- 13:18
#colages <- 11:17
#returnages <- 13:18
#temp <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)


mnppy_f <- array(dim=c(3,6,6))
mnppy_f_wts <- array(dim=c(3,6,6))
rowages <- 13:18
colages <- 11:17
returnages <- 13:18
for (i in 1:6) {
  for (j in 1:3) {
    popsizes <- AgeByDebutAge_num_f[j,,,i]
    lifeparts <- AgeByDebutAge_lp_f[j,,,i]
    temp <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)
    mnppy_f[j,,i] <- temp$mnppy
    mnppy_f_wts[j,,i] <- temp$wts
  }  
}

mnppy_m <- array(dim=c(3,6,6))
mnppy_m_wts <- array(dim=c(3,6,6))
rowages <- 13:18
colages <- 11:17
returnages <- 13:18
for (i in 1:6) {
  for (j in 1:3) {
    popsizes <- AgeByDebutAge_num_m[j,,,i]
    lifeparts <- AgeByDebutAge_lp_m[j,,,i]
    temp <- ppy_backcalc(popsizes, lifeparts, rowages, colages, 13:18)
    mnppy_m[j,,i] <- temp$mnppy
    mnppy_m_wts[j,,i] <- temp$wts
  }  
}

matplot(13:18, t(mnppy_f[,,1]),type='l',ylim=c(0,2), col=c("black", ' blue', 'red'), lty=1)
matplot(13:18, t(mnppy_f[,,2]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=2)
matplot(13:18, t(mnppy_f[,,3]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=3)
matplot(13:18, t(mnppy_f[,,4]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=4)
matplot(13:18, t(mnppy_f[,,5]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=5)
matplot(13:18, t(mnppy_f[,,6]),type='l', add=TRUE, col=c("black", ' blue', 'red'), lty=6)

matplot(years, t(mnppy_f[,1,]),type='l')  # MNPPY for 13yo over time - declining
matplot(years, t(mnppy_f[,2,]),type='l')  # MNPPY for 14yo over time - declining
matplot(years, t(mnppy_f[,3,]),type='l')  # MNPPY for 15yo over time - declining?
matplot(years, t(mnppy_f[,4,]),type='l')  # MNPPY for 16yo over time - declining for H?
matplot(years, t(mnppy_f[,5,]),type='l')  # MNPPY for 17yo over time - ?
matplot(years, t(mnppy_f[,6,]),type='l')  # MNPPY for 18yo over time

matplot(years, t(mnppy_m[,1,]),type='l')  # MNPPY for 13yo over time - declining
matplot(years, t(mnppy_m[,2,]),type='l')  # MNPPY for 14yo over time - declining
matplot(years, t(mnppy_m[,3,]),type='l')  # MNPPY for 15yo over time - declining?
matplot(years, t(mnppy_m[,4,]),type='l')  # MNPPY for 16yo over time - declining for H?
matplot(years, t(mnppy_m[,5,]),type='l')  # MNPPY for 17yo over time - declining for H?
matplot(years, t(mnppy_m[,6,]),type='l')  # MNPPY for 18yo over time
