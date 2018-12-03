

bbb <- lm(condom_f[1,,1]~ages, weights = wts_f[1,,1])
plot(ages,condom_f[1,,1], ylim=c(0,1))
lines(14:18,predict(bbb))
ccc <- lm(condom_f[1,,6]~ages, weights = wts_f[1,,6])
points(ages,condom_f[1,,6], ylim=c(0,1))
lines(14:18,predict(ccc))

