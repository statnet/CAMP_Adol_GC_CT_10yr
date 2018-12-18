
# none of the existing functions seem to work with means 
# that are both weighted and across one dimension of a 3D array
#library(matrixStats)
#library(diagis)
#apply(condom_m, 1:2, weighted.mean, w=wts_m)
#weighted_mean(condom_m, 1:2, wts_m)
#rowWeightedMeans(condom_m, wts_m, 2:3)
#condom_m %>% weighted.mean()

#apply(condom_m * wts_m, c(1,3), sum) / apply(wts_m, c(1,3), sum)
#bbb <- lm(condom_f[1,,1]~ages, weights = wts_f[1,,1])
#plot(ages,condom_f[1,,1], ylim=c(0,1))
#lines(ages,predict(bbb))
#ccc <- lm(condom_f[1,,6]~ages, weights = wts_f[1,,6])
#points(ages,condom_f[1,,6], ylim=c(0,1))
#lines(ages,predict(ccc))

condom_f_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(condom_f_df) <- c('ethn', 'age', 'year')
condom_f_df$condom <- as.vector(condom_f)
condom_f_df$wts <- as.vector(condom_wts_f)
condom_f_df$agefac <- relevel(as.factor(condom_f_df$age), ref='16')

condom_m_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(condom_m_df) <- c('ethn', 'age', 'year')
condom_m_df$condom <- as.vector(condom_m)
condom_m_df$wts <- as.vector(condom_wts_m)
condom_m_df$agefac <- relevel(as.factor(condom_m_df$age), ref='16')

condom_f_reg_1 <- glm(condom ~ agefac + year + ethn + year*ethn,
                    data=condom_f_df, weights=wts, 
                    family="binomial")
pred_condom_f_lo_1 <- array(predict(condom_f_reg_1), dim=c(3,6,6))
pred_condom_f_1 <- exp(pred_condom_f_lo_1) / (1+exp(pred_condom_f_lo_1))

condom_m_reg_1 <- glm(condom ~ agefac + year + ethn + year*ethn,
                      data=condom_m_df, weights=wts, 
                      family="binomial")
pred_condom_m_lo_1 <- array(predict(condom_m_reg_1), dim=c(3,6,6))
pred_condom_m_1 <- exp(pred_condom_m_lo_1) / (1+exp(pred_condom_m_lo_1))

matplot(t(condom_f[1,,]), type='l')
matplot(t(pred_condom_f_1[1,,]), type='l', add=TRUE)
matplot(t(condom_f[2,,]), type='l')
matplot(t(pred_condom_f_1[2,,]), type='l', add=TRUE)
matplot(t(condom_f[3,,]), type='l')
matplot(t(pred_condom_f_1[3,,]), type='l', add=TRUE)

matplot(t(condom_m[1,,]), type='l')
matplot(t(pred_condom_m_1[1,,]), type='l', add=TRUE)
matplot(t(condom_m[2,,]), type='l')
matplot(t(pred_condom_m_1[2,,]), type='l', add=TRUE)
matplot(t(condom_m[3,,]), type='l')
matplot(t(pred_condom_m_1[3,,]), type='l', add=TRUE)

condom_f_reg_2 <- glm(condom ~ age + year + ethn + year*ethn,
                      data=condom_f_df, weights=wts, 
                      family="binomial")
pred_condom_f_lo_2 <- array(predict(condom_f_reg_2), dim=c(3,6,6))
pred_condom_f_2 <- exp(pred_condom_f_lo_2) / (1+exp(pred_condom_f_lo_2))

condom_m_reg_2 <- glm(condom ~ age + year + ethn + year*ethn,
                      data=condom_m_df, weights=wts, 
                      family="binomial")
pred_condom_m_lo_2 <- array(predict(condom_m_reg_2), dim=c(3,6,6))
pred_condom_m_2 <- exp(pred_condom_m_lo_2) / (1+exp(pred_condom_m_lo_2))

matplot(t(condom_f[1,,]), type='l')
matplot(t(pred_condom_f_2[1,,]), type='l', add=TRUE)
matplot(t(condom_f[2,,]), type='l')
matplot(t(pred_condom_f_2[2,,]), type='l', add=TRUE)
matplot(t(condom_f[3,,]), type='l')
matplot(t(pred_condom_f_2[3,,]), type='l', add=TRUE)

matplot(t(condom_m[1,,]), type='l')
matplot(t(pred_condom_m_2[1,,]), type='l', add=TRUE)
matplot(t(condom_m[2,,]), type='l')
matplot(t(pred_condom_m_2[2,,]), type='l', add=TRUE)
matplot(t(condom_m[3,,]), type='l')
matplot(t(pred_condom_m_2[3,,]), type='l', add=TRUE)
