
prop_eversex_f <- eversex_f / wts_f

prop_eversex_f_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(prop_eversex_f_df) <- c('ethn', 'age', 'year')
prop_eversex_f_df$prop_eversex <- as.vector(prop_eversex_f)
prop_eversex_f_df$wts <- as.vector(wts_f)
prop_eversex_f_df$agefac <- relevel(as.factor(prop_eversex_f_df$age), ref='16')

eversex_reg1 <- glm(prop_eversex~age+year+ethn, data=prop_eversex_f_df, weights=wts, 
    family="binomial")
summary(eversex_reg1) # Coefficients are intuitive

eversex_reg2 <- glm(prop_eversex~age+year+ethn+age*ethn+age*year+ethn*year, 
                    data=prop_eversex_f_df, weights=wts, 
                    family="binomial")
summary(eversex_reg2) # Coefficients are not so intuitive

eversex_reg3 <- glm(prop_eversex~age+year+ethn+age*ethn, 
                    data=prop_eversex_f_df, weights=wts, 
                    family="binomial")
summary(eversex_reg3) # Coefficients are moderately intuitive

eversex_reg4 <- glm(prop_eversex~agefac+year+ethn,
                    data=prop_eversex_f_df, weights=wts, 
                    family="binomial")
summary(eversex_reg4) # Coefficients are moderately intuitive

eversex_reg5 <- glm(prop_eversex~agefac+year+ethn+
                      year*ethn,
                    data=prop_eversex_f_df, weights=wts, 
                    family="binomial")
summary(eversex_reg5) # Coefficients are moderately intuitive

pred2.lo <- array(predict(eversex_reg2), dim=c(3,6,6))
pred2.p <- exp(pred2.lo) / (1+exp(pred2.lo))
pred3.lo <- array(predict(eversex_reg3), dim=c(3,6,6))
pred3.p <- exp(pred3.lo) / (1+exp(pred3.lo))
pred4.lo <- array(predict(eversex_reg4), dim=c(3,6,6))
pred4.p <- exp(pred4.lo) / (1+exp(pred4.lo))
pred5.lo <- array(predict(eversex_reg5), dim=c(3,6,6))
pred5.p <- exp(pred5.lo) / (1+exp(pred5.lo))

matplot(t(eversex_f[1,,]/wts_f[1,,]), type='l')
matplot(t(pred2.p[1,,]), type='l', add=TRUE)
matplot(t(pred3.p[1,,]), type='l', add=TRUE)
matplot(t(pred4.p[1,,]), type='l', add=TRUE)
matplot(t(pred5.p[1,,]), type='l', add=TRUE)

matplot(t(eversex_f[2,,]/wts_f[2,,]), type='l')
matplot(t(pred2.p[2,,]), type='l', add=TRUE)
matplot(t(pred3.p[2,,]), type='l', add=TRUE)
matplot(t(pred4.p[2,,]), type='l', add=TRUE)
matplot(t(pred5.p[2,,]), type='l', add=TRUE)

matplot(t(eversex_f[3,,]/wts_f[3,,]), type='l')
matplot(t(pred2.p[3,,]), type='l', add=TRUE)
matplot(t(pred3.p[3,,]), type='l', add=TRUE)
matplot(t(pred4.p[3,,]), type='l', add=TRUE)
matplot(t(pred5.p[3,,]), type='l', add=TRUE)

matplot(t(pred4.p[,1,]),type='l', ylim=c(0,1))
matplot(t(pred4.p[,2,]),type='l', add=T)
matplot(t(pred4.p[,3,]),type='l', add=T)
matplot(t(pred4.p[,4,]),type='l', add=T)
matplot(t(pred4.p[,5,]),type='l', add=T)
matplot(t(pred4.p[,6,]),type='l', add=T)

matplot(t(pred5.p[,1,]),type='l', ylim=c(0,1))
matplot(t(pred5.p[,2,]),type='l', add=T)
matplot(t(pred5.p[,3,]),type='l', add=T)
matplot(t(pred5.p[,4,]),type='l', add=T)
matplot(t(pred5.p[,5,]),type='l', add=T)
matplot(t(pred5.p[,6,]),type='l', add=T)
