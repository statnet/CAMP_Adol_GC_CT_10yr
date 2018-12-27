
################################################
# teen-SPARC 10-year extension master script
# 
# Notes: 
#   1. Change the setwd() line if needed


### Basics
setwd("C:/git/CAMP_10yr_proj/scripts/")
rm(list=ls())

# Get all inputs
source("a10_import.R")

# Set years info
years <- seq(2007, 2017, by=2)
nyears <- length(years)

##### TODO: Using total schoolpops from census for sure.  But:
#####       for race: we are currently using mean of weights across time 
#####                    - but this ignores that some of the total are not B/H/W
#####                 we can find out the wts for the other race/ethn groups, and subtract from census (yes)
#####                 or we could use the census pop numbers (no)
#####       for sex: currently we assume 50/50 but should redo with age/specific weights averaged across yrs.
#####

#### Get pop sizes by age/race/sex that are averaged across years
meanschoolpop <- mean(schoolpops$totschoolpop)                 # Tot pop size averaged across years
mean_pct_age_race <- apply(wts_f+wts_m, 1:2, mean) / 
          sum(apply(wts_f+wts_m, 1:2, mean))                   # Tot % age/race averaged across years

pct_f <- sum(wts_f) / (sum(wts_f) + sum(wts_m))                         # Tot % female averaged across years

n_f <- array11(mean_pct_age_race * pct_f * meanschoolpop)
n_m <- array11(mean_pct_age_race * (1-pct_f) * meanschoolpop)

n_f <- n_f[-4,,]
n_m <- n_m[-4,,]
wts_f <- wts_f[-4,,]
wts_m <- wts_m[-4,,]

#### Get eversex sizes

prop_eversex_f <- eversex_f / wts_f
prop_eversex_f[prop_eversex_f==Inf] <- 0
prop_eversex_f[is.nan(prop_eversex_f)] <- 0
prop_eversex_f_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(prop_eversex_f_df) <- c('ethn', 'age', 'year')
prop_eversex_f_df$prop_eversex <- as.vector(prop_eversex_f)
prop_eversex_f_df$wts <- as.vector(wts_f)
prop_eversex_f_df$agefac <- relevel(as.factor(prop_eversex_f_df$age), ref='16')
eversex_f_reg <- glm(prop_eversex ~ agefac + year + ethn + year*ethn,
                    data=prop_eversex_f_df, weights=wts, 
                    family="binomial")
pred_eversex_f <- array(predict(eversex_f_reg, type='response'), dim=c(3,6,6))

prop_eversex_m <- eversex_m / wts_m
prop_eversex_m[prop_eversex_m==Inf] <- 0
prop_eversex_m[is.nan(prop_eversex_m)] <- 0
prop_eversex_m_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(prop_eversex_m_df) <- c('ethn', 'age', 'year')
prop_eversex_m_df$prop_eversex <- as.vector(prop_eversex_m)
prop_eversex_m_df$wts <- as.vector(wts_m)
prop_eversex_m_df$agefac <- relevel(as.factor(prop_eversex_m_df$age), ref='16')
eversex_m_reg <- glm(prop_eversex ~ agefac + year + ethn + year*ethn,
                     data=prop_eversex_m_df, weights=wts, 
                     family="binomial")
pred_eversex_m_lo <- array(predict(eversex_m_reg, type='response'), dim=c(3,6,6))

if(F) {
  matplot(t(pred_eversex_f[,1,]),type='l', ylim=c(0,1))
  matplot(t(pred_eversex_f[,2,]),type='l', add=T)
  matplot(t(pred_eversex_f[,3,]),type='l', add=T)
  matplot(t(pred_eversex_f[,4,]),type='l', add=T)
  matplot(t(pred_eversex_f[,5,]),type='l', add=T)
  matplot(t(pred_eversex_f[,6,]),type='l', add=T)

  matplot(t(pred_eversex_m[,1,]),type='l', ylim=c(0,1))
  matplot(t(pred_eversex_m[,2,]),type='l', add=T)
  matplot(t(pred_eversex_m[,3,]),type='l', add=T)
  matplot(t(pred_eversex_m[,4,]),type='l', add=T)
  matplot(t(pred_eversex_m[,5,]),type='l', add=T)
  matplot(t(pred_eversex_m[,6,]),type='l', add=T)
}

################### Condom use

#### Get condomuse sizes

condom_f_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(condom_f_df) <- c('ethn', 'age', 'year')
condom_f_df$condom <- as.vector(condom_f)
condom_f_df$wts <- as.vector(condom_wts_f)
condom_f_df$agefac <- relevel(as.factor(condom_f_df$age), ref='16')
condom_f_reg <- glm(condom ~ agefac + year + ethn + year*ethn,
                     data=condom_f_df, weights=wts, 
                     family="binomial")
pred_condom_lo <- array(predict(condom_f_reg, type='response'), dim=c(3,6,6))

condom_m_df <- expand.grid(c('B','H','W'), 13:18, seq(2007,2017,2))
colnames(condom_m_df) <- c('ethn', 'age', 'year')
condom_m_df$condom <- as.vector(condom_m)
condom_m_df$wts <- as.vector(condom_wts_m)
condom_m_df$agefac <- relevel(as.factor(condom_m_df$age), ref='16')
condom_m_reg <- glm(condom ~ agefac + year + ethn + year*ethn,
                    data=condom_m_df, weights=wts, 
                    family="binomial")
pred_condom_m_lo <- array(predict(condom_m_reg, type='response'), dim=c(3,6,6))

if(F) {
  matplot(t(pred_condom_f[,1,]),type='l', ylim=c(0,1))
  matplot(t(pred_condom_f[,2,]),type='l', add=T)
  matplot(t(pred_condom_f[,3,]),type='l', add=T)
  matplot(t(pred_condom_f[,4,]),type='l', add=T)
  matplot(t(pred_condom_f[,5,]),type='l', add=T)
  matplot(t(pred_condom_f[,6,]),type='l', add=T)
  
  matplot(t(pred_condom_m[,1,]),type='l', ylim=c(0,1))
  matplot(t(pred_condom_m[,2,]),type='l', add=T)
  matplot(t(pred_condom_m[,3,]),type='l', add=T)
  matplot(t(pred_condom_m[,4,]),type='l', add=T)
  matplot(t(pred_condom_m[,5,]),type='l', add=T)
  matplot(t(pred_condom_m[,6,]),type='l', add=T)
}





if(F) {
  a10_gc01 <- a10(n_f=n_f, n_m=n_m,
                    init_sexdeb_f=init_sexdeb_f,
                    init_sexdeb_m=init_sexdeb_m,
#                    beta_m2f=beta_m2f,
#                    beta_f2m=beta_f2m,
                     prop_evesex_f=prop_evesex_f,
                     prop_evesex_m=prop_evesex_m
#                    coital_acts_pp_f=coital_acts_pp_f,
#                    coital_acts_pp_m=coital_acts_pp_m,
#                    condom_use_f=condom_use_f,
#                    condom_use_m=condom_use_m,
#                    ann_chg_npartners=ann_chg_npartners,
#                    ann_chg_coital=ann_chg_coital,
#                    ann_chg_condoms=ann_chg_condoms
  )
}

# Save image
save.image()

