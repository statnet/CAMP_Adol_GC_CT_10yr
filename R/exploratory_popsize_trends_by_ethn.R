
### Changes in pop sizes - do the changes in race composition reflect clear secular trends?

# In this first version we use absolute pop sizes - which might be a bit confusing since it combines
#     possible secular trends in race composition with non-secular trends in overall pop sizes

library(tidyverse)

years <- seq(2007, 2017, by=2)
nyears <- length(years)

abspopsizes_f <- wts_f
for (i in 1:nyears) {
  abspopsizes_f[,,i] <- abspopsizes_f[,,i]* 
                        unname(unlist(
                          (schoolpops %>% filter(year==years[i]) %>% select("totschoolpop"))
                        )) / 
                        sum(wts_f[,,i])
}
matplot(t(abspopsizes_f[1,,]), type='b') # Black girls by year, by age
matplot(t(abspopsizes_f[2,,]), type='b') # Hisp  girls by year, by age **strong increasing secular trends**
matplot(t(abspopsizes_f[3,,]), type='b') # White girls by year, by age ** mild decreasing secular trends **

abspopsizes_m <- wts_m
for (i in 1:nyears) {
  abspopsizes_m[,,i] <- abspopsizes_m[,,i]* 
    unname(unlist(
      (schoolpops %>% filter(year==years[i]) %>% select("totschoolpop"))
    )) / 
    sum(wts_m[,,i])
}
matplot(t(abspopsizes_m[1,,]), type='b') # Black boys by year, by age
matplot(t(abspopsizes_m[2,,]), type='b') # Hisp  boys by year, by age **strong increasing secular trends**
matplot(t(abspopsizes_m[3,,]), type='b') # White boys by year, by age ** mild decreasing secular trends **

# Significant trends?  Rows are BF, HF, WF, BM, HM, WF; cols are ages 13:18
matrix( c(
  sapply(1:6, function(x) summary(lm(t(abspopsizes_f[1,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(abspopsizes_f[2,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(abspopsizes_f[3,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(abspopsizes_m[1,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(abspopsizes_m[2,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(abspopsizes_m[3,,])~years))[[x]]$coefficients[2,4]) <0.05
  ),
  nrow=6, byrow=T)


# In this second version we use absolute pop sizes - which might be a bit confusing since it combines
#     possible secular trends in race composition with non-secular trends in overall pop sizes

relpopsizes_f <- wts_f
for (i in 1:nyears) {  # Could be replaced by apply but I'm just not feelin' it.
  relpopsizes_f[,,i] <- relpopsizes_f[,,i] / sum(wts_f[,,i])
}
matplot(t(relpopsizes_f[1,,]), type='b') # Black girls by year, by age
matplot(t(relpopsizes_f[2,,]), type='b') # Hisp  girls by year, by age **strong increasing secular trends**
matplot(t(relpopsizes_f[3,,]), type='b') # White girls by year, by age ** mild decreasing secular trends **

relpopsizes_m <- wts_m
for (i in 1:nyears) { # Could be replaced by apply but I'm just not feelin' it.
  relpopsizes_m[,,i] <- relpopsizes_m[,,i] / sum(wts_m[,,i])
}
matplot(t(relpopsizes_m[1,,]), type='b') # Black boys by year, by age
matplot(t(relpopsizes_m[2,,]), type='b') # Hisp  boys by year, by age **strong increasing secular trends**
matplot(t(relpopsizes_m[3,,]), type='b') # White boys by year, by age ** mild decreasing secular trends **

# Significant trends?  Rows are BF, HF, WF, BM, HM, WF; cols are ages 13:18
matrix( c(
  sapply(1:6, function(x) summary(lm(t(relpopsizes_f[1,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(relpopsizes_f[2,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(relpopsizes_f[3,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(relpopsizes_m[1,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(relpopsizes_m[2,,])~years))[[x]]$coefficients[2,4]) <0.05,
  sapply(1:6, function(x) summary(lm(t(relpopsizes_m[3,,])~years))[[x]]$coefficients[2,4]) <0.05
),
nrow=6, byrow=T)


###  All values averaged across years
par(mfrow=c(1,2))
matplot(t(apply(abspopsizes_f, 1:2, mean)), type='l')
matplot(t(apply(abspopsizes_m, 1:2, mean)), type='l')

popsize_f <- apply(abspopsizes_f, 1:2, mean)
popsize_m <- apply(abspopsizes_m, 1:2, mean)

