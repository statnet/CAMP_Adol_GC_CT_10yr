
#### Results for paper "XXXX"

### Short names
fns <- a10_gc_nbc$n_inc_insch_f  # Female no behavior change, school
fcs <- a10_gc_obs$n_inc_insch_f    # etc.
fnt <- a10_gc_nbc$n_inc_total_f
fct <- a10_gc_obs$n_inc_total_f
mns <- a10_gc_nbc$n_inc_insch_m
mcs <- a10_gc_obs$n_inc_insch_m
mnt <- a10_gc_nbc$n_inc_total_m
mct <- a10_gc_obs$n_inc_total_m


###  Number of cases averted, by sex and ethn, no CIs

  round(matrix(c(                                      # in school
    sum(fns[ ,,11] - fcs[ ,,11], na.rm=TRUE), 
    sum(fns[1,,11] - fcs[1,,11], na.rm=TRUE), 
    sum(fns[2,,11] - fcs[2,,11], na.rm=TRUE), 
    sum(fns[3,,11] - fcs[3,,11], na.rm=TRUE), 
    sum(mns[ ,,11] - mcs[ ,,11], na.rm=TRUE), 
    sum(mns[1,,11] - mcs[1,,11], na.rm=TRUE), 
    sum(mns[2,,11] - mcs[2,,11], na.rm=TRUE), 
    sum(mns[3,,11] - mcs[3,,11], na.rm=TRUE)  
  ), 4,2), 1)
  
  round(matrix(c(                                      # total
    sum(fnt[ ,,11] - fct[ ,,11], na.rm=TRUE), 
    sum(fnt[1,,11] - fct[1,,11], na.rm=TRUE), 
    sum(fnt[2,,11] - fct[2,,11], na.rm=TRUE), 
    sum(fnt[3,,11] - fct[3,,11], na.rm=TRUE), 
    sum(mnt[ ,,11] - mct[ ,,11], na.rm=TRUE), 
    sum(mnt[1,,11] - mct[1,,11], na.rm=TRUE), 
    sum(mnt[2,,11] - mct[2,,11], na.rm=TRUE), 
    sum(mnt[3,,11] - mct[3,,11], na.rm=TRUE)  
  ), 4,2), 1)


######  % cases averted, by sex and ethn

  round(matrix(c(                                      # in school
    sum(fns[ ,,11] - fcs[ ,,11], na.rm=TRUE)/sum(fns[ ,,11]), 
    sum(fns[1,,11] - fcs[1,,11], na.rm=TRUE)/sum(fns[1,,11]), 
    sum(fns[2,,11] - fcs[2,,11], na.rm=TRUE)/sum(fns[2,,11]), 
    sum(fns[3,,11] - fcs[3,,11], na.rm=TRUE)/sum(fns[3,,11]), 
    sum(mns[ ,,11] - mcs[ ,,11], na.rm=TRUE)/sum(mns[ ,,11]), 
    sum(mns[1,,11] - mcs[1,,11], na.rm=TRUE)/sum(mns[1,,11]), 
    sum(mns[2,,11] - mcs[2,,11], na.rm=TRUE)/sum(mns[2,,11]), 
    sum(mns[3,,11] - mcs[3,,11], na.rm=TRUE)/sum(mns[3,,11])  
  ), 4,2), 3)
  
  round(matrix(c(                                      # total
    sum(fnt[ ,,11] - fct[ ,,11], na.rm=TRUE)/sum(fnt[ ,,11]), 
    sum(fnt[1,,11] - fct[1,,11], na.rm=TRUE)/sum(fnt[1,,11]), 
    sum(fnt[2,,11] - fct[2,,11], na.rm=TRUE)/sum(fnt[2,,11]), 
    sum(fnt[3,,11] - fct[3,,11], na.rm=TRUE)/sum(fnt[3,,11]), 
    sum(mnt[ ,,11] - mct[ ,,11], na.rm=TRUE)/sum(mnt[ ,,11]), 
    sum(mnt[1,,11] - mct[1,,11], na.rm=TRUE)/sum(mnt[1,,11]), 
    sum(mnt[2,,11] - mct[2,,11], na.rm=TRUE)/sum(mnt[2,,11]), 
    sum(mnt[3,,11] - mct[3,,11], na.rm=TRUE)/sum(mnt[3,,11])  
  ), 4,2), 3)
  

##### By year

asum <- function(x, y) apply(x, y, sum)

round(asum(fns[,,2:11], 3) - asum(fcs[,,2:11], 3), 1)
round(asum(fns[,,2:11], c(1,3)) - asum(fcs[,,2:11], c(1,3)), 1)
(asum(fns[,,2:11], 3) - asum(fcs[,,2:11], 3))/asum(fns[,,2:11], 3)
(asum(fns[,,2:11], c(1,3)) - asum(fcs[,,2:11], c(1,3)))/asum(fns[,,2:11], c(1,3))


##### By age





## race-specific year-specific PIAs      

aaa <- array(sapply(1:100, function(x) 
  apply(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,2:11], c(1,3), sum)), 
  dim=c(3,10,100))

bbb <- array(sapply(1:100, function(x) 
  apply(a10_gc_obs_100[[x]]$n_inc_insch_f[,,2:11], c(1,3), sum)), 
  dim=c(3,10,100))

ptest <- (asum(fns[,,2:11], c(1,3)) - asum(fcs[,,2:11], c(1,3)))/
          asum(fns[,,2:11], c(1,3))

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

aaa <- array(sapply(1:100, function(x) apply(a10_gc_nbc_100[[x]]$n_inc_insch_f[,,2:11], c(1,3), sum)), dim=c(3,10,100))
bbb <- array(sapply(1:100, function(x) apply(a10_gc_obs_100[[x]]$n_inc_insch_f[,,2:11], c(1,3), sum)), dim=c(3,10,100))

ptest <- (asum(fns[,,2:11], c(1,3)) - asum(fcs[,,2:11], c(1,3)))
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

