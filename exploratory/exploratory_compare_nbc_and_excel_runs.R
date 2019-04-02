
source('C:/git/CAMP_10yr_proj/scripts/a10_no_behav_change_script.R', echo=TRUE)
save(a10_gc_nbc, file='a10_gc_nbc.rda')

inc_yr_f_nbc <- apply(a10_gc_nbc$n_inc_f,3,sum)
inc_yr_m_nbc <- apply(a10_gc_nbc$n_inc_m,3,sum)

par(mfrow=c(1,2))
plot(inc_yr_f_nbc, ylim=c(0, max(inc_yr_f_nbc, na.rm=TRUE)*1.2), type='l')
lines(inc_yr_m_nbc, col='blue')

source('C:/git/CAMP_10yr_proj/scripts/a10__comp_with_excel_gc.R', echo=TRUE)
save(a10_gc_sync, file='a10_gc_sync.rda')

inc_yr_f_exc <- apply(a10_gc_sync$n_inc_f,3,sum, na.rm=TRUE)
inc_yr_m_exc <- apply(a10_gc_sync$n_inc_m,3,sum, na.rm=TRUE)

plot(inc_yr_f_exc, ylim=c(0, max(inc_yr_f_exc, na.rm=TRUE)*1.2), type='l')
lines(inc_yr_m_exc, col='blue')



############################

rm(list=ls())
load('a10_gc_nbc.rda')
load('a10_gc_sync.rda')
 
plot(a10_gc_sync$prev_f[1,6,], ylim=c(0,0.05))
points(a10_gc_nbc$prev_f[1,6,])
