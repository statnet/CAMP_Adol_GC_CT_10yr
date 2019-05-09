
rm(list=ls())
load('a10_gc_nbc.rda')
load('a10_gc01.rda')

diag_total_by_yr_and_race_f_nbc <- apply(a10_gc_nbc$n_diag_total_f, c(1,3), sum)
diag_total_by_yr_and_race_m_nbc <- apply(a10_gc_nbc$n_diag_total_m, c(1,3), sum)
diag_total_by_yr_and_race_f_01 <- apply(a10_gc01$n_diag_total_f, c(1,3), sum)
diag_total_by_yr_and_race_m_01 <- apply(a10_gc01$n_diag_total_m, c(1,3), sum)


matplot(t(diag_total_by_yr_and_race_f_nbc), 
        ylim=c(0,max(diag_total_by_yr_and_race_f_nbc,na.rm=TRUE)*1.2), type='b')
matplot(t(diag_total_by_yr_and_race_f_01), 
        type='b', lty=2, add=TRUE)
        
matplot(t(diag_total_by_yr_and_race_f_01/diag_total_by_yr_and_race_f_nbc))

matplot(t(diag_total_by_yr_and_race_m), type='b', add=TRUE, pch=4:6)

