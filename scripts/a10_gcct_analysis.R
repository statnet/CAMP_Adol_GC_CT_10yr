
a10_gc01_averted_f <- a10_gc_nbc$n_diag_insch_f[,,2:11] - a10_gc01$n_diag_insch_f[,,2:11]
a10_gc01_averted_f_by_race_and_year <- apply(a10_gc01_averted_f, c(1,3), sum)
matplot(t(a10_gc01_averted_f_by_race_and_year), type='l')
a10_gc01_averted_f_by_race_and_year_p <- a10_gc01_averted_f_by_race_and_year / 
                                         apply(a10_gc_nbc$n_diag_insch_f[,,2:11], c(1,3), sum)
matplot(t(a10_gc01_averted_f_by_race_and_year_p), type='b', ylim=c(0,1))
matplot(t(pred_eversex_f_dyn[,6,]), type='l', ylim=c(0,1))


a10_gc01_averted_m <- a10_gc_nbc$n_diag_insch_m[,,2:11] - a10_gc01$n_diag_insch_m[,,2:11]
a10_gc01_averted_m_by_race_and_year <- apply(a10_gc01_averted_m, c(1,3), sum)
matplot(t(a10_gc01_averted_m_by_race_and_year), type='l')
a10_gc01_averted_m_by_race_and_year_p <- a10_gc01_averted_m_by_race_and_year / 
  apply(a10_gc_nbc$n_diag_insch_m[,,2:11], c(1,3), sum)
matplot(t(a10_gc01_averted_m_by_race_and_year_p), type='b', ylim=c(0,1))
matplot(t(pred_eversex_m_dyn[,6,]), type='l', ylim=c(0,1))

