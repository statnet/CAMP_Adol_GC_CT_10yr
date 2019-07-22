#########################################################################
### Init diagnoses, total (in and out of school)

dx_gc_f[,1:2,1] <- dx_gc_10_14_f[,,1]*(meanpop_13to18_f[,1:2,1])/1e5
dx_gc_f[,3:6,1] <- dx_gc_15_19_f[,,1]*(meanpop_13to18_f[,3:6,1])/1e5
dx_gc_m[,1:2,1] <- dx_gc_10_14_m[,,1]*(meanpop_13to18_m[,1:2,1])/1e5
dx_gc_m[,3:6,1] <- dx_gc_15_19_m[,,1]*(meanpop_13to18_m[,3:6,1])/1e5

dx_gc_init_tot_f <- rowSums(dx_gc_f[,,1])
dx_gc_init_tot_m <- rowSums(dx_gc_m[,,1])

dx_ct_f[,1:2,1] <- dx_ct_10_14_f[,,1]*(meanpop_13to18_f[,1:2,1])/1e5
dx_ct_f[,3:6,1] <- dx_ct_15_19_f[,,1]*(meanpop_13to18_f[,3:6,1])/1e5
dx_ct_m[,1:2,1] <- dx_ct_10_14_m[,,1]*(meanpop_13to18_m[,1:2,1])/1e5
dx_ct_m[,3:6,1] <- dx_ct_15_19_m[,,1]*(meanpop_13to18_m[,3:6,1])/1e5

dx_ct_init_tot_f <- rowSums(dx_ct_f[,,1])
dx_ct_init_tot_m <- rowSums(dx_ct_m[,,1])

save(dx_gc_f, dx_gc_m, dx_ct_f, dx_ct_m, file='../output/a10_init_dx_nonage.rda')