
#############################################################
#'Main a10 function
#'
#' @param x A vector
#' @return A matrix with 3 rows containing the values from x filled in by row

#' @export
a10 <- function(nf_init, nm_init, sexdeb_init_f, sexdeb_init_m,
                init_prev_f, init_prev_m,
                beta_m2f, beta_f2m, pc_debuting_f, pc_debuting_m,
                coital_acts_pp_f, coital_acts_pp_m,
                condom_use_f, condom_use_m,
                ann_chg_npartners, ann_chg_coital, ann_chg_condoms
       ) {

  ##########################################################################
  # Init bookkeeping
  
  nf <- nm <- nf_sexdeb <- nm_sexdeb <- nf_inf <- nm_inf <- array(dim=c(3,6,11))

  nf[,,1] <- nf_init
  nm[,,1] <- nm_init
  
  nf_sexdeb[,,1] <- nf[,,1] * sexdeb_init_f
  nm_sexdeb[,,1] <- nm[,,1] * sexdeb_init_m
  
  nf_inf[,,1] <- nf[,,1] * init_prev_f
  nf_inf[,,1] <- nf[,,1] * init_prev_m
  
  ##########################################################################
  # Advancement
  
  for (i in 1:10) {
    # Sexually debuted
    nf_sexdeb[,, i+1] <- cbind(0,nf_sexdeb[, 1:5, i]) + nf[,,1]*pc_debuting_f
    nm_sexdeb[,, i+1] <- cbind(0,nm_sexdeb[, 1:5, i]) + nm[,,1]*pc_debuting_m
  }
  
  
  ##########################################################################
  # Final processing

  result <- nf_sexdeb
  return(result)
}
