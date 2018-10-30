
#############################################################
#'Main a10 function
#'
#' @param n_f A 3x6x11 array indicating the population size for females, by race/eth by age by year
#' @param n_m A 3x6x11 array indicating the population size for males, by race/eth by age by year
#' @param sexdeb_f A 3x6x11 matrix inidicating the % of females debuted (as FSM), by race/eth by age by year
#' @param sexdeb_m A 3x6x11 matrix inidicating the % of males debuted (as MSF), by race/eth by age by year
#' @param beta_f2m Per-act transmission probability from female to male
#' @param beta_m2f Per-act transmission probability from male to female
#' @param init_prev_f A 3x6 matrix indicating the initial STI prevalence among females who have sexually debuted, by race/eth by age
#' @param init_prev_m A 3x6 matrix indicating the initial STI prevalence among males who have sexually debuted, by race/eth by age
#' @param newppy_f A 3x6 matrix indicating the mean new partners per year for debuted females, by race/eth by age
#' @param newppy_m A 3x6 matrix indicating the mean new partners per year for debuted males, by race/eth by age
#' @param coital_acts_pp_f A 3x6 matrix indicating the mean coital acts per partner for females, by race/eth by age
#' @param coital_acts_pp_m A 3x6 matrix indicating the mean coital acts per partner for males, by race/eth by age
#' @param condom_use_f, A 3x6x11 matrix indicating % condom use for females, by race/eth by age by year
#' @param condom_use_m, A 3x6x11 matrix indicating % condom use for males, by race/eth by age by year
#' 
#' @return A list comprising two arrays of dimensions [3,6,11], containing the estimated number of 
#'   incident cases of STI per age per year. The three rows represent the 
#'   three race/ethnicity groups (B, H, W); the six columns represent the ages (13:18);
#'   the 11 layers represent the years (baseline, years 1:10). The first array in the list is F,
#'   and the second M.

#' @export
a10 <- function(n_f, n_m, 
                sexdeb_f, sexdeb_m,
                init_prev_f, init_prev_m,
                beta_m2f, beta_f2m,
                newppy_f, newppy_m,
                coital_acts_pp_f, coital_acts_pp_m,

                                condom_use_f, condom_use_m
                #ann_chg_npartners, ann_chg_coital, ann_chg_condoms
       ) {
  ################################################
  # Notes to self
  # Will need to convert incidence to prevalence again
  # Will need to incprorate race mixing
  # Need to figure out how to include sex in last 3 mos
  # Need to decide on model calibration
  # Need to build ppy converter following tool method, or do something different
  # Need to redo all tool parameters by race/eth
  
  ##########################################################################
  # Dimensional error checking
  if (dim(n_f) != c(3,6,11)) stop("n_f must be an array with dimensions c(3,6,11).")
  if (dim(n_m) != c(3,6,11)) stop("n_m must be an array with dimensions c(3,6,11).")
  # More to do.
  
  ##########################################################################
  # Init bookkeeping
  
  # Calc # who have seuxally debuted
  n_sexdeb_f <- n_f * sexdeb_f
  n_sexdeb_m <- n_m * sexdeb_m
  
  # Create arrays to store number of prevalent cases in the cross-section
  n_prev_f <- n_prev_m <- array(dim=c(3,6,11))
  n_prev_f[,,1] <- n_sexdeb_f[,,1] * init_prev_f
  n_prev_m[,,1] <- n_sexdeb_m[,,1] * init_prev_m
  
  # Create arrays to store number of incident cases per year
  n_inc_f <- n_inc_m <- array(dim=c(3,6,11))
  n_inc_f[,,1] <- NA
  n_inc_m[,,1] <- NA

    #
  cl_acts_f <- newppy_f * coital_acts_pp_f * (1-condom_use_f)
  cl_acts_m <- newppy_m * coital_acts_pp_m * (1-condom_use_m)
  
  ##########################################################################
  # Advancement
  
  for (i in 1:10) {
    # Sexually debuted
    #nf_sexdeb[,, i+1] <- cbind(0,nf_sexdeb[, 1:5, i]) + nf[,,1]*pc_debuting_f
    #nm_sexdeb[,, i+1] <- cbind(0,nm_sexdeb[, 1:5, i]) + nm[,,1]*pc_debuting_m
  
  inc_f[,,i+1] <- cl_acts_f[,,i+1] * prev_m[,,i]
    
  }
  
  
  ##########################################################################
  # Final processing

  result <- nf_sexdeb
  return(result)
}
