
#############################################################
#'Main a10 function
#'
#' @param n_f A 3x6x11 array indicating the population size for females, by race/eth by age by year
#' @param n_m A 3x6x11 array indicating the population size for males, by race/eth by age by year
#' @param prop_eversex_f A 3x6x11 matrix inidicating the proportion of females debuted (as FSM), by race/eth by age by year
#' @param prop_eversex_m A 3x6x11 matrix inidicating the proportion of males debuted (as MSF), by race/eth by age by year
#' @param condom_use_f, A 3x6x11 matrix indicating % condom use for females, by race/eth by age by year
#' @param condom_use_m, A 3x6x11 matrix indicating % condom use for males, by race/eth by age by year
#' @param mean_new_part_f A 3x6x11 matrix indicating the mean new partners per year for debuted females, by race/eth by age
#' @param mean_new_part_m A 3x6x11 matrix indicating the mean new partners per year for debuted males, by race/eth by age
#' @param coital_acts_pp_f A 3x6x11 matrix indicating the mean coital acts per partner for females, by race/eth by age
#' @param coital_acts_pp_m A 3x6x11 matrix indicating the mean coital acts per partner for males, by race/eth by age
#' @param diag_init_f A 3x6 matrix indicating the number of recent annual STI diagnoses among HS females, by race/eth by age
#' @param diag_init_m A 3x6 matrix indicating the number of recent annual STI diagnoses among HS males, by race/eth by age
#' @param prop_diag_f The proportion of females who get diagnosed for the STI
#' @param prop_diag_m The proportion of males who get diagnosed for the STI
#' @param dur_inf_f The average duration of infection for females
#' @param dur_inf_m The average duration of infection for males
#' @param beta_f2m Per-act transmission probability from female to male
#' @param beta_m2f Per-act transmission probability from male to female
#' 
#' @return A list comprising two arrays of dimensions [3,6,11], containing the estimated number of 
#'   incident cases of STI per age per year. The three rows represent the 
#'   three race/ethnicity groups (B, H, W); the six columns represent the ages (13:18);
#'   the 11 layers represent the years (baseline, years 1:10). The first array in the list is F,
#'   and the second M.

#' @export
a10 <- function(n_f, n_m, 
                prop_eversex_f,
                prop_eversex_m,
                condom_use_f,
                condom_use_m,
                mean_new_part_f,
                mean_new_part_m,
                coital_acts_pp_f,
                coital_acts_pp_m,
                diag_init_f,
                diag_init_m,
                prop_diag_f,
                prop_diag_m,
                dur_inf_f,
                dur_inf_m,
                beta_f2m,
                beta_m2f
        ) {

  ##################################################
  # Notes to self
  # Need to convert incidence to prevalence again
  # Need to incorporate race mixing
  # Need to figure out how to include sex in last 3 mos
  # Need to decide on model calibration
  
  ##########################################################################
  # Dimensional error checking
  if (sum(dim(n_f) == c(3,6,11)) <3) stop("n_f must be an array with dimensions c(3,6,11).")
  if (sum(dim(n_f) == c(3,6,11)) <3) stop("n_m must be an array with dimensions c(3,6,11).")
  # TODO
  
  ##########################################################################
  # Init bookkeeping
  
  # Calc # who have sexually debuted
  n_eversex_f <- n_f * prop_eversex_f
  n_eversex_m <- n_m * prop_eversex_m
  
  # Create arrays to store partner prevalence per year
  part_prev_f <- part_prev_m <- array(dim=c(3,6,10))

  # Create arrays to store number of incident cases per year
  n_inc_f <- n_inc_m <- array(dim=c(3,6,10))
  n_inc_f[,,1] <- NA
  n_inc_m[,,1] <- NA

  # Create arrays to store number of diagnoses per year
  n_diag_f <- n_diag_m <- array(dim=c(3,6,11))
  n_diag_f[,,1] <- diag_init_f
  n_diag_m[,,1] <- diag_init_m

  # Create arrays to store number of prevalent cases in the cross-section
  n_prev_f <- n_prev_m <- array(dim=c(3,6,11))
  n_prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f
  n_prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m
  
  # Calculate the number of condomless acts per person
  cl_acts_f <- mean_new_part_f * coital_acts_pp_f * (1-condom_use_f)
  cl_acts_m <- mean_new_part_m * coital_acts_pp_m * (1-condom_use_m)
  
  ##########################################################################
  # Advancement

  for (i in 1:10) {  

    part_prev_f[,,i] <- matrix(rep(rowSums(n_prev_m[,,i])/rowSums(n_eversex_m[,,i]),6),
                          nrow=3, byrow=FALSE)
    
    part_prev_m[,,i] <- matrix(rep(rowSums(n_prev_f[,,i])/rowSums(n_eversex_f[,,i]),6),
                          nrow=3, byrow=FALSE)
    
    n_inc_f[,,i] <- (n_eversex_f[,,i] - n_prev_f[,,i]) * part_prev_f[,,i] *
                        cl_acts_f[,,i] * beta_m2f 

    n_inc_m[,,i] <- (n_eversex_m[,,i] - n_prev_m[,,i]) * part_prev_m[,,i] *
                        cl_acts_m[,,i] * beta_f2m

    n_prev_f[,,i+1] <- n_inc_f[,,i] * dur_inf_f
    n_prev_m[,,i+1] <- n_inc_m[,,i] * dur_inf_m
  
    n_diag_f[,,i+1] <- n_inc_f[,,i] * prop_diag_f
    n_diag_m[,,i+1] <- n_inc_m[,,i] * prop_diag_m
    
    }
  
  
  ##########################################################################
  # Final processing

  result <- list(n_inc_f, n_inc_m, n_prev_f, n_prev_m, n_diag_f, n_diag_m,
                 n_eversex_f, n_eversex_m               
  )
  return(result)
}
