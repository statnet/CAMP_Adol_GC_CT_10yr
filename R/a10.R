
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
#' @param diag_init_f A vector of length 3 indicating the number of recent annual STI diagnoses among females, by race/eth
#' @param diag_init_m A vector of length 3 indicating the number of recent annual STI diagnoses among males, by race/eth
#' @param prop_diag_f The proportion of females who get diagnosed for the STI
#' @param prop_diag_m The proportion of males who get diagnosed for the STI
#' @param dur_inf_f The average duration of infection for females
#' @param dur_inf_m The average duration of infection for males
#' @param beta_f2m Per-act transmission probability from female to male
#' @param beta_m2f Per-act transmission probability from male to female
#' @param meanpop_tot_f Total female population in and out of school across the relevant ages
#' @param meanpop_tot_m Total male population in and out of school across the relevant ages
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
                beta_m2f,
                meanpop_tot_f,
                meanpop_tot_m
        ) {

  ##################################################
  # Notes to self
  # Need to convert incidence to prevalence again
  # Need to incorporate race mixing
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

  # Create arrays to store number of incident cases per year
  n_inc_f <- n_inc_m <- array(dim=c(3,6,11))
  n_inc_f[,,1] <- NA
  n_inc_m[,,1] <- NA

  # Create arrays to store number of diagnoses per year *total* (in and out of HS)
  #n_diag_tot_f <- n_diag_tot_m <- array(dim=c(3,6,11))
  #n_diag_tot_f[,,1] <- diag_init_f
  #n_diag_tot_m[,,1] <- diag_init_m

  # Create arrays to store number of diagnoses per year *in HS*
  n_diag_f <- n_diag_m <- array(dim=c(3,6,11))
  n_diag_f[,,1] <- NA
  n_diag_m[,,1] <- NA

  # Create arrays to store prevalence in the cross-section
  prev_f <- prev_m <- array(dim=c(3,6,11))
  prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f / rowSums(prop_eversex_f[,,1]*meanpop_tot_f[,,1])
  prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m / rowSums(prop_eversex_m[,,1]*meanpop_tot_m[,,1])

  # Calculate the number of condomless acts per person
  cl_acts_f <- mean_new_part_f * coital_acts_pp_f * (1-condom_use_f)
  cl_acts_m <- mean_new_part_m * coital_acts_pp_m * (1-condom_use_m)

  ##########################################################################
  # Advancement

  for (i in 2:11) {
    
    # Get weighted avg of prevalence in age range among those eversex, in or out of school 
    #  Differs from in school bc age population weights are different, even though age-specific prevs are the same.
    #  This is all needed to make consistent with the tool.
    overall_prev_f <- rowSums(prev_f[,,i-1]*meanpop_tot_f[,,i-1]*prop_eversex_f[,,i-1]) /   
                                rowSums(meanpop_tot_f[,,i-1]*prop_eversex_f[,,i-1])
    overall_prev_m <- rowSums(prev_m[,,i-1]*meanpop_tot_m[,,i-1]*prop_eversex_m[,,i-1]) / 
                                rowSums(meanpop_tot_m[,,i-1]*prop_eversex_m[,,i-1])
    
    n_inc_f[,,i] <- (n_eversex_f[,,i-1]*(1-prev_f[,,i-1])) *          # Number suscep F
                    (1-(1-overall_prev_m*beta_m2f)^cl_acts_f[,,i-1])  # Prob per suscep F

    n_inc_m[,,i] <- (n_eversex_m[,,i-1]*(1-prev_m[,,i-1])) *          # Number suscep F
                    (1-(1-overall_prev_f*beta_f2m)^cl_acts_m[,,i-1])  # Prob per suscep F

    n_diag_f[,,i] <- n_inc_f[,,i] * prop_diag_f
    n_diag_m[,,i] <- n_inc_m[,,i] * prop_diag_m

    prev_f[,,i] <- n_inc_f[,,i]*dur_inf_f / n_eversex_f[,,i]
    prev_m[,,i] <- n_inc_m[,,i]*dur_inf_m / n_eversex_m[,,i]
  }


  ##########################################################################
  # Final processing

  result <- list(n_inc_f = n_inc_f,
                 n_inc_m = n_inc_m,
                 prev_f = prev_f,
                 prev_m = prev_m,
                 n_diag_f = n_diag_f,
                 n_diag_m = n_diag_m,
                 n_eversex_f = n_eversex_f,
                 n_eversex_m = n_eversex_m
  )
  return(result)
}
