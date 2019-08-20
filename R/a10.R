
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
#' @param p_ethn_f A 3x3 matrix indicating the proportion of females' partnerships that are with males of each ethn
#' @param p_ethn_m A 3x3 matrix indicating the proportion of males' partnerships that are with females of each ethn
#' @param diag_init_f A vector of length 3 or a 3x6 matrix, indicating the number of recent annual STI diagnoses among females, by race/eth (and optionally by age) -- in and out of school
#' @param diag_init_m A vector of length 3 or a 3x6 matrix, indicating the number of recent annual STI diagnoses among males, by race/eth (and optionally by age) -- in and out of school
#' @param prop_diag_f The proportion of females who get diagnosed for the STI
#' @param prop_diag_m The proportion of males who get diagnosed for the STI
#' @param dur_inf_f The average duration of infection for females
#' @param dur_inf_m The average duration of infection for males
#' @param beta_f2m Per-act transmission probability from female to male
#' @param beta_m2f Per-act transmission probability from male to female
#' @param meanpop_tot_f Total female population in and out of school across the relevant ages
#' @param meanpop_tot_m Total male population in and out of school across the relevant ages
#'
#' @return A list comprising two arrays of dimensions [3,6,12], containing the estimated number of
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
                p_ethn_f,
                p_ethn_m,
                diag_init_f,
                diag_init_m,
                prop_diag_f,
                prop_diag_m,
                dur_inf_f,
                dur_inf_m,
                beta_f2m,
                beta_m2f,
                meanpop_tot_f,
                meanpop_tot_m,
                part_prev_ratio_f,
                part_prev_ratio_m
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

  # Create arrays to store number of incident cases per year **in school8 and *total*
  n_inc_insch_f <- n_inc_insch_m <- n_inc_total_f <- n_inc_total_m <- array(dim=c(3,6,12))
  n_inc_insch_f[,,1] <- n_inc_insch_m[,,1] <- n_inc_total_f[,,1] <- n_inc_total_m[,,1] <- NA
  
  # Create arrays to store number of diagnoses per year *total* (in and out of HS)
  n_diag_total_f <- n_diag_total_m <- array(dim=c(3,6,12))
  if(is.matrix(diag_init_f) & sum(dim(diag_init_f)==c(3,6))==2) {
    n_diag_total_f[,,1] <- diag_init_f
    n_diag_total_m[,,1] <- diag_init_m
  } else n_diag_total_f[,,1] <- n_diag_total_m[,,1] <- NA

  # Create arrays to store number of diagnoses per year *in HS*
  n_diag_insch_f <- n_diag_insch_m <- array(dim=c(3,6,12))
  n_diag_insch_f[,,1] <- NA
  n_diag_insch_m[,,1] <- NA

  # Create arrays to store prevalence in the cross-section (value should be same in sch and tot)
  prev_f <- prev_m <- array(dim=c(3,6,12))
  
  if(is.vector(diag_init_f) & length(diag_init_f)==3) {
    ###prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f / rowSums(prop_eversex_f[,,1]*meanpop_tot_f[,,1])
    prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f / rowSums(meanpop_tot_f[,,1])
  } else {
    if(is.matrix(diag_init_f) & sum(dim(diag_init_f)==c(3,6))==2) {
      ###prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f / (prop_eversex_f[,,1]*meanpop_tot_f[,,1])
      prev_f[,,1] <- diag_init_f * dur_inf_f / prop_diag_f / (meanpop_tot_f[,,1])
    } else {
      stop("diag_init_f must be either a vector of length 3 or a matrix of dim (3,6).")
    }
  }
  
  if(is.vector(diag_init_f) & length(diag_init_f)==3) {
    ###prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m / rowSums(prop_eversex_m[,,1]*meanpop_tot_m[,,1])
    prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m / rowSums(meanpop_tot_m[,,1])
  } else {
    if(is.matrix(diag_init_f) & sum(dim(diag_init_f)==c(3,6))==2) {
      ###prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m / (prop_eversex_m[,,1]*meanpop_tot_m[,,1])
      prev_m[,,1] <- diag_init_m * dur_inf_m / prop_diag_m / (meanpop_tot_m[,,1])    } else {
      stop(" diag_init_m must be either a vector of length 3 or a matrix of dim (3,6).")
    }
  }

  # Calculate the number of condomless acts per person
  cl_acts_f <- mean_new_part_f * coital_acts_pp_f * (1-condom_use_f)
  cl_acts_m <- mean_new_part_m * coital_acts_pp_m * (1-condom_use_m)

  ##########################################################################
  # Advancement

  for (i in 2:12) {
    
    # Get weighted avg of prevalence in age range among those eversex, in or out of school 
    #  Differs from in school bc age population weights are different, even though age-specific prevs are the same.
    #  This is all needed to make consistent with the tool.
    
    ###overall_prev_f <- rowSums(prev_f[,,i-1]*meanpop_tot_f[,,i-1]*prop_eversex_f[,,i-1]) /   
    ###                            rowSums(meanpop_tot_f[,,i-1]*prop_eversex_f[,,i-1])
    ###overall_prev_m <- rowSums(prev_m[,,i-1]*meanpop_tot_m[,,i-1]*prop_eversex_m[,,i-1]) / 
    ###                            rowSums(meanpop_tot_m[,,i-1]*prop_eversex_m[,,i-1])
    
    overall_prev_f <- rowSums(prev_f[,,i-1]*meanpop_tot_f[,,i-1]) /   
                                rowSums(meanpop_tot_f[,,i-1])
    overall_prev_m <- rowSums(prev_m[,,i-1]*meanpop_tot_m[,,i-1]) / 
                                rowSums(meanpop_tot_m[,,i-1])

    n_inc_insch_f[,,i] <- (n_eversex_f[,,i-1]*(1-prev_f[,,i-1])) *           # Transm from BM
    ####n_inc_insch_f[,,i] <- (n_eversex_f[,,i-1]) *           # Transm from BM
                        (1-(1-overall_prev_m[1]*part_prev_ratio_f*beta_m2f)^(cl_acts_f[,,i-1]*p_ethn_f[,1])) + 

                        (n_eversex_f[,,i-1]*(1-prev_f[,,i-1])) *           # Transm from HM
                        ####(n_eversex_f[,,i-1]) *           # Transm from HM
                        (1-(1-overall_prev_m[2]*part_prev_ratio_f*beta_m2f)^(cl_acts_f[,,i-1]*p_ethn_f[,2])) + 

                        (n_eversex_f[,,i-1]*(1-prev_f[,,i-1])) *           # Transm from WM
                        ####(n_eversex_f[,,i-1]) *           # Transm from WM
                        (1-(1-overall_prev_m[3]*part_prev_ratio_f*beta_m2f)^(cl_acts_f[,,i-1]*p_ethn_f[,3]))  
      
    n_inc_insch_m[,,i] <- (n_eversex_m[,,i-1]*(1-prev_m[,,i-1])) *           # Transm from BF
    ####n_inc_insch_m[,,i] <- (n_eversex_m[,,i-1]) *           # Transm from BF
                        (1-(1-overall_prev_f[1]*part_prev_ratio_m*beta_f2m)^(cl_acts_m[,,i-1]*p_ethn_m[,1])) + 
                    
                        (n_eversex_m[,,i-1]*(1-prev_m[,,i-1])) *           # Transm from HF
                        ####(n_eversex_m[,,i-1]) *           # Transm from HF
                        (1-(1-overall_prev_f[2]*part_prev_ratio_m*beta_f2m)^(cl_acts_m[,,i-1]*p_ethn_m[,2])) + 
                    
                        (n_eversex_m[,,i-1]*(1-prev_m[,,i-1])) *           # Transm from WF
                        ####(n_eversex_m[,,i-1]) *           # Transm from WF
                        (1-(1-overall_prev_f[3]*part_prev_ratio_m*beta_f2m)^(cl_acts_m[,,i-1]*p_ethn_m[,3]))  

    n_diag_insch_f[,,i] <- n_inc_insch_f[,,i] * prop_diag_f
    n_diag_insch_m[,,i] <- n_inc_insch_m[,,i] * prop_diag_m
    
    n_inc_total_f[,,i] <- n_inc_insch_f[,,i] * meanpop_tot_f[,,1] / n_f[,,1]
    n_inc_total_m[,,i] <- n_inc_insch_m[,,i] * meanpop_tot_m[,,1] / n_m[,,1]
    n_diag_total_f[,,i] <- n_diag_insch_f[,,i] * meanpop_tot_f[,,1] / n_f[,,1]
    n_diag_total_m[,,i] <- n_diag_insch_m[,,i] * meanpop_tot_m[,,1] / n_m[,,1]

    #prev_f[,,i] <- n_inc_insch_f[,,i]*dur_inf_f / n_eversex_f[,,i]                # Old way with no aging
    #prev_m[,,i] <- n_inc_insch_m[,,i]*dur_inf_m / n_eversex_m[,,i]
    
    prev_f[,,i] <- n_inc_insch_f[,,i]*dur_inf_f / n_f[,,i-1]                        
    prev_m[,,i] <- n_inc_insch_m[,,i]*dur_inf_m / n_m[,,i-1]
    
    ##prev_f_num_temp <- n_inc_insch_f[,,i]*dur_inf_f                                # Prev number
    ##prev_m_num_temp <- n_inc_insch_m[,,i]*dur_inf_m 
  
    #prev_f_num_temp <- cbind(c(0,0,0), prev_f_num_temp[,1:5])                     # Advance one age
    #prev_m_num_temp <- cbind(c(0,0,0), prev_m_num_temp[,1:5])                     # Advance one age

    #aging_ratio_f <- n_eversex_f[,1:5,i-1]/n_eversex_f[,2:6,i]
    #aging_ratio_m <- n_eversex_m[,1:5,i-1]/n_eversex_m[,2:6,i]
    
    ##aging_ratio_f <- n_eversex_f[,,i-1]/n_eversex_f[,,i]
    ##aging_ratio_m <- n_eversex_m[,,i-1]/n_eversex_m[,,i]

    #denom_f <- n_eversex_f[,,i]                                                   # Growing pops (i.e. more 15 yos in Y1 than 14 yos in Y0) mean new people entering, who had not previously sexually devuted, so are all uninfected; they should be added to denominator
    #denom_m <- n_eversex_m[,,i]
    
    ##denom_f <- n_eversex_f[,,i]                                                    # Growing pops (i.e. more 15 yos in Y1 than 14 yos in Y0) mean new people entering, who had not previously sexually devuted, so are all uninfected; they should be added to denominator
    ##denom_m <- n_eversex_m[,,i]

    ##denom_f[aging_ratio_f>1] <- n_eversex_f[,,i-1][aging_ratio_f>1]                # Shrinking pops (i.e. fewer 18 yos in Y1 than 17 yos in Y0) mean people are leaving school through aging out or dropout; they *are* sexually experienced, and the prevalence rate calcuated on the old pop size should carry forward
    ##denom_m[aging_ratio_m>1] <- n_eversex_m[,,i-1][aging_ratio_m>1]      

    #prev_f[,,i] <- prev_f_num_temp / denom_f
    #prev_m[,,i] <- prev_m_num_temp / denom_m
    
  }


  ##########################################################################
  # Final processing

  result <- list(n_inc_insch_f = n_inc_insch_f,
                 n_inc_insch_m = n_inc_insch_m,
                 n_inc_total_f = n_inc_total_f,
                 n_inc_total_m = n_inc_total_m,
                 prev_f = prev_f,
                 prev_m = prev_m,
                 n_diag_insch_f = n_diag_insch_f,
                 n_diag_insch_m = n_diag_insch_m,
                 n_diag_total_f = n_diag_total_f,
                 n_diag_total_m = n_diag_total_m,
                 n_eversex_f = n_eversex_f,
                 n_eversex_m = n_eversex_m,
                 cl_acts_f = cl_acts_f,
                 cl_acts_m = cl_acts_m
  )
  return(result)
}
