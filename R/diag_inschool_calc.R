
#############################################################
#'Function to convert all diagnoses to diagnoses in school 
#'  NA: to be run separately by sex
#'
#' @param XX A 3x6 matrix of past-year diagnoses among
#' @param XX A 3x6 matrix of precent of cases that get diagnosed
#' @param XX A 3x6 matrix indicating mean dur of infection
#' @return A 3 x 6 matrix indicating est. number of prevalent cases in the cross-section

#' @export
diag_inschool_calc <- function(diags, perc_diag, infdur) {
  initPrev <- (diags/perc_diag)/infdur
  return(initPrev)
  }



