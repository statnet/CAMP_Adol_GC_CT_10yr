
#############################################################
#'Function calculate initial prevalence 
#'  NA: to be run separately forby sex
#'
#' @param diags A 3x6 matrix of past-year diagnoses among
#' @param perc_diag A 3x6 matrix of precent of cases that get diagnosed
#' @param infdur A 3x6 matrix indicating mean dur of infection
#' @return A 3 x 6 matrix indicating est. number of prevalent cases in the cross-section

#' @export
initPrev_calc <- function(diags, perc_diag, infdur) {
  initPrev <- (diags/perc_diag)/infdur
  return(initPrev)
  }



