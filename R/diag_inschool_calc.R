
#############################################################
#'Function to convert all diagnoses to diagnoses in school 
#'  NA: to be run separately by sex
#'
#' @param diags A 3x6 matrix of past-year diagnoses among adols regardless of school status
#' @param perc_inschool A 3x6 matrix of precent of adols in HS
#' @return A 3x6 matrix indicating number of diagnoses among HS students

#' @export
diag_inschool_calc <- function(diags, perc_inschool) {
  diags_inschool <- diags*perc_inschool
  return(diags_inschool)
  }



