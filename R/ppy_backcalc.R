
#############################################################
#'Function to back calculate PPY 
#'
#' @param popsizes a matrix of population sizes. 
#' The rows represent age at survey, and the columns represent age of debut. 
#' Cells above the main diagonal should contain NA; a warning will be produced if they do not.
#' @param lifeparts a matrix of mean lifetime partner counts. 
#' The rows represent age at survey, and the columns represent age of debut. 
#' Cells above the main diagonal should contain NA; a warning will be produced if they do not.
#' @param rowages a vector indicating the ages represented by the rows of popsizes and lifeparts
#' @param colages a vector indicating the ages represented by the columns of popsizes and lifeparts
#' @param returnages a vectr indicating the ages that should be included in the returned object
#' #' @return A vector of estimated new partners per year by age.
#' @export
ppy_backcalc <- function(popsizes, lifepops, rowages, colages, returnages) {
  nrowages <- length(rowages)
  ncolages <- length(colages)
  
  currage <- matrix(rowages[row(popsizes)], nrow=length(rowages))
  firstsexage <- matrix(colages[col(popsizes)], nrow=length(rowages))
  
  if (nrowages != nrow(popsizes)) stop("Length of rowages must equal the number of rows in popsizes.")
  if (nrowages != nrow(lifeparts)) stop("Length of rowages must equal the number of rows in lifeparts.")  
  if (ncolages != ncol(popsizes)) stop("Length of colages must equal the number of columns in popsizes.")
  if (ncolages != ncol(lifeparts)) stop("Length of colages must equal the number of columns in lifeparts.")  
  
  if(any(currage < firstsexage & !is.na(popsizes))) warning(
    "There is at least one cell in popsizes where with a non-NA entry where debut age is 
    greater than current age.")
  
  if(any(currage < firstsexage & !is.na(lifeparts))) warning(
    "There is at least one cell in lifeparts where with a non-NA entry where debut age is 
    greater than current age.")
  
  if(any(currage == firstsexage & !is.na(popsizes))) {
    warning("At least one cell in popsizes for which current age equals age at first sex contains non-NA values.
            This information is not used in the calculation of mean lifetime partner numbers by age.")
    popsizes[currage == firstsexage & !is.na(popsizes)] <- NA
  }
    
  if(any(currage == firstsexage & !is.na(lifeparts))) {
    warning("At least one cell in lifeparts for which current age equals age at first sex contains non-NA values.
            This information is not used in the calculation of mean lifetime partner numbers by age.")
    lifeparts[currage == firstsexage & !is.na(lifeparts)] <- NA
  }

  mppy <- lifeparts / (currage-firstsexage)  # Calculate mean partners per year for each cell entry (Matrix C in teen-SPARC worsheet ppy_calc)
  mppy.wt <- mppy*popsizes                   # Calculate mean partners per year weighted by popsize (Matrix D in teen-SPARC worsheet ppy_calc)
  mppy.wt.cum <- sapply(1:ncolages, function(x) rowSums(mppy.wt[,1:x, drop=FALSE])) # Calculate mean partners per year by age had, weighted by popsize (Matrix E in teen-SPARC worsheet ppy_calc)
  mppy.wt.cum.tot <- colSums(mppy.wt.cum, na.rm = TRUE) # Matrix E col sums
  pop.wt.cum <- sapply(1:ncolages, function(x) rowSums(popsizes[,1:x, drop=FALSE])) # Calculate cum num of people (tot wts) contributing to observations in Matrix E 
  pop.wt.cum.tot <- colSums(pop.wt.cum, na.rm = TRUE) # Matrix F col sums
  mnppy.by.age <- mppy.wt.cum.tot / pop.wt.cum.tot
  
  result <- list()
  result$ages <- returnages
  result$mnppy <- rep(NA,length(returnages))
  result$wts <- rep(0,length(returnages))
  for (i in 1:length(returnages)) {
    if (returnages[i] %in% colages) {
      result$mnppy[i] <- mnppy.by.age[which(colages==returnages[i])]
      result$wts[i] <- pop.wt.cum.tot[which(colages==returnages[i])]
    }
  }
  return(result)
  }
