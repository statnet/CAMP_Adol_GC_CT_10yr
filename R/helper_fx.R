
#############################################################
#'Make a matrix with 3 rows from a vector
#'
#' @param x A vector
#' @return A matrix with 3 rows containing the values from x filled in by row

#' @export
mat3 <- function(x) {
  matrix(x, nrow=3, byrow=TRUE)
}


#############################################################
#'Make a matrix with 2 rows from a vector
#'
#' @param x A vector
#' @return A matrix with 2 rows containing the values from x filled in by row
#' @export
mat2 <- function(x) {
  matrix(x, nrow=2, byrow=TRUE)
}

#############################################################
#'Make an array that repeats a matrix 11 times
#'
#' @param x A matrix
#' @return A matrix with 11 laters with each layer equaling x 
#' @export
array11 <- function(x) {
  array(rep(x,11), dim=c(dim(x), 11))
}
