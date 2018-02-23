
#############################################################
#'Make a matrix with 3 rows from a vector
#'
#' @param x A vector
#' @return A matrix with 3 rows containing the values from x filled in by row

#' @export
mat3 <- function(x) {
  matrix(x, nrow=3, byrow=TRUE)
}