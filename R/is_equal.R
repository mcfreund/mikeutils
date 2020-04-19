#' Vectorized test for numeric equivalence.
#' from: https://stackoverflow.com/questions/35097815/vectorized-equality-testing
#' @param x numeric vector
#' @param y numeric vector
#' @param tol tolerance. defaults to .Machine\$double.eps^0.5
#' @examples

#' @export

is_equal <- function(x, y, tol = .Machine$double.eps^0.5) {
  ## https://stackoverflow.com/questions/35097815/vectorized-equality-testing
  abs(x - y) < tol
}
