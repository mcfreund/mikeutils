#' Common vector math.
#'
#' @param v a numeric vector
#' @param m a numeric vector or matrix
#' @keywords scaling, normalizing, centering, linear algebra

#' @export
ssq <- function(v) sqrt(sum(v^2))

#' @export
scale2unit <- function(m) {
  if (is.null(dim(m))) {
    m / sqrt(sum(m^2))
  } else {
    sweep(m, 2, sqrt(colSums(m^2)), FUN = "/")
  }
}

#' @export
meancenter <- function(m) {
  if (is.null(dim(m))) {
    m - mean(m)
  } else {
    sweep(m, 2, colMeans(m), FUN = "-")
  }
}

#' @export
cosinesim <- function(v, w) c((v / sqrt(sum(v^2))) %*% (w / sqrt(sum(w^2))))
