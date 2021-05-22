#' Computes cross-validated Mahalanobis distance.
#'
#'
#'
#' @param x a data matrix, conditions as columns
#' @param m optional; a contrast matrix, e.g., created with `contrast_matrix()`
#' @param W optional; a whitening matrix
#' @keywords RSA
#' @examples

#' @export

cvdist <- function(x, m = NULL, condition.names = NULL, W = NULL) {
  # x = U_unit[, , tr.i, ]

  ## NB: currently configured only for 2 folds!

  ## input validation

  if (is.array(x)) {

    if (length(dim(x)) != 3) stop("x must be 3D array (vertex, condition, fold)")

  } else {

    stop("x must be 3D array (vertex, condition, fold)")

  }

  if (is.null(m)) m <- mikeutils::contrast_matrix(dim(x)[2], condition.names = condition.names)


  ## compute

  x1 <- x[, , 1]
  x2 <- x[, , 2]

  if (is.null(W)) {

    g <- rowMeans(tcrossprod(m, x1) * tcrossprod(m, x2))  ## means to scale by num verts

  } else {

    g <- rowMeans(tcrossprod(m, W %*% x1) * tcrossprod(m, x2))  ## means to scale by num verts

  }


  dim(g) <- sqrt(c(length(g), length(g)))
  dimnames(g) <- list(colnames(m), colnames(m))

  g

}
