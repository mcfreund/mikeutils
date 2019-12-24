#' Perform common operations on (dis)similarity matrices.
#'
#' A wrapper for the corrplot::corrplot() function with convenient defaults.
#' @param m a correlation matrix
#' @keywords correlation matrices, plotting
#' @examples

#' @export
oneminus <- function(x) 1 - x  ## helpful in pipes
#' @export
fliprows <- function(x) x[nrow(x):1, ]
#' @export
flipcols <- function(x) x[, ncol(x):1]
#' @export
mean.rzr <- function(v) tanh(mean(atanh(v)))  ## fisher's r-to-z
#' @export
rank.mat <- function(m) {
  m[] <- rank(m)
  m
}
#' @export
dist2mat <- function(m, ...) as.matrix(dist(t(m), ...))  ## transposed dist (wrapper for dist objects)


## data wrangling ----
#' @export
mat2vec <- function(m) {

  if (any(is.na(m))) stop("matrix contains NA values.")

  m[upper.tri(m, diag = TRUE)] <- NA
  data.table::melt(m, na.rm = TRUE)

}
