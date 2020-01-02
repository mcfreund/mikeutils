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
mat2vec <- function(m, strings = TRUE, full.matrix = FALSE) {

  if (any(is.na(m))) stop("matrix contains NA values.")
  if (!full.matrix) m[upper.tri(m, diag = TRUE)] <- NA

  d <- reshape2::melt(m, na.rm = TRUE)

  if (strings) {
    i <- sapply(d, is.factor)
    d[i] <- lapply(d[i], as.character)
  }

  d

}

#' @export
vec2mat <- function(v, dnames, diag.val = 1) {

  ## dimension of square matrix from num elements in upper triangle (excluding diag):
  ## (http://blog.phytools.org/2013/06/upper-triangle-of-matrix-to-vector-by.html)
  d <- (sqrt(8 * length(v) + 1) + 1) / 2

  m <- diag(d)
  diag(m) <- diag.val
  colnames(m) <- dnames
  rownames(m) <- dnames

  m[lower.tri(m, diag = FALSE)] <- v
  m <- t(m)
  m[lower.tri(m, diag = FALSE)] <- v

  m

}
