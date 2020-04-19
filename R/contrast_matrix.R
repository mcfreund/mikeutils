#' Generates contrast matrix for forming cross-validated euclidean distance matrix.
#'
#' @param n number of conditions
#' @param condition.names optional
#' @examples

#' @export

contrast_matrix <- function(n, condition.names) {
  # n <- 10
  # condition.names <- letters[1:n]

  if (n < 2) stop("you need more than 1 condition you dummy")

  W <- matrix(0, nrow = n^2, ncol = n)

  if (missing(condition.names)) {
    dimnames(W) <- list(contrast = NULL, condition = NULL)
  } else {
    dimnames(W) <- list(
      contrast = paste0(rep(condition.names, each = n), "_", rep(condition.names, n)),
      condition = condition.names
    )
  }

  for (condition.i in seq_len(n)) {
    # condition.i = 1

    row.beg <- (condition.i - 1) * n + 1
    row.end <- (condition.i - 1) * n + n
    W.i <- W[row.beg:row.end, ]  ## square matrix; the contrasts that define a column of the similarity matrix

    W.i[, condition.i] <- 1  ## the condition to which all others are contrasted
    diag(W.i) <- diag(W.i) - 1  ## all others

    W[row.beg:row.end, ] <- W.i

  }

  W

}

