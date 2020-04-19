#' Generates Mahalanobis spatial pre-whitening matrix.
#' Optionally regularizes matrix towards diagonal according to the Ledoit-Wolf method.
#' Depends: tawny package for Ledoit-Wolf method.
#'
#' @param E channel (e.g., vertex)-by-timepoint matrix of residuals.
#' @param shrinkage use Ledoit-Wolf method ("LW" or "ledoitwolf"), or manually specify lambda (e.g., 0.4).
#' @examples

#' @export

whitening <- function(E, shrinkage = "ledoitwolf") {

  S <- cov(E)  ## sample cov
  # corrplot::corrplot(cov2cor(S), method = "color")
  H <- diag(nrow(S))  ## target to shrink towards (Ledoit-Wolf's 'F')

  if (shrinkage %in% c("lw", "ledoitwolf", "LW"))  {
    k <- tawny::shrinkage.intensity(E, H, S)
    lambda <- max(c(0, min(k / nrow(E), 1)))  ## shrinkage factor
  } else lambda <- shrinkage

  S_hat <- lambda * H + (1 - lambda) * S  ## shrunken matrix

  W2 <- solve(S_hat)  ## mahalanobis whitening matrix^2

  list(W2 = W2, lambda = lambda)

}

