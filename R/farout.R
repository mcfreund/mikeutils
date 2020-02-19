#' Identifies "far" univariate outliers via boxplot method.
#'
#'
#'
#' @param x
#'
#'
#' @export

farout <- function(x) {

  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr3 <- IQR(x) * 3

  x < (q1 - iqr3) | x > (q3 + iqr3)

}
