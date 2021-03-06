#' Quickly plot a correlation matrix.
#'
#' A wrapper for the corrplot::corrplot() function with convenient defaults.
#' @param m a correlation matrix
#' @keywords correlation matrices, plotting
#' @examples

#' @export
qcor <- function(m, .title = "", ...) corrplot::corrplot(m, method = "color", title = .title, mar = c(0, 0, 2, 0), ...)
