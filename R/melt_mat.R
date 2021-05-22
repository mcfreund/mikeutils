#' Reshapes matrix to long-form dataframe for use with `ggplot2()`.
#'
#'
#'
#' @param R matrix
#' @param var.names
#' @param val.name
#' @keywords RSA
#' @examples

#' @export



melt_mat <- function(R, var.names = c("v1", "v2"), val.name = "value") {

  ## make factors for row and column labels
  dn <- dimnames(R)
  if (is.null(dn)) {
    dn <- setNames(list(paste0("cell_", 1:nrow(R)), paste0("cell_", 1:ncol(R))), var.names)
  } else {
    names(dn) <- var.names
  }

  labels <- expand.grid(dn, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = TRUE)
  labels[[2]] <- factor(labels[[2]], levels = rev(levels(labels[[2]])))

  r <- c(R)

  cbind(labels, setNames(as.data.frame(c(R)), val.name))

}

