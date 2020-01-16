#' Reads a gifti image and reshapes to matrix.
#'
#' A short wrapper for gifti::read_gifti().
#'
#' @param name name of .gii file to read
#' @keywords AFNI, neuroimaging, gifti
#' @export
read_gifti2matrix  <- function(name){

  d <- gifti::read_gifti(name)$data
  matrix(unlist(d, use.names = FALSE), nrow = length(d), byrow = TRUE)  ## (use.names == FALSE for speed)

}

