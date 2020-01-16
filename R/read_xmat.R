#' Read an AFNI gifti image and reshapes to matrix.
#'
#' Optionally extracts sub-bricks by labels.
#'
#' @param name name of .xmat.1D file to read
#' @param uncensored boolean: read an uncensored file? defaults to TRUE, so nrows corresponds to other GLM matrices (Y, E).
#' @keywords AFNI, neuroimaging

#' @export
read_xmat <- function(
  name,
  uncensored = TRUE
)
{
  ## TODO:
  ##  - input validation
  ##  - afni error checking (embed within X_temp?)

  ## get column names

  xlabels <- afni("1d_tool.py", paste0("-infile ", name, " -show_group_labels"))
  xlabels <- gsub("(.*) label (.*)", "\\2", xlabels)

  ## delete previously created files (afni will not overwrite)

  unlink("X_temp.1D")
  unlink("X_temp")

  ## get in format for R to read

  afni("1d_tool.py", paste0("-infile ", name, " -censor_fill -write X_temp.1D"))  ## write 1D file
  afni("1dcat", "-d X_temp.1D > X_temp")  ## write text file from 1D file for R to read

  X <- as.matrix(read.table("X_temp", quote = "\"", comment.char = ""))
  dimnames(X) <- list(tr = NULL, regressor = xlabels)

  unlink("X_temp.1D")
  unlink("X_temp")

  X

}
