#' Read an AFNI gifti image and reshapes to matrix.
#'
#' Optionally extracts sub-bricks by labels.
#'
#' @param name name of .gii file to read
#' @param xlabels optional; a character vector that indicates the exact strings of the sub-bricks to be extracted.
#' If omitted, all sub-bricks are extracted.
#' @param space the surface space the .gii is in. %in% c("hcp", "fsave")
#' @keywords AFNI, neuroimaging, gifti
#' @examples

#' @export


collate_surface_params <- function(
  name,
  space = "hcp",
  xlabels = NULL
){

  # xlabels <- c(
  #   ## intercept and baseline drift
  #   "Run#1Pol#0", "Run#1Pol#1", "Run#1Pol#2", "Run#1Pol#3", "Run#1Pol#4", "Run#1Pol#5",
  #   ## sustained
  #   "block#0",
  #   ## transient
  #   "blockONandOFF#0", "blockONandOFF#1", "blockONandOFF#2", "blockONandOFF#3", "blockONandOFF#4", "blockONandOFF#5",
  #   ## events
  #   "AX#0", "AX#1", "AX#2", "AX#3", "AX#4", "AX#5", "AX#6", "AX#7",
  #   "AY#0", "AY#1", "AY#2", "AY#3", "AY#4", "AY#5", "AY#6", "AY#7",
  #   "Ang#0", "Ang#1", "Ang#2", "Ang#3", "Ang#4", "Ang#5", "Ang#6", "Ang#7",
  #   "BX#0", "BX#1", "BX#2", "BX#3", "BX#4", "BX#5", "BX#6", "BX#7",
  #   "BY#0", "BY#1", "BY#2", "BY#3", "BY#4", "BY#5", "BY#6", "BY#7",
  #   "Bng#0", "Bng#1", "Bng#2", "Bng#3", "Bng#4", "Bng#5", "Bng#6", "Bng#7",
  #   ## movement
  #   "movregs[0]#0", "movregs[1]#0", "movregs[2]#0", "movregs[3]#0", "movregs[4]#0", "movregs[5]#0"
  # )
  # name <- file.path(
  #   "/scratch1/witzermanm/AFNI_ANALYSIS_SUBSUBJECT/RESULTS_RUNWISE",
  #   "204319", "Axcpt", "baseline",
  #   "baseline_Cues_EVENTS_censored_run1",
  #   paste0("betas_204319_L.func.gii")
  # )
  # space <- "hcp"

  ## TODO
  ##  - input validation
  ##  - afni output checks
  ##  - return gifti option

  ## input validation
  ## ...

  ## get number of vertices

  if (space == "hcp") {
    n.vertices <- 32492
  } else if (space == "fsave") {
    n.vertices <- 10242
  } else {
    stop("!space %in% c('hcp', 'fsave')")
  }

  xlabels.actual <- afni("3dinfo", paste0("-label ", name))  ## TODO: some form of error checking here...
  xlabels.actual <- unlist(strsplit(xlabels.actual, "\\|"))

  if (is.null(xlabels)) {
    ## if xlabels missing, use labels.subbricks to extract params (i.e., extract all params):
    xlabels <- xlabels.actual
    xinds <- seq_len(length(xlabels))
  } else {
    ## else match xlabels to labels.subbricks:
    if (any(!xlabels %in% xlabels.actual)) stop("any(!xlabels %in% labels.subbricks)")
    xinds <- which(xlabels %in% xlabels.actual)
  }

  ## read, extract, and reshape to matrix

  gii <- gifti::read_gifti(name)
  d <- gii$data[xinds]
  m <- matrix(unlist(d, use.names = FALSE), nrow = length(d), byrow = TRUE)  ## (use.names == FALSE for speed)

  ## check dims & add dimnames

  n.params <- length(xinds)
  if (!all(dim(m) == c(n.params, n.vertices))) stop("dims not expected!")
  dimnames(m) <- list(param = xlabels[xinds], vertex = NULL)

  m

}
