#' A wrapper for issuing AFNI commands to terminal.
#'
#' Depends: If on a windows platform, will require WSL.
#'
#'
#' @param .fun AFNI function to call
#' @param .args a character vector of length 1 with all commands following function call
#' @param afni.path Optional; location of AFNI on machine. If not specified, will guess path based on `Sys.info()["nodename"]`
#' @keywords AFNI, neuroimaging
#' @examples

#' @export

afni <- function(.fun, .args, afni.path, ...) {

  ## use windows subsystem linux?

  if (.Platform$OS.type == "windows") {
    use.wsl <- TRUE
  } else if (.Platform$OS.type == "unix") {
    use.wsl <- FALSE
  } else {stop("Unknown OS")}

  ## guess AFNI path if not specified

  if (missing(afni.path)) {

    nodename <- Sys.info()["nodename"]

    if (nodename == "ccplinux1") {
      afni.path <- "/usr/local/pkg/linux_openmp_64/"
    } else if (nodename == "CCP-FREUND") {
      afni.path <- "/home/mcf/abin/"
    } else {stop("Unknown AFNI path. (Add nodename to function or specify 'manually'.)")}

  }

  ## execute

  if (use.wsl) {
    system2(
      command = "wsl",
      args    = paste(afni.path, .fun, " ", .args),
      stdout  = TRUE,
      ...
    )
  } else {
    system2(
      command = paste0(afni.path, .fun),
      args    = .args,
      stdout  = TRUE,
      ...
    )
  }

}
