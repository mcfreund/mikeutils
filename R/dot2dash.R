#' Substitute common characters.
#'
#' A wrapper for common string substitutions.
#' @param x character string
#' @keywords string manipulation, gsub
#' @examples

#' @export
dot2dash <- function(x) gsub("\\.", "-", x)
#' @export
dash2dot <- function(x) gsub("\\-", ".", x)
#' @export
dot2und <- function(x) gsub("\\.", "_", x)
#' @export
und2dot <- function(x) gsub("_", "\\.", x)
#' @export
dash2und <- function(x) gsub("\\-", "_", x)
#' @export
und2dash <- function(x) gsub("_", "\\-", x)
