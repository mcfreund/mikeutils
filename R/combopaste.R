#' Paste all combinations of elements of a set of strings.
#'
#' @param a (character) vector 1, left-hand side of output
#' @param b (character) vector 2, right-hand side of output
#' @param sep a character string to separate the results
#' @keywords string manipulation, concatenating strings
#' @examples
#' combo.paste(letters[1:2], letters[3:4])  ##
#' combo.paste(letters[1:2], letters[3:4], sep = "_")
#' combo_paste(letters[1:2], letters[3:4])  ## some common options for sep can be specified via the the function name
#' combopaste3(letters[1:2], letters[3:4], letters[24:26])  ## accommodates upt to three vectors

#' @export
combo.paste <- function(a, b, sep = ".") apply(expand.grid(a, b), 1, paste0, collapse = sep)
#' @export
combo_paste <- function(a, b, sep = "_") apply(expand.grid(a, b), 1, paste0, collapse = sep)
#' @export
combopaste <- function(a, b, sep = "") apply(expand.grid(a, b), 1, paste0, collapse = sep)
#' @export
combo.paste3 <- function(a, b, d, sep = ".") apply(expand.grid(apply(expand.grid(a, b), 1, paste0, collapse = sep), d), 1, paste0, collapse = sep)
#' @export
combo_paste3 <- function(a, b, d, sep = "_") apply(expand.grid(apply(expand.grid(a, b), 1, paste0, collapse = sep), d), 1, paste0, collapse = sep)
#' @export
combopaste3 <- function(a, b, d, sep = "_") apply(expand.grid(apply(expand.grid(a, b), 1, paste0, collapse = sep), d), 1, paste0, collapse = sep)
