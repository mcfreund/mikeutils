#' Plot statistics on a surface.
#'
#' Given an overlay (e.g., statistics) and underlay (e.g., group-template gifti file).
#' Overlay and underlay must have same structure: lists of length two, with names "L" and "R".
#' Elements within .$L and .$R correspond to vertices.
#'
#'
#' @param d data.frame with one row per ROI.
#' @param col.values name for column of values for overlay
#' @param col.roi.num name for column of roi numbers to match in template
#' @param col.hemi name for column indicating hemisphere
#' @param template which underlay to use?
#' @keywords AFNI, neuroimaging, gifti, data vis
#'
#' @export


build_overlay <- function (
  d, col.values, col.roi.num = "num.roi", col.hemi = "hemi", template = mmp
) {

  if (data.table::is.data.table(d) | tibble::is.tibble(d)) d <- as.data.frame(d)
  if (!is.data.frame(d)) stop("d is not data.frame")
  if (any(!c(col.roi.num, col.hemi, col.values) %in% names(d))) stop("names not in names(d)")
  if (!identical(sort(names(template)), c("L", "R"))) stop("template names wrong (not L, R)")

  overlay <- setNames(vector("list", 2), c("L", "R"))

  for (hemi.i in seq_along(overlay)) {

    ## setup objs for inner loop

    name.hemi.i <- names(overlay)[hemi.i]
    di <- d[d[[col.hemi]] == name.hemi.i, ]
    template.i <- template[[name.hemi.i]]

    for (roi.j in seq_len(nrow(di))) {

      num.roi.j <- di[[col.roi.num]][roi.j]
      template.i[template.i == num.roi.j] <- di[roi.j, col.values]

    }

    overlay[[name.hemi.i]] <- template.i

  }

  overlay

}
