#' Plot statistics on a surface.
#'
#' Given an overlay (e.g., statistics) and underlay (e.g., group-template gifti file).
#' Overlay and underlay must have same structure: lists of length two, with names "L" and "R".
#' Elements within .$L and .$R correspond to vertices.
#'
#' Much of this was adapted from Jo Etzel and John Muscheli.
#' See here: http://mvpa.blogspot.com/2018/06/tutorial-plotting-gifti-images-in-r.html
#'
#' @param overlay list containing values to plot on surface
#' @param underlay list containing underlay image
#' @param hues color pallete
#' @param pos.max/neg.min optional; values more extreme than these arguments will be set to pos.max/neg.min. If not specified (default), no thresholding occurs.
#' @param pos.min/neg.max optional; values less extreme these will be set to 0. If not specified (default), no thresholding occurs.
#' @param thresh0 set zero values to NA? (default == TRUE)
#' @param facet.names a vector of elements %in% c("left_lateral", "left_medial", "right_lateral", "right_medial"). Order determines order of facets within of plot grid.
#' @param dims the dimensions of the plot matrix to set up (prod(dims) must equal length(facet.names))
#' @param anno add text indicaing range of values displayed?
#' @param anno.cex size of annotation
#' @param not.rstudio.gd do not draw plot within rstudio (as rstudio device can be slow!); default is TRUE
#' @keywords AFNI, neuroimaging, gifti, data vis
#'
#' @export


plot_surface <- function (
  overlay,
  underlay       = hcp,
  hues           = viridis(500),
  pos.max        = NULL,
  pos.min        = NULL,
  neg.max        = NULL,
  neg.min        = NULL,
  thresh0        = TRUE,
  facet.names    = c("left_lateral", "left_medial", "right_lateral", "right_medial"),
  dims           = c(1, 4),
  anno           = FALSE,
  facet.params,  ## optional
  anno.cex       = 0.5,
  not.rstudio.gd = TRUE,
  ...
) {


  ## input validation ----

  is.bad.type <- !all(is.list(overlay) | is.list(underlay) | is.character(facet.names) | is.numeric(dims))
  if (is.bad.type) stop("input(s) of wrong type")
  if (any(length(overlay) != 2, length(underlay) != 2)) stop("over/underlay not of length 2")
  if (any(vapply(underlay, class, character(1)) != "gifti")) stop("bad image classes")
  if (any(!names(c(overlay, underlay)) %in% c("L", "R"))) stop("over / underlay names not %in% c('L', 'R')")
  if (any(!facet.names %in% c("left_lateral", "left_medial", "right_lateral", "right_medial"))) {
    stop("bad facet.names")
  }
  if (length(dims) != 2) stop("length(dims) != 2")
  if (prod(dims) != length(facet.names)) stop("prod(dims) != length(facet.names)")
  if (!missing(facet.params)) {
    are.bad.facet.params <- any(
      !is.list(facet.params),
      !unique(vapply(facet.params, length, numeric(1))) == 2,
      !length(facet.params) == length(facet.names),
      !names(facet.params) %in% c("left_lateral", "left_medial", "right_lateral", "right_medial"),
      !unique(c(vapply(facet.params, names, character(2)))) %in% c("theta", "phi")
    )
    if (are.bad.facet.params) stop("bad facet.params")
  }
  if (missing(facet.params)) {
    facet.params <- list(
      right_medial  = c(theta = 270, phi = 0),
      right_lateral = c(theta = 90, phi = 0),
      left_medial   = c(theta = 90, phi = 0),
      left_lateral  = c(theta = 270, phi = 0)
    )
  }
  if (any(!c(pos.max, pos.min) >= 0) | any(!c(neg.min, neg.max) >= 0)) stop("bad lims")


  ## threshold ----

  ## pos max

  if (length(pos.max) == 0) {
    ## if null, don't threshold
  } else if (pos.max == 0) {
    ## if 0, plot no positive
    overlay$L[overlay$L > 0] <- NA
    overlay$R[overlay$R > 0] <- NA
  } else {
    overlay$L[overlay$L > pos.max] <- pos.max  ## greater than pos.max, set to max
    overlay$R[overlay$R > pos.max] <- pos.max
  }

  ## pos min

  if (length(pos.min) == 0) {
    ## if null, don't threshold
  } else {
    overlay$L[overlay$L <= pos.min & overlay$L > 0] <- NA  ## less than pos.max, set to 0
    overlay$R[overlay$R <= pos.min & overlay$R > 0] <- NA
  }

  ## neg min

  if (length(neg.min) == 0) {
    ## if null, don't threshold
  } else if (neg.min == 0) {
    ## plot no negative
    overlay$L[overlay$L < 0] <- NA
    overlay$R[overlay$R < 0] <- NA
  } else {
    overlay$L[overlay$L < neg.min] <- neg.min  ## more extreme than neg.min, set to neg.min
    overlay$R[overlay$R < neg.min] <- neg.min
  }

  ## neg max

  if (length(neg.max) == 0) {
    ## if null, don't threshold
  } else {
    overlay$L[overlay$L > neg.max & overlay$L < 0] <- NA  ## less extreme than neg.max, set to 0
    overlay$R[overlay$R > neg.max & overlay$R < 0] <- NA
  }

  ## handling zero values (typ sub-cortical regions)

  if (thresh0) {
    overlay$L[overlay$L == 0] <- NA
    overlay$R[overlay$R == 0] <- NA
  }


  ## prepare objects for plot3D ----

  triangles <- lapply(underlay, function(.) c(t(.[["data"]][["triangle"]])) + 1)
  indices   <- lapply(triangles, function(.) .[seq(1, length(.), 3)])
  pointsets <- lapply(underlay, function(.) .[["data"]][["pointset"]])
  coords    <- list(L = pointsets$L[triangles$L, ], R = pointsets$R[triangles$R, ])
  values    <- list(L = overlay$L[indices$L], R = overlay$R[indices$R])

  if (not.rstudio.gd) if (interactive()) dev.new(noRStudioGD = TRUE)  ## don't draw to rstudio device (slow!)


  ## plot ----

  par(mfrow = dims)

  for (facet.i in facet.names) {

    hemi.i <- switch(grepl("right", facet.i) + 1, "L", "R")

    plot3D::triangle3D(
      tri    = coords[[hemi.i]],
      colvar = values[[hemi.i]],
      theta  = facet.params[[facet.i]]["theta"],
      phi    = facet.params[[facet.i]]["phi"],
      ltheta = facet.params[[facet.i]]["theta"],
      lphi   = facet.params[[facet.i]]["phi"],
      zlim   = c(-60, 90),
      d      = 6,
      resfac = 0.01,
      bty    = "n",
      colkey = switch((facet.i == "left_lateral") + 1, FALSE, list(side = 1, cex.axis = 1.5)),
      col    = hues,
      facets = FALSE,
      ...
    )

  }

  if (anno) {
    mtext(
      text = paste0("range displayed: ", paste0(round(range(overlay, na.rm = TRUE), 2), collapse = ", ")),
      side = 1, line = -2.5, cex = 0.5, adj = 1
    )
  }

}
