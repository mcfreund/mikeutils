#' Plotting GIFTI surface files.
#'
## Adds a ggplot layer for bootstrapped confidence interval for 2D scatterplot.
## Much of this was adapted from code written by Joset Etzel
## (http://mvpa.blogspot.com/2018/06/tutorial-plotting-gifti-images-in-r.html) and by John Muscheli (from library(gifti)).
#'
#' @param underlay a list of length 2, with elements titled c("L", "R"), corresponding to each hemisphere of a gifti of the desired underlay.
#' @param overlay values (colors) to plot; same structure as underlay
#' @param facet.names character vector with elements \%in\% c("left_lateral", "left_medial", "right_lateral", "right_medial"). Determines the row-wise ordering of the plot matrix.
#' @param dims Dimensions of plot matrix (row, col).
#' @param facet.params Optional list, of same structure as facet.names, containing theta and phi elements which specify orientations of each facet.
#' @examples

#' @export

plot_surface <- function(
  overlay, underlay,
  facet.names = c("left_lateral", "left_medial", "right_lateral", "right_medial"),
  dims = c(1, 4),
  facet.params
){
  # underlay <- hcp; overlay <- mmp
  # facet.names <- c("left_lateral", "left_medial", "right_lateral", "right_medial")
  # dims = c(1, 4)

  ## TODO
  ## limits
  ## titles + text
  ## superior & inferior views?

  ## input validation

  is.bad.type <- !all(is.list(overlay) | is.list(underlay) | is.character(facet.names) | is.numeric(dims))
  if (is.bad.type) stop("input(s) of wrong type")
  if (any(length(overlay) !=2, length(underlay) != 2)) stop("over/underlay not of length 2")
  if (any(vapply(c(overlay, underlay), class, character(1)) != "gifti")) stop("bad image classes")
  if (any(!names(c(overlay, underlay)) %in% c("L", "R"))) stop("over / underlay names not %in% c('L', 'R')")
  if (any(!facet.names %in% c("left_lateral", "left_medial", "right_lateral", "right_medial"))) stop("bad facet.names")
  if (length(dims) != 2) stop("length(dims) != 2")
  if (prod(dims) != length(facet.names)) stop("prod(dims) != length(facet.names)")
  if (!missing(facet.params)) {
    are.bad.facet.params <- any(
      !is.list(facet.params),  ## type ok?
      !unique(vapply(facet.params, length, numeric(1))) == 2,  ## lengths ok?
      !length(facet.params) == length(facet.names),  ## length ok?
      !names(facet.params) %in% c("left_lateral", "left_medial", "right_lateral", "right_medial"),  ## names ok?
      !unique(c(vapply(facet.params, names, character(2)))) %in% c("theta", "phi")  ## element names ok?
    )
    if (are.bad.facet.params) stop("bad facet.params")
  }

  ## default orientations

  if (missing(facet.params)) {

    facet.params <- list(
      right_medial  = c(theta = 270, phi = 0),
      right_lateral = c(theta = 90, phi = 0),
      left_medial   = c(theta = 90, phi = 0),
      left_lateral  = c(theta = 270, phi = 0)
    )

  }

  ## extract data from gifti objects and format

  triangles <- lapply(underlay, function(.) c(t(.[["data"]][["triangle"]])) + 1)
  indices   <- lapply(triangles, function(.) .[seq(1, length(.), 3)])
  pointsets <- lapply(underlay, function(.) .[["data"]][["pointset"]])
  values    <- lapply(overlay, function(.) .[["data"]][[1]][, 1])

  ## assign colors to triangles

  coords <- list(L = pointsets$L[triangles$L, ], R = pointsets$R[triangles$R, ])
  values <- list(L = values$L[indices$L], R = values$R[indices$R])

  ## draw

  par(mfrow = dims)

  for (facet.i in facet.names) {
    # facet.i = "right_medial"

    hemi.i <- switch(grepl("right", facet.i) + 1, "L", "R")

    plot3D::triangle3D(
      tri    = coords[[hemi.i]],  ## overlay
      colvar = values[[hemi.i]],  ## underlay
      theta  = facet.params[[facet.i]]["theta"],  ## "polar angle" / "colatitute"
      phi    = facet.params[[facet.i]]["phi"],  ## "azithmuthal angle"
      ## (see https://en.wikipedia.org/wiki/Spherical_coordinate_system)
      ltheta = facet.params[[facet.i]]["theta"],  ## lighting source
      lphi   = facet.params[[facet.i]]["phi"],
      zlim   =  c(-60, 90),
      d      = 6, ## ?
      resfac = 0.01,  ## resolution factor
      bty    = "n",  ## background type
      colkey = FALSE,  ## color key
      # col    = cols,  ## colors
      # clim   = c.lims,  ## color limits
      facets = FALSE
    )

  }
}
