#' Bootstrapping bivariate credible intervals for a ggplot
#'
## Adds a ggplot layer for bootstrapped confidence interval for 2D scatterplot.
## See https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html for a walkthrough.
#'
#' @param n number of resamples
#' @param percent percentile of credible interval
#' @keywords data vis, ggplot additions, robust statistics
#' @examples

#' @export
stat_boot_ci <- function(
  mapping = NULL, data = NULL, geom = "ribbon",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, n = 1000, percent = 95, ...) {

  ggplot2::layer(
    stat = BootCI, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, percent = percent, na.rm = na.rm, ...)
  )

}

#' @export
BootCI <- ggplot2::ggproto(
  "BootCI", ggplot2::Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, params, n = 1000, percent = 95) {

    X <- cbind(rep(1, length(data$x)), data$x)  ## design matrix (includes intercept)
    y <- data$y

    predictions <- vapply(
      seq_len(n),
      function(.) {
        samp <- sample.int(nrow(X), replace = TRUE)
        Xsamp <- X[samp, ]
        X %*% solve(t(Xsamp) %*% Xsamp, t(Xsamp) %*% y[samp])  ## get bs then dot with X
      },
      FUN.VALUE = numeric(nrow(X))
    )

    .alpha <- (100 - percent) / 200  ## 2 tailed
    grid <- data.frame(
      x    = data$x,
      ymax = apply(predictions, 1, quantile, 1 - .alpha),
      ymin = apply(predictions, 1, quantile, .alpha)
    )

    grid

  }

)
