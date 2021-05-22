#'
#'
#'
#'
#' @param R matrix
#' @param var.names
#' @param val.name
#' @keywords RSA
#' @examples

#' @export



matplot <- function(x) {

  ggplot(melt_mat(x), aes(v1, v2, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c(option = "inferno") +
    theme_minimal() +
    theme(
      axis.text = element_blank(), axis.title = element_blank(), legend.position = "none",
      panel.border = element_blank(), panel.grid = element_blank()
    )

}
