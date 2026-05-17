# ============================================================
# bb_plot_dendrograma
# ============================================================

#' Dendrograma de relevés
#'
#' @param dist_obj Lista devuelta por [bb_distance()].
#' @param k `integer`. Clusters. Si `NULL` detecta por gap statistic en `2:10`.
#' @param method_hclust `character`. Método de agrupamiento. Default `"ward.D2"`.
#'
#' @return `ggplot`.
#'
#' @importFrom ggplot2 ggplot aes geom_text scale_color_brewer theme_minimal labs geom_segment
#' @importFrom ggdendro dendro_data label segment 
#' @importFrom cluster clusGap maxSE
#'
#' @export
bb_plot_dendrograma <- function(dist_obj, k = NULL, method_hclust = "ward.D2") {
  hc <- hclust(dist_obj$dist, method = method_hclust)

  if (is.null(k)) {
    gap <- cluster::clusGap(
      as.matrix(dist_obj$dist),
      FUN   = function(x, k) list(cluster = cutree(hclust(dist(x),
                                  method = method_hclust), k)),
      K.max = 10,
      B     = 50
    )
    k <- cluster::maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"],
                        method = "Tibs2001SEmax")
    message("k detectado por gap statistic: ", k)
  }

  clusters  <- cutree(hc, k = k)
  dend_data <- ggdendro::dendro_data(hc)
  labels    <- ggdendro::label(dend_data)
  labels$cluster <- factor(clusters[labels$label])

  ggplot2::ggplot() +
    ggdendro::geom_segment(data = ggdendro::segment(dend_data),
                           ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
    ggplot2::geom_text(data = labels,
                       ggplot2::aes(x = x, y = -0.01, label = label, color = cluster),
                       angle = 90, hjust = 1, size = 3) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title  = paste0("Dendrograma (k=", k, ", ", method_hclust, ")"),
                  x = NULL, y = "Altura", color = "Cluster")
}

