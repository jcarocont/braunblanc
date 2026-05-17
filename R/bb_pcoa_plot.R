# ============================================================
# bb_pcoa_plot — toma pcoa_obj + bb para formacion
# ============================================================

#' Gráfico PCoA
#'
#' @param bb Lista devuelta por [bb_transform()] (para formaciones).
#' @param pcoa_obj Lista devuelta por [bb_pcoa()].
#' @param ovals `logical`. Elipses por formación. Default `TRUE`.
#'
#' @return `ggplot`.
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_color_brewer theme_minimal labs
#' @importFrom ggforce geom_mark_ellipse
#' @importFrom dplyr left_join
#'
#' @export
bb_pcoa_plot <- function(bb, pcoa_obj, ovals = TRUE) {
  scores  <- pcoa_obj$scores |>
    dplyr::left_join(bb$meta[, c("parcela", "formacion")], by = "parcela")
  var_exp <- round(pcoa_obj$varianza_explicada, 1)

  p <- ggplot2::ggplot(scores, ggplot2::aes(x = PCoA1, y = PCoA2,
                                             color = formacion)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_brewer(palette = "Set2") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "PCoA",
      x     = sprintf("PCoA1 (%.1f%%)", var_exp[1]),
      y     = sprintf("PCoA2 (%.1f%%)", var_exp[2]),
      color = "Formación"
    )

  if (ovals) {
    p <- p + ggforce::geom_mark_ellipse(
      ggplot2::aes(fill = formacion, label = formacion),
      alpha = 0.1, show.legend = FALSE
    )
  }

  p
}
