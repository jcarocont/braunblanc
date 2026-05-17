# ============================================================
# bb_nmds_plot — toma nmds_obj + bb para formacion
# ============================================================

#' Gráfico NMDS
#'
#' @param bb Lista devuelta por [bb_transform()] (para formaciones).
#' @param nmds_obj Lista devuelta por [bb_nmds()].
#' @param sp_scores `data.frame` de [bb_score_species()]. Opcional.
#' @param ovals `logical`. Elipses por formación. Default `TRUE`.
#'
#' @return `ggplot`.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_text scale_color_brewer
#'   theme_minimal labs
#' @importFrom ggforce geom_mark_ellipse
#' @importFrom dplyr left_join
#'
#' @export
bb_nmds_plot <- function(bb, nmds_obj, sp_scores = NULL, ovals = TRUE) {
  scores <- nmds_obj$scores_relev |>
    dplyr::left_join(bb$meta[, c("parcela", "formacion")], by = "parcela")

  p <- ggplot2::ggplot(scores, ggplot2::aes(x = NMDS1, y = NMDS2,
                                             color = formacion)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_brewer(palette = "Set2") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = sprintf("NMDS (stress = %.3f)", nmds_obj$stress),
                  color = "Formación")

  if (ovals) {
    p <- p + ggforce::geom_mark_ellipse(
      ggplot2::aes(fill = formacion, label = formacion),
      alpha = 0.1, show.legend = FALSE
    )
  }

  if (!is.null(sp_scores)) {
    p <- p + ggplot2::geom_text(
      data = sp_scores,
      ggplot2::aes(x = NMDS1, y = NMDS2, label = sp),
      color = "grey30", size = 3, fontface = "italic", inherit.aes = FALSE
    )
  }

  p
}

