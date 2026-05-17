#' Heatmap de cobertura por especie × parcela
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param top_n `integer`. Número de especies a mostrar, seleccionadas por
#'   IVI_geo descendente. Default `20`.
#'
#' @return `ggplot`.
#'
#' @importFrom ggplot2 ggplot aes geom_tile theme_minimal theme element_text labs
#' @importFrom dplyr slice_max pull filter mutate
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_reorder
#'
#' @export
bb_heatmap <- function(bb, top_n = 20) {
  top_sp <- bb_stat_sp(bb) |>
    dplyr::slice_max(IVI_geo, n = top_n, with_ties = FALSE) |>
    dplyr::pull(sp)

  bb$matrix |>
    tidyr::pivot_longer(dplyr::all_of(bb$parcelas),
                        names_to  = "parcela",
                        values_to = "cobertura") |>
    dplyr::filter(sp %in% top_sp) |>
    dplyr::mutate(sp = factor(sp, levels = rev(top_sp))) |>
    ggplot2::ggplot(ggplot2::aes(x = parcela, y = sp, fill = cobertura)) +
    ggplot2::geom_tile() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
    ggplot2::labs(
      title = paste0("Heatmap cobertura — top ", top_n, " especies por IVI"),
      x     = "Parcela",
      y     = NULL,
      fill  = "Cobertura"
    )
}
