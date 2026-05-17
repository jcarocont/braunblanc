# ============================================================
# bb_score_species — necesita ambos
# ============================================================
#' Scores de especies en el espacio NMDS
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param nmds_obj Lista devuelta por [bb_nmds()].
#' @param top_n `integer`. Top especies por IVI_geo. Default `15`.
#'
#' @return `data.frame` con scores NMDS de las top especies.
#'
#' @importFrom vegan wascores scores
#' @importFrom dplyr filter pull slice_head
#'
#' @export
bb_score_species <- function(bb, nmds_obj, top_n = 15) {
  mat      <- bb$matrix
  parcelas <- bb$parcelas
  mat_t    <- t(as.matrix(mat[, parcelas]))

  sp_scores <- as.data.frame(
    vegan::wascores(vegan::scores(nmds_obj$nmds, display = "sites"), mat_t)
  )
  sp_scores$sp <- rownames(sp_scores)

  top_sp <- bb_stat_sp(bb) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::pull(sp)

  sp_scores |> dplyr::filter(sp %in% top_sp)
}
