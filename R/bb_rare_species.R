# ============================================================
# bb_rare_species: especies raras
# ============================================================

#' Identificar especies raras
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param umbral_freq `numeric`. Umbral máximo de frecuencia absoluta (%)
#'   para considerar una especie como rara. Default `20` (clase I de constancia).
#' @param umbral_cob `numeric`. Umbral máximo de cobertura relativa (%)
#'   para considerar una especie como rara. Default `1`.
#'
#' @return `data.frame` con especies raras y sus estadísticos.
#'
#' @export
bb_rare_species <- function(bb, umbral_freq = 20, umbral_cob = 1) {
  bb_stat_sp(bb) |>
    dplyr::filter(
      frecuencia_abs_pct <= umbral_freq,
      cobertura_rel_pct  <= umbral_cob
    ) |>
    dplyr::arrange(frecuencia_abs_pct, cobertura_rel_pct)
}
