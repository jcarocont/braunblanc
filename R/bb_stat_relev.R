# ============================================================
# bb_stat_relev: estadísticos por relevé
# ============================================================

#' Estadísticos por relevé (parcela)
#'
#' @param bb Lista devuelta por [bb_transform()].
#'
#' @return `data.frame` con riqueza, cobertura e índices de diversidad por parcela.
#'
#' @importFrom dplyr all_of
#' @importFrom vegan diversity
#'
#' @export
bb_stat_relev <- function(bb) {
  mat      <- bb$matrix
  parcelas <- bb$parcelas

  mat_vals <- as.matrix(mat[, parcelas])
  rownames(mat_vals) <- mat$sp

  # transponer: parcelas en filas para vegan
  mat_t <- t(mat_vals)

  .seguro_diversity <- function(x, index) {
    if (sum(x, na.rm = TRUE) == 0) return(NA_real_)
    vegan::diversity(x, index = index)
  }

  data.frame(
    parcela        = parcelas,
    riqueza        = colSums(mat_vals > 0, na.rm = TRUE),
    cobertura_tot  = colSums(mat_vals,     na.rm = TRUE),
    shannon        = apply(mat_t, 1, \(x) .seguro_diversity(x, "shannon")),
    simpson        = apply(mat_t, 1, \(x) .seguro_diversity(x, "simpson")),
    pielou         = {
      sh <- apply(mat_t, 1, \(x) .seguro_diversity(x, "shannon"))
      rq <- colSums(mat_vals > 0, na.rm = TRUE)
      ifelse(rq > 1, sh / log(rq), NA_real_)
    },
    stringsAsFactors = FALSE
  )
}
