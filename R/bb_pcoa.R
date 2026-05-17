# ============================================================
# bb_pcoa
# ============================================================

#' Ordenación PCoA
#'
#' @param dist_obj Lista devuelta por [bb_distance()].
#'
#' @return Lista con `$pcoa`, `$scores` y `$varianza_explicada`.
#'
#' @export
bb_pcoa <- function(dist_obj) {
  pcoa    <- cmdscale(dist_obj$dist, k = 2, eig = TRUE)
  eig_pos <- pcoa$eig[pcoa$eig > 0]
  var_exp <- 100 * eig_pos / sum(eig_pos)

  scores <- as.data.frame(pcoa$points)
  colnames(scores) <- c("PCoA1", "PCoA2")
  scores$parcela <- attr(dist_obj$dist, "Labels")

  list(
    pcoa               = pcoa,
    scores             = scores,
    varianza_explicada = var_exp
  )
}
