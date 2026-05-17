# ============================================================
# bb_nmds
# ============================================================

#' Ordenación NMDS
#'
#' @param dist_obj Lista devuelta por [bb_distance()].
#' @param k `integer`. Dimensiones. Default `2`.
#' @param trymax `integer`. Intentos de convergencia. Default `100`.
#'
#' @return Lista con `$nmds`, `$scores_relev` y `$stress`.
#'
#' @importFrom vegan metaMDS scores
#'
#' @export
bb_nmds <- function(dist_obj, k = 2, trymax = 100) {
  nmds <- vegan::metaMDS(dist_obj$dist, k = k, trymax = trymax,
                         autotransform = FALSE)

  scores_relev <- as.data.frame(vegan::scores(nmds, display = "sites"))
  colnames(scores_relev) <- c("NMDS1", "NMDS2")
  scores_relev$parcela <- attr(dist_obj$dist, "Labels")

  list(
    nmds         = nmds,
    scores_relev = scores_relev,
    stress       = nmds$stress
  )
}

