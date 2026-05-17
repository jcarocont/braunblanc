# ============================================================
# bb_distance
# ============================================================

#' Calcular matriz de distancias entre relevés
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param method `character`. Método de distancia pasado a [vegan::vegdist()].
#'   Default `"bray"`.
#' @param hellinger `logical`. Si `TRUE` aplica transformación Hellinger antes
#'   de calcular distancias. Default `FALSE`.
#'
#' @return Lista con `$dist` (objeto `dist`) y `$method` (método usado).
#'
#' @importFrom vegan vegdist decostand
#'
#' @export
bb_distance <- function(bb, method = "bray", hellinger = FALSE) {
  mat      <- bb$matrix
  parcelas <- bb$parcelas

  mat_t <- t(as.matrix(mat[, parcelas]))
  rownames(mat_t) <- parcelas

  if (hellinger) mat_t <- vegan::decostand(mat_t, method = "hellinger")

  list(
    dist   = vegan::vegdist(mat_t, method = method),
    method = method
  )
}
