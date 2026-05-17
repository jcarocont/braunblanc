# ============================================================
# bb_quality: estadísticas de calidad
# ============================================================

#' Estadísticas de calidad de una tabla fitosociológica
#'
#' @param bb Lista devuelta por [read_bbtable()] o [bb_transform()].
#'
#' @return Objeto de clase `bb_quality` con métricas de calidad del dataset.
#'
#' @importFrom dplyr filter pull
#'
#' @export
bb_quality <- function(bb) {

  mat  <- bb$matrix
  meta <- bb$meta
  parc <- bb$parcelas

  # consistencia cruzada matrix <-> meta
  parc_en_matrix <- parc
  parc_en_meta   <- as.character(meta$parcela)
  solo_matrix    <- setdiff(parc_en_matrix, parc_en_meta)
  solo_meta      <- setdiff(parc_en_meta, parc_en_matrix)

  # NAs por parcela y por especie (sobre columnas de parcelas)
  mat_vals <- mat[, parc]
  nas_por_parcela  <- colSums(is.na(mat_vals))
  nas_por_especie  <- rowSums(is.na(mat_vals))

  # especies fantasma (todas coberturas 0 o NA tras transform)
  # funciona tanto con caracteres como numérico
  if (is.numeric(mat_vals[[1]])) {
    cob_total_sp  <- rowSums(mat_vals, na.rm = TRUE)
    sp_fantasma   <- mat$sp[cob_total_sp == 0]
  } else {
    sp_fantasma <- character(0)  # no aplica antes de transform
  }

  # parcelas vacías
  if (is.numeric(mat_vals[[1]])) {
    cob_total_parc <- colSums(mat_vals, na.rm = TRUE)
    parc_vacias    <- names(cob_total_parc)[cob_total_parc == 0]
  } else {
    parc_vacias <- character(0)
  }

  # resumen básico
  resumen <- list(
    n_especies  = nrow(mat),
    n_parcelas  = length(parc),
    n_meta_rows = nrow(meta)
  )

  structure(
    list(
      resumen         = resumen,
      solo_matrix     = solo_matrix,
      solo_meta       = solo_meta,
      nas_por_parcela = nas_por_parcela,
      nas_por_especie = nas_por_especie,
      sp_fantasma     = sp_fantasma,
      parc_vacias     = parc_vacias
    ),
    class = "bb_quality"
  )
}
