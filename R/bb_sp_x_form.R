# ============================================================
# bb_sp_x_form: especies por formación
# ============================================================

#' Presencia y cobertura de especies por formación vegetal
#'
#' @param bb Lista devuelta por [bb_transform()].
#'
#' @return `data.frame` con frecuencia y cobertura media por especie y formación.
#'
#' @importFrom dplyr left_join group_by summarise mutate
#' @importFrom tidyr pivot_longer
#'
#' @export
bb_sp_x_form <- function(bb) {
  mat      <- bb$matrix
  parcelas <- bb$parcelas

  mat |>
    tidyr::pivot_longer(dplyr::all_of(parcelas),
                        names_to  = "parcela",
                        values_to = "cobertura") |>
    dplyr::left_join(bb$meta[, c("parcela", "formacion")], by = "parcela") |>
    dplyr::group_by(formacion, sp) |>
    dplyr::summarise(
      n_parcelas      = dplyr::n(),
      frecuencia_pct  = 100 * sum(cobertura > 0, na.rm = TRUE) / dplyr::n(),
      cobertura_media = mean(cobertura[cobertura > 0], na.rm = TRUE),
      cobertura_total = sum(cobertura, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(frecuencia_pct > 0) |>
    dplyr::arrange(formacion, dplyr::desc(cobertura_total))
}
