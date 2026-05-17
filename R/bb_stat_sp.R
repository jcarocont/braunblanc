# ============================================================
# bb_stat_sp: estadísticos por especie
# ============================================================

#' Estadísticos por especie
#'
#' @param bb Lista devuelta por [bb_transform()].
#'
#' @return `data.frame` con frecuencia, cobertura, IVI y constancia por especie.
#'
#' @importFrom dplyr group_by summarise mutate arrange desc
#' @importFrom tidyr replace_na
#'
#' @export
bb_stat_sp <- function(bb) {
  mat      <- bb$matrix
  parcelas <- bb$parcelas
  n_parc   <- length(parcelas)
  clasificar_constancia <- function(freq_pct) {
  dplyr::case_when(
    freq_pct <= 20 ~ "I",
    freq_pct <= 40 ~ "II",
    freq_pct <= 60 ~ "III",
    freq_pct <= 80 ~ "IV",
    TRUE           ~ "V"
  )}

  datos_largos <- mat |>
    tidyr::pivot_longer(dplyr::all_of(parcelas),
                        names_to  = "parcela",
                        values_to = "cobertura") |>
    dplyr::mutate(presencia = cobertura > 0)

  datos_largos |>
    dplyr::group_by(sp) |>
    dplyr::summarise(
      n_parcelas_presente            = sum(presencia, na.rm = TRUE),
      frecuencia_abs_pct             = 100 * n_parcelas_presente / n_parc,
      cobertura_abs                  = sum(cobertura, na.rm = TRUE),
      cobertura_media_todas_parcelas = mean(cobertura, na.rm = TRUE),
      cobertura_media_solo_presencia = if (n_parcelas_presente > 0)
                                         mean(cobertura[presencia], na.rm = TRUE)
                                       else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      frecuencia_rel_pct = if (sum(frecuencia_abs_pct) > 0)
                             100 * frecuencia_abs_pct / sum(frecuencia_abs_pct)
                           else rep(NA_real_, dplyr::n()),
      cobertura_rel_pct  = if (sum(cobertura_abs, na.rm = TRUE) > 0)
                             100 * cobertura_abs / sum(cobertura_abs, na.rm = TRUE)
                           else rep(NA_real_, dplyr::n()),
      IVI_geo    = sqrt((frecuencia_rel_pct+0.001)*(cobertura_rel_pct+0.001)),
      constancia = clasificar_constancia(frecuencia_abs_pct)
    ) |>
    dplyr::arrange(dplyr::desc(IVI_geo))

# helper interno
}
