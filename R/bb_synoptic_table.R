# ============================================================
# bb_synoptic_table
# ============================================================

#' Tabla sintética fitosociológica
#'
#' Cobertura media y clase de constancia por especie × formación.
#' Formato clásico: `"III 2.3"`.
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param min_constancia `character`. Clase mínima de constancia para incluir
#'   una especie en una formación. Default `"II"`.
#'
#' @return `data.frame` con especies en filas y formaciones en columnas.
#'
#' @importFrom dplyr left_join group_by summarise mutate filter
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @export
bb_synoptic_table <- function(bb, min_constancia = "II") {
  mat      <- bb$matrix
  parcelas <- bb$parcelas

  clases_orden <- c("I" = 1, "II" = 2, "III" = 3, "IV" = 4, "V" = 5)
  umbral <- clases_orden[[min_constancia]]

  n_por_form <- bb$meta |>
    dplyr::group_by(formacion) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  mat |>
    tidyr::pivot_longer(dplyr::all_of(parcelas),
                        names_to  = "parcela",
                        values_to = "cobertura") |>
    dplyr::left_join(bb$meta[, c("parcela", "formacion")], by = "parcela") |>
    dplyr::group_by(formacion, sp) |>
    dplyr::summarise(
      n_presencia     = sum(cobertura > 0,           na.rm = TRUE),
      cobertura_media = mean(cobertura[cobertura > 0], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(n_por_form, by = "formacion") |>
    dplyr::mutate(
      freq_pct   = 100 * n_presencia / n,
      constancia = clasificar_constancia(freq_pct),
      clase_num  = clases_orden[constancia]
    ) |>
    dplyr::filter(clase_num >= umbral) |>
    dplyr::mutate(
      valor = paste0(constancia, " ", round(cobertura_media, 1))
    ) |>
    dplyr::select(sp, formacion, valor) |>
    tidyr::pivot_wider(names_from  = formacion,
                       values_from = valor,
                       values_fill = "-")
}
