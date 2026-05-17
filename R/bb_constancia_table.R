# ============================================================
# bb_constancia_table
# ============================================================

#' Tabla de constancia por formación
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param format `character`. Formato del valor en cada celda:
#'   `"class"` (I–V), `"numeric"` (% frecuencia), `"percent"` (% con símbolo).
#'   Default `"class"`.
#'
#' @return `data.frame` con especies en filas y formaciones en columnas.
#'
#' @importFrom dplyr left_join group_by summarise mutate
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @export
bb_constancia_table <- function(bb, format = c("class", "numeric", "percent")) {
  format <- match.arg(format)

  mat      <- bb$matrix
  parcelas <- bb$parcelas

  # n parcelas por formacion
  n_por_form <- bb$meta |>
    dplyr::group_by(formacion) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  mat |>
    tidyr::pivot_longer(dplyr::all_of(parcelas),
                        names_to  = "parcela",
                        values_to = "cobertura") |>
    dplyr::left_join(bb$meta[, c("parcela", "formacion")], by = "parcela") |>
    dplyr::group_by(formacion, sp) |>
    dplyr::summarise(n_presencia = sum(cobertura > 0, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::left_join(n_por_form, by = "formacion") |>
    dplyr::mutate(
      freq_pct = 100 * n_presencia / n,
      valor = switch(format,
        "class"   = clasificar_constancia(freq_pct),
        "numeric" = as.character(round(freq_pct, 1)),
        "percent" = paste0(round(freq_pct, 1), "%")
      )
    ) |>
    dplyr::select(sp, formacion, valor) |>
    tidyr::pivot_wider(names_from = formacion, values_from = valor, values_fill = switch(format, "class" = "I", "0", "0%"))
}

