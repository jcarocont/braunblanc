# ============================================================
# bb_dom_range: rango-dominancia
# ============================================================

#' Rango-dominancia de especies
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param by_form `logical`. Si `TRUE` calcula el rango-dominancia por
#'   formación vegetal. Si `FALSE` (default) calcula global.
#'
#' @return `data.frame` con rango, cobertura relativa y acumulada por especie.
#'
#' @importFrom dplyr mutate arrange desc
#'
#' @export
bb_dom_range <- function(bb, by_form = FALSE) {

  .calc_range <- function(df) {
    df |>
      dplyr::arrange(dplyr::desc(cobertura_abs)) |>
      dplyr::mutate(
        rango          = seq_len(dplyr::n()),
        cobertura_rel  = 100 * cobertura_abs / sum(cobertura_abs, na.rm = TRUE),
        cobertura_acum = cumsum(cobertura_rel)
      )
  }

  if (!by_form) {
    bb_stat_sp(bb) |>
      dplyr::select(sp, cobertura_abs) |>
      .calc_range()
  } else {
    bb_sp_x_form(bb) |>
      dplyr::rename(cobertura_abs = cobertura_total) |>
      dplyr::group_by(formacion) |>
      dplyr::group_modify(~ .calc_range(.x)) |>
      dplyr::ungroup()
  }
}
