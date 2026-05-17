# ============================================================
# bb_fidelity
# ============================================================

#' Fidelidad de especies a formaciones vegetales
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param method `character`. Método de fidelidad: `"phi"` (correlación punto-biserial)
#'   o `"indval"` (IndVal de Dufrene & Legendre). Default `"phi"`.
#'
#' @return `data.frame` con fidelidad por especie y formación, ordenado desc.
#'
#' @importFrom dplyr left_join group_by summarise mutate filter arrange desc
#' @importFrom tidyr pivot_longer
#'
#' @export
bb_fidelity <- function(bb, method = c("phi", "indval")) {
  method   <- match.arg(method)
  mat      <- bb$matrix
  parcelas <- bb$parcelas

  datos <- mat |>
    tidyr::pivot_longer(dplyr::all_of(parcelas),
                        names_to  = "parcela",
                        values_to = "cobertura") |>
    dplyr::mutate(presencia = as.integer(cobertura > 0)) |>
    dplyr::left_join(bb$meta[, c("parcela", "formacion")], by = "parcela")

  n_total <- length(parcelas)
  formaciones <- unique(bb$meta$formacion)

  do.call(rbind, lapply(formaciones, function(form) {
    do.call(rbind, lapply(unique(mat$sp), function(especie) {

      d       <- datos[datos$sp == especie, ]
      en_form <- d$formacion == form
      a  <- sum( en_form &  d$presencia)   # presente en formacion
      b  <- sum(!en_form &  d$presencia)   # presente fuera
      c_ <- sum( en_form & !d$presencia)   # ausente en formacion
      d_ <- sum(!en_form & !d$presencia)   # ausente fuera

      if (method == "phi") {
        n <- a + b + c_ + d_
        denom <- sqrt((a + b) * (c_ + d_) * (a + c_) * (b + d_))
        fidelidad <- if (denom == 0) NA_real_ else (a * d_ - b * c_) / denom

      } else {
        # IndVal = (A / n_form) * (A / n_presencias) * 100
        n_form <- sum(en_form)
        fidelidad <- if (n_form == 0 || (a + b) == 0) NA_real_
                     else (a / n_form) * (a / (a + b)) * 100
      }

      data.frame(sp = especie, formacion = form, fidelidad = fidelidad,
                 stringsAsFactors = FALSE)
    }))
  })) |>
    dplyr::filter(!is.na(fidelidad)) |>
    dplyr::arrange(formacion, dplyr::desc(fidelidad))
}
