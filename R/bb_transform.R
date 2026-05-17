# ============================================================
# bb_transform: convierte valores BB a numérico
# ============================================================

#' Transformar valores Braun-Blanquet a numérico
#'
#' @param bb Lista devuelta por [read_bbtable()].
#' @param scale Named vector de conversión. Si `NULL` usa [bb_scale_default()].
#'   Los nombres son los símbolos BB (`"r"`, `"+"`, `"1"`, ...) y los valores
#'   son los porcentajes de cobertura media.
#'
#' @return El mismo objeto `bb` con la matriz convertida a numérico.
#'
#' @examples
#' \dontrun{
#' bb <- read_bbtable("Libro2.xlsx")
#' bb <- bb_transform(bb)
#'
#' # escala custom (Domin)
#' bb <- bb_transform(bb, scale = c("r"=0.1, "+"=1, "1"=5, "2"=15,
#'                                   "3"=30, "4"=50, "5"=70,
#'                                   "6"=85, "7"=97.5))
#' }
#'
#' @importFrom dplyr mutate across all_of
#'
#' @export
bb_transform <- function(bb, scale = NULL) {
  if (is.null(scale)) scale <- bb_scale_default()

  .convertir <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x[x %in% c("", "NA", "NULL", "-")] <- NA_character_
    resultado <- scale[tolower(x)]
    # valores no en scale: intentar numérico directo
    sin_match <- is.na(resultado)
    resultado[sin_match] <- suppressWarnings(as.numeric(x[sin_match]))
    as.numeric(tidyr::replace_na(resultado, 0))
  }

  bb$matrix <- bb$matrix |>
    dplyr::mutate(dplyr::across(dplyr::all_of(bb$parcelas), .convertir))

  bb$scale_used <- scale
  bb
}
