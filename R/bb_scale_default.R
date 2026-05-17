# ============================================================
# bb_scale_default: escala estándar Braun-Blanquet
# ============================================================

#' Escala de conversión Braun-Blanquet estándar
#'
#' @return Named vector con símbolos BB y sus valores numéricos (% cobertura media).
#' @export
bb_scale_default <- function() {
  c("r" = 0.1, "+" = 0.5, "1" = 5, "2" = 25, "3" = 50, "4" = 75, "5" = 97.5)
}
