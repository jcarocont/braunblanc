# ============================================================
# print.bb_quality
# ============================================================

#' @export
print.bb_quality <- function(x, ...) {
  cat("=== bb_quality ===\n")
  cat(sprintf("Especies : %d | Parcelas : %d | Filas meta : %d\n",
              x$resumen$n_especies, x$resumen$n_parcelas, x$resumen$n_meta_rows))

  cat("\n-- Consistencia matrix <-> meta --\n")
  if (length(x$solo_matrix))
    cat("Solo en matrix :", paste(x$solo_matrix, collapse = ", "), "\n")
  else
    cat("Solo en matrix : ninguna\n")
  if (length(x$solo_meta))
    cat("Solo en meta   :", paste(x$solo_meta, collapse = ", "), "\n")
  else
    cat("Solo en meta   : ninguna\n")

  cat("\n-- NAs --\n")
  cat("Por parcela:", if (all(x$nas_por_parcela == 0)) "ninguno"
      else paste(names(x$nas_por_parcela[x$nas_por_parcela > 0]),
                 x$nas_por_parcela[x$nas_por_parcela > 0], sep = "=", collapse = ", "), "\n")
  cat("Por especie:", sum(x$nas_por_especie > 0), "especies con NAs\n")

  cat("\n-- Especies fantasma (cob total = 0) --\n")
  cat(if (length(x$sp_fantasma)) paste(x$sp_fantasma, collapse = ", ")
      else "ninguna", "\n")

  cat("\n-- Parcelas vacías --\n")
  cat(if (length(x$parc_vacias)) paste(x$parc_vacias, collapse = ", ")
      else "ninguna", "\n")

  invisible(x)
}


# ============================================================
# save.bb_quality
# ============================================================

#' Guardar estadísticas de calidad a Excel
#'
#' @param x Objeto `bb_quality`.
#' @param file `character`. Ruta del archivo `.xlsx` de salida.
#' @param ... No usado.
#'
#' @importFrom writexl write_xlsx
#'
#' @export
save.bb_quality <- function(x, file, ...) {
  sheets <- list(
    resumen = data.frame(
      metrica = c("n_especies", "n_parcelas", "n_meta_rows"),
      valor   = unlist(x$resumen)
    ),
    consistencia = data.frame(
      tipo    = c(rep("solo_matrix", length(x$solo_matrix)),
                  rep("solo_meta",   length(x$solo_meta))),
      parcela = c(x$solo_matrix, x$solo_meta)
    ),
    nas_por_parcela = data.frame(
      parcela = names(x$nas_por_parcela),
      n_nas   = as.integer(x$nas_por_parcela)
    ),
    nas_por_especie = data.frame(
      especie = names(x$nas_por_especie),
      n_nas   = as.integer(x$nas_por_especie)
    ),
    sp_fantasma = data.frame(especie  = if (length(x$sp_fantasma)) x$sp_fantasma else character(0)),
    parc_vacias = data.frame(parcela  = if (length(x$parc_vacias)) x$parc_vacias else character(0))
  )

  writexl::write_xlsx(sheets, path = file)
  message("✓ Guardado en: ", file)
  invisible(x)
}
