# ============================================================
# bb_indval: IndVal con permutaciones via indicspecies
# ============================================================

#' Especies indicadoras por formación (IndVal)
#'
#' Wrapper de [indicspecies::multipatt()] con permutaciones.
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param nperm `integer`. Número de permutaciones. Default `999`.
#' @param alpha `numeric`. Umbral de significancia para filtrar resultados.
#'   Default `0.05`.
#' @param func `character`. Función de IndVal pasada a [indicspecies::multipatt()].
#'   Default `"IndVal.g"`.
#'
#' @return `data.frame` con especies indicadoras, formación, IndVal y p-value.
#'
#' @importFrom indicspecies multipatt
#' @importFrom dplyr filter arrange
#'
#' @export
bb_indval <- function(bb, nperm = 999, alpha = 0.05, func = "IndVal.g") {

  mat      <- bb$matrix
  parcelas <- bb$parcelas

  # parcelas en filas, especies en columnas — formato indicspecies
  mat_t <- t(as.matrix(mat[, parcelas]))
  rownames(mat_t) <- parcelas

  # formacion como vector de grupos, mismo orden que parcelas
  grupos <- bb$meta$formacion[match(parcelas, bb$meta$parcela)]

  res <- indicspecies::multipatt(
    mat_t,
    cluster      = grupos,
    func         = func,
    permutations = nperm,
    print.perm   = FALSE
  )

  # extraer tabla de resultados
  tabla <- as.data.frame(res$sign)
  tabla$sp <- rownames(tabla)

  # columnas de grupo son las que empiezan con "s."
  cols_grupo <- grep("^s\\.", names(tabla), value = TRUE)

  # identificar formacion dominante (la que tiene 1 en la fila)
  tabla$formacion <- apply(tabla[, cols_grupo], 1, function(x) {
    hit <- which(x == 1)
    if (length(hit) == 1) gsub("^s\\.", "", cols_grupo[hit])
    else "combinada"
  })

  tabla |>
    dplyr::filter(p.value <= alpha) |>
    dplyr::select(sp, formacion, stat, p.value) |>
    dplyr::arrange(formacion, dplyr::desc(stat))
}
