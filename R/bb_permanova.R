# ============================================================
# bb_permanova
# ============================================================

#' PERMANOVA, Betadisper y SIMPER
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param dist_obj Lista devuelta por [bb_distance()].
#' @param nperm `integer`. Permutaciones. Default `999`.
#'
#' @return Lista con `$permanova`, `$betadisper`, `$simper`.
#'
#' @importFrom vegan adonis2 betadisper permutest simper
#' @importFrom dplyr mutate
#' @importFrom purrr imap_dfr
#'
#' @export
bb_permanova <- function(bb, dist_obj, nperm = 999) {

  mat      <- bb$matrix
  parcelas <- bb$parcelas
  meta     <- bb$meta

  grupos <- meta$formacion[match(parcelas, meta$parcela)]
  mat_t  <- t(as.matrix(mat[, parcelas]))

  permanova <- tryCatch({
    ad <- vegan::adonis2(dist_obj$dist ~ grupos, permutations = nperm)
    as.data.frame(ad) |> tibble::rownames_to_column("termino")
  }, error = function(e) data.frame(error = conditionMessage(e)))

  betadisper <- tryCatch({
    bd <- vegan::betadisper(dist_obj$dist, group = as.factor(grupos))
    pt <- vegan::permutest(bd, permutations = nperm)
    tibble::tibble(termino = rownames(as.data.frame(pt$tab)),
                   as.data.frame(pt$tab))
  }, error = function(e) data.frame(error = conditionMessage(e)))

  simper <- tryCatch({
    sim <- vegan::simper(mat_t, group = as.factor(grupos),
                         permutations = nperm)
    purrr::imap_dfr(sim, function(x, nm) {
      df <- as.data.frame(x) |>
        tibble::rownames_to_column("sp") |>
        dplyr::mutate(comparacion = nm, .before = 1)
      if ("cumsum"  %in% names(df)) dplyr::arrange(df, comparacion, cumsum)
      else if ("average" %in% names(df)) dplyr::arrange(df, comparacion,
                                                         dplyr::desc(average))
      else df
    })
  }, error = function(e) data.frame(error = conditionMessage(e)))

  list(permanova = permanova, betadisper = betadisper, simper = simper)
}

