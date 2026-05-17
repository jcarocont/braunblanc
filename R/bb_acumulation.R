# ============================================================
# bb_accumulation
# ============================================================

#' Curva de acumulación general y por formación
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param nperm `integer`. Permutaciones. Default `999`.
#'
#' @return Lista con `$general` (curva global), `$por_formacion` (curvas por
#'   formación), `$resumen_formacion`, `$estandarizada`.
#'
#' @importFrom vegan specaccum
#' @importFrom dplyr group_by summarise filter arrange desc
#'
#' @export
bb_accumulation <- function(bb, nperm = 999) {

  mat      <- bb$matrix
  parcelas <- bb$parcelas
  meta     <- bb$meta

  # matriz presencia/ausencia, parcelas en filas
  mat_t  <- t(as.matrix(mat[, parcelas]))
  mat_pa <- (mat_t > 0) * 1

  # curva general
  acum <- vegan::specaccum(mat_pa, method = "random", permutations = nperm)
  general <- data.frame(
    parcelas      = acum$sites,
    riqueza_media = acum$richness,
    sd            = acum$sd,
    riqueza_inf   = acum$richness - acum$sd,
    riqueza_sup   = acum$richness + acum$sd
  )

  # curvas por formacion
  formaciones <- sort(unique(meta$formacion))

  por_formacion <- do.call(rbind, lapply(formaciones, function(f) {
    parc_f <- meta$parcela[meta$formacion == f]
    sub    <- mat_pa[rownames(mat_pa) %in% parc_f, , drop = FALSE]
    sub    <- sub[, colSums(sub) > 0, drop = FALSE]

    if (nrow(sub) == 0 || ncol(sub) == 0) {
      return(data.frame(formacion = f, parcelas = NA_real_,
                        riqueza_media = NA_real_, sd = NA_real_,
                        riqueza_inf = NA_real_, riqueza_sup = NA_real_,
                        n_parcelas_formacion = nrow(sub),
                        riqueza_observada_total = 0))
    }
    if (nrow(sub) == 1) {
      riq <- sum(colSums(sub) > 0)
      return(data.frame(formacion = f, parcelas = 1,
                        riqueza_media = riq, sd = NA_real_,
                        riqueza_inf = NA_real_, riqueza_sup = NA_real_,
                        n_parcelas_formacion = 1,
                        riqueza_observada_total = riq))
    }

    acc <- vegan::specaccum(sub, method = "random", permutations = nperm)
    data.frame(
      formacion               = f,
      parcelas                = acc$sites,
      riqueza_media           = acc$richness,
      sd                      = acc$sd,
      riqueza_inf             = acc$richness - acc$sd,
      riqueza_sup             = acc$richness + acc$sd,
      n_parcelas_formacion    = nrow(sub),
      riqueza_observada_total = sum(colSums(sub) > 0)
    )
  }))

  # resumen y estandarizacion
  resumen <- por_formacion |>
    dplyr::group_by(formacion) |>
    dplyr::summarise(
      n_parcelas_formacion    = max(n_parcelas_formacion,    na.rm = TRUE),
      riqueza_observada_total = max(riqueza_observada_total, na.rm = TRUE),
      riqueza_1_parcela       = riqueza_media[which(parcelas == 1)[1]],
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(riqueza_observada_total))

  esf_global    <- min(resumen$n_parcelas_formacion, na.rm = TRUE)
  esf_replicadas <- resumen |>
    dplyr::filter(n_parcelas_formacion >= 2) |>
    dplyr::summarise(x = min(n_parcelas_formacion, na.rm = TRUE)) |>
    dplyr::pull(x)

  estandarizada <- por_formacion |>
    dplyr::filter(parcelas %in% c(esf_global, esf_replicadas)) |>
    dplyr::mutate(tipo_comparacion = dplyr::case_when(
      parcelas == esf_global     ~ paste0("Global a ", esf_global, " parcela(s)"),
      parcelas == esf_replicadas ~ paste0("Replicadas a ", esf_replicadas, " parcela(s)"),
      TRUE                       ~ as.character(parcelas)
    )) |>
    dplyr::arrange(tipo_comparacion, dplyr::desc(riqueza_media))

  list(general        = general,
       por_formacion  = por_formacion,
       resumen        = resumen,
       estandarizada  = estandarizada)
}
