#' Estadísticos por formación vegetal
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param method `character`. `"relev"` agrega estadísticos de [bb_stat_relev()]
#'   por formación (media ± sd). `"todos"` calcula índices sobre el pool de
#'   todas las parcelas de cada formación.
#'
#' @return `data.frame` con estadísticos por formación.
#'
#' @importFrom dplyr left_join group_by summarise n
#' @importFrom vegan diversity
#'
#' @export
bb_stat_form <- function(bb, method = c("relev", "todos")) {
  method <- match.arg(method)

  if (method == "relev") {
    return(
      bb_stat_relev(bb) |>
        dplyr::left_join(bb$meta[, c("parcela", "formacion")], by = "parcela") |>
        dplyr::group_by(formacion) |>
        dplyr::summarise(
          n_parcelas    = dplyr::n(),
          riqueza_media = mean(riqueza,       na.rm = TRUE),
          riqueza_sd    = sd(riqueza,         na.rm = TRUE),
          cob_media     = mean(cobertura_tot, na.rm = TRUE),
          cob_sd        = sd(cobertura_tot,   na.rm = TRUE),
          shannon_media = mean(shannon,       na.rm = TRUE),
          shannon_sd    = sd(shannon,         na.rm = TRUE),
          simpson_media = mean(simpson,       na.rm = TRUE),
          pielou_media  = mean(pielou,        na.rm = TRUE),
          .groups = "drop"
        )
    )
  }

  # method == "todos": pool por formación desde la matriz base
  mat      <- bb$matrix
  parcelas <- bb$parcelas

  .seguro_diversity <- function(x, index) {
    if (sum(x, na.rm = TRUE) == 0) return(NA_real_)
    vegan::diversity(x, index = index)
  }

  formaciones <- unique(bb$meta$formacion)

  do.call(rbind, lapply(formaciones, function(form) {
    parc_form <- intersect(bb$meta$parcela[bb$meta$formacion == form], parcelas)
    if (!length(parc_form)) return(NULL)

    sub    <- as.matrix(mat[, parc_form, drop = FALSE])
    pool   <- rowSums(sub, na.rm = TRUE)  # vector especie, suma across parcelas
    riq    <- sum(pool > 0)

    data.frame(
      formacion       = form,
      n_parcelas      = length(parc_form),
      riqueza         = riq,
      cobertura_total = sum(pool),
      shannon         = .seguro_diversity(pool, "shannon"),
      simpson         = .seguro_diversity(pool, "simpson"),
      pielou          = if (riq > 1) .seguro_diversity(pool, "shannon") / log(riq) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}
