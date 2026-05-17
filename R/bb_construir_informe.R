# ============================================================
# bb_construir_informe
# ============================================================

#' Construir informe fitosociológico
#'
#' @param bb Lista devuelta por [bb_transform()].
#' @param name `character`. Nombre base del proyecto. Define nombres de archivos
#'   y directorio de salida.
#' @param format `character`. Formato de salida: `"excel"`, `"graficos"`,
#'   `"word"` o `"excel-y-plots"`. Default `"excel-y-plots"`.
#' @param out_dir `character`. Directorio raíz de salida. Default `"."`.
#' @param nperm `integer`. Permutaciones para PERMANOVA/SIMPER/acumulación.
#'   Default `999`.
#'
#' @return Invisible. Escribe archivos en disco.
#'
#' @importFrom writexl write_xlsx
#' @importFrom ggplot2 ggsave
#'
#' @export
bb_construir_informe <- function(bb, name, format = "excel-y-plots",
                                 out_dir = ".", nperm = 999) {

  format <- match.arg(format, c("excel", "graficos", "word", "excel-y-plots"))

  # directorios
  plots_dir <- file.path(out_dir, name)
  xlsx_path <- file.path(out_dir, paste0("flora-", name, ".xlsx"))

  if (format %in% c("graficos", "excel-y-plots"))
    dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

  # calcular todo
  message("Calculando estadísticos...")
  dist_obj  <- bb_distance(bb)
  stat_sp   <- bb_stat_sp(bb)
  stat_relev <- bb_stat_relev(bb)
  stat_form  <- bb_stat_form(bb, method = "todos")
  sp_x_form  <- bb_sp_x_form(bb)
  raras      <- bb_rare_species(bb)
  dom_range  <- bb_dom_range(bb)
  perm       <- bb_permanova(bb, dist_obj, nperm = nperm)
  acum       <- bb_accumulation(bb, nperm = nperm)
  quality    <- bb_quality(bb)

  nmds_obj   <- bb_nmds(dist_obj)
  pcoa_obj   <- bb_pcoa(dist_obj)
  sp_scores  <- bb_score_species(bb, nmds_obj)

  dist_mat   <- as.data.frame(as.matrix(dist_obj$dist))
  dist_mat   <- cbind(parcela = rownames(dist_mat), dist_mat)

  # ---- EXCEL ----
  if (format %in% c("excel", "excel-y-plots")) {
    message("Escribiendo Excel...")

    sheets <- list(
      "Resumen"              = data.frame(
        indicador = c("proyecto", "fecha", "n_parcelas", "n_especies",
                      "nmds_stress"),
        valor     = as.character(c(name, as.character(Sys.time()),
                                   length(bb$parcelas), nrow(bb$matrix),
                                   round(nmds_obj$stress, 4)))
      ),
      "Estadigrafos_sp"      = stat_sp,
      "Estadigrafos_relev"   = stat_relev,
      "Estadigrafos_form"    = stat_form,
      "Sp_x_form"            = sp_x_form,
      "Especies_raras"       = raras,
      "Rango_dominancia"     = dom_range,
      "Distancia_Bray"       = dist_mat,
      "NMDS_sitios"          = nmds_obj$scores_relev,
      "NMDS_especies_top"    = sp_scores,
      "PCoA_sitios"          = pcoa_obj$scores,
      "PERMANOVA"            = perm$permanova,
      "Betadisper"           = perm$betadisper,
      "SIMPER"               = perm$simper,
      "Acumulacion_general"  = acum$general,
      "Raref_por_formacion"  = acum$por_formacion,
      "Raref_resumen"        = acum$resumen,
      "Raref_estandarizada"  = acum$estandarizada,
      "QC_nas_parcela"       = as.data.frame(quality$nas_por_parcela),
      "QC_solo_matrix"       = data.frame(parcela = quality$solo_matrix),
      "QC_solo_meta"         = data.frame(parcela = quality$solo_meta),
      "QC_sp_fantasma"       = data.frame(sp = quality$sp_fantasma)
    )

    writexl::write_xlsx(sheets, path = xlsx_path)
    message("Excel: ", xlsx_path)
  }

  # ---- GRAFICOS ----
  if (format %in% c("graficos", "excel-y-plots")) {
    message("Generando gráficos en: ", plots_dir)

    .guardar <- function(p, fname, w = 12, h = 8) {
      ggplot2::ggsave(file.path(plots_dir, fname), plot = p,
                      width = w, height = h, dpi = 150)
    }

    # nmds
    .guardar(bb_nmds_plot(bb, nmds_obj, ovals = TRUE),
             "ordenacion_nmds.png")
    .guardar(bb_nmds_plot(bb, nmds_obj, sp_scores = sp_scores, ovals = FALSE),
             "ordenacion_nmds_especies.png")

    # pcoa
    .guardar(bb_pcoa_plot(bb, pcoa_obj, ovals = TRUE),
             "ordenacion_pcoa.png")

    # dendrograma
    .guardar(bb_plot_dendrograma(dist_obj),
             "dendrograma.png")

    # ivi
    .guardar(
      stat_sp |>
        dplyr::slice_max(IVI_geo, n = 15, with_ties = FALSE) |>
        dplyr::mutate(sp = forcats::fct_reorder(sp, IVI_geo)) |>
        ggplot2::ggplot(ggplot2::aes(x = IVI_geo, y = sp)) +
        ggplot2::geom_col() +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(title = "Top 15 especies por IVI", x = "IVI_200", y = NULL),
      "top15_ivi.png"
    )

    # rango dominancia
    .guardar(
      dom_range |>
        ggplot2::ggplot(ggplot2::aes(x = rango, y = cobertura_rel)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(title = "Curva rango-dominancia",
                      x = "Rango", y = "Cobertura relativa (%)"),
      "rango_dominancia.png"
    )

    # acumulacion general
    .guardar(
      acum$general |>
        ggplot2::ggplot(ggplot2::aes(x = parcelas, y = riqueza_media)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = riqueza_inf, ymax = riqueza_sup),
                             alpha = 0.2) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(title = "Curva de acumulación de especies",
                      x = "Parcelas", y = "Riqueza acumulada media"),
      "acumulacion_general.png"
    )

    # rarefaccion por formacion
    .guardar(
      acum$por_formacion |>
        ggplot2::ggplot(ggplot2::aes(x = parcelas, y = riqueza_media,
                                     color = formacion, fill = formacion)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = riqueza_inf, ymax = riqueza_sup),
                             alpha = 0.15, color = NA) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(title = "Rarefacción por formación vegetal",
                      x = "Parcelas", y = "Riqueza acumulada media",
                      color = "Formación", fill = "Formación"),
      "rarefaccion_por_formacion.png", w = 12, h = 8
    )

    # heatmap top 20 IV

    .guardar(bb_heatmap(bb, top_n = 20), "heatmap_cobertura.png", w = 14, h = 9)

    message("Gráficos: ", plots_dir)
  }

  invisible(NULL)
}
