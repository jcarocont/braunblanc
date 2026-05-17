# ============================================================
# read_bbtable: lectura sin transformar valores BB
# ============================================================

#' Leer una planilla fitosociológica
#'
#' Lee un archivo Excel con hojas de matriz y metadata. Los valores BB se
#' conservan como caracteres — usar [bb_transform()] para convertir a numérico.
#'
#' @param path `character`. Ruta al archivo `.xlsx`.
#'
#' @return Lista con elementos `matrix` (caracteres BB) y `meta` (metadata).
#'
#' @examples
#' \dontrun{
#' bb <- read_bbtable("Libro2.xlsx")
#' }
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom stringr str_squish str_detect
#' @importFrom stringdist stringdist
#' @importFrom dplyr rename mutate filter
#'
#' @export
read_bbtable <- function(path) {

  .norm <- function(x) tolower(iconv(stringr::str_squish(x), to = "ASCII//TRANSLIT"))

  .fuzzy_sheet <- function(nombres, patron) {
    norm <- .norm(nombres)
    hit  <- which(stringr::str_detect(norm, patron))
    if (length(hit)) return(nombres[hit[1]])
    nombres[which.min(stringdist::stringdist(norm, patron, method = "jw"))]
  }

  .extraer_col <- function(df, patrones) {
    nms_norm <- .norm(names(df))
    for (pat in patrones) {
      idx <- which(stringr::str_detect(nms_norm, pat))
      if (length(idx)) return(names(df)[idx[1]])
    }
    stop("Columna no encontrada. Patrones: ", paste(patrones, collapse = ", "))
  }

  sheets     <- readxl::excel_sheets(path)
  raw_matriz <- readxl::read_excel(path, sheet = .fuzzy_sheet(sheets, "matriz"),
                                   col_types = "text")
  raw_meta   <- readxl::read_excel(path, sheet = .fuzzy_sheet(sheets, "meta"))

  names(raw_matriz) <- stringr::str_squish(names(raw_matriz))
  names(raw_meta)   <- stringr::str_squish(names(raw_meta))

  parcelas <- names(raw_matriz)[stringr::str_detect(names(raw_matriz), "^\\d+$")]
  if (!length(parcelas)) stop("No se detectaron columnas de parcelas (nombres numericos).")

  matrix <- raw_matriz |>
    dplyr::rename(sp = dplyr::all_of(.extraer_col(raw_matriz,
                  c("^sp$", "especie", "species", "taxon")))) |>
    dplyr::mutate(sp = stringr::str_squish(as.character(sp))) |>
    dplyr::filter(!is.na(sp), sp != "")

  meta <- raw_meta |>
    dplyr::rename(
      parcela   = dplyr::all_of(.extraer_col(raw_meta, c("^pm$", "parcela", "punto"))),
      formacion = dplyr::all_of(.extraer_col(raw_meta, c("form.*veget", "unidad.*veget",
                                                          "vegetacional", "formacion")))
    ) |>
    dplyr::mutate(
      parcela   = stringr::str_squish(as.character(parcela)),
      formacion = stringr::str_squish(as.character(formacion))
    ) |>
    dplyr::filter(!is.na(parcela), parcela != "")

  list(matrix = matrix, meta = meta, parcelas = parcelas)
}
