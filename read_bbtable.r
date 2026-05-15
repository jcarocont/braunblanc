#' Leer una planilla fitosociológica
#'
#' @param path `character`. Ruta al archivo `.xlsx`.
#' @param val_r `numeric`. Valor para categoría `"r"`. Default `0.1`.
#' @param val_mas `numeric`. Valor para `"+"`. Default `0.5`.
#'
#' @return Lista con dos elementos:
#' \describe{
#'   \item{`matrix`}{`data.frame`. Matriz especie × parcela con valores BB convertidos.}
#'   \item{`meta`}{`data.frame`. Metadata de parcelas con columnas `parcela` y `formacion`.}
#' }
#'
#' @examples
#' \dontrun{
#' datos <- read_bbtable("Libro2.xlsx")
#' datos$matrix
#' datos$meta
#' }
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom stringr str_squish str_detect
#' @importFrom stringdist stringdist
#' @importFrom dplyr rename mutate filter across all_of case_when
#' @importFrom tidyr replace_na
#'
#' @export
read_bbtable <- function(path, val_r = 0.1, val_mas = 0.5) {

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

  .convertir_bb <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x[x %in% c("", "NA", "NULL", "-", "0")] <- NA_character_
    dplyr::case_when(
      is.na(x)          ~ 0,
      tolower(x) == "r" ~ val_r,
      x == "+"          ~ val_mas,
      TRUE              ~ suppressWarnings(as.numeric(x))
    ) |> tidyr::replace_na(0)
  }

  sheets <- readxl::excel_sheets(path)
  raw_matriz <- readxl::read_excel(path, sheet = .fuzzy_sheet(sheets, "matriz"))
  raw_meta   <- readxl::read_excel(path, sheet = .fuzzy_sheet(sheets, "meta"))

  names(raw_matriz) <- stringr::str_squish(names(raw_matriz))
  names(raw_meta)   <- stringr::str_squish(names(raw_meta))

  parcelas <- names(raw_matriz)[stringr::str_detect(names(raw_matriz), "^\\d+$")]
  if (!length(parcelas)) stop("No se detectaron columnas de parcelas (nombres numericos).")

  matrix <- raw_matriz |>
    dplyr::rename(sp = dplyr::all_of(.extraer_col(raw_matriz, c("^sp$", "especie", "species", "taxon")))) |>
    dplyr::mutate(sp = stringr::str_squish(as.character(sp))) |>
    dplyr::filter(!is.na(sp), sp != "") |>
    dplyr::mutate(dplyr::across(dplyr::all_of(parcelas), .convertir_bb))

  meta <- raw_meta |>
    dplyr::rename(
      parcela   = dplyr::all_of(.extraer_col(raw_meta, c("^pm$", "parcela", "punto"))),
      formacion = dplyr::all_of(.extraer_col(raw_meta, c("form.*veget", "unidad.*veget", "vegetacional", "formacion")))
    ) |>
    dplyr::mutate(
      parcela   = stringr::str_squish(as.character(parcela)),
      formacion = stringr::str_squish(as.character(formacion))
    ) |>
    dplyr::filter(!is.na(parcela), parcela != "")

  list(matrix = matrix, meta = meta)
}
