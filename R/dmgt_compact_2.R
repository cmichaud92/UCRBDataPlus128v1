#' Title
#'
#' @param .data A list of data frames extracted from .dbf files
#'
#' @return A list conisiting of only named data frames
#' @export
#'
#' @examples
dmgt_compact_2 <- function(.data) {

  .data <- .data[names(.data) != "NULL"]
  .data <- .data[!is.na(names(.data))]

  names(.data) <- tolower(names(.data))
  tbl_names <- unique(names(.data))[]

  raw_file <- list()

  for (i in seq_along(tbl_names)) {
    raw_file[[i]] <- purrr::map_df(.data[grepl(tbl_names[i], names(.data))], dplyr::bind_rows)
  }

  names(raw_file) <- tbl_names

  raw_file <- raw_file |>
    purrr::map(dplyr::rename_with, tolower)

  return(raw_file)
}
