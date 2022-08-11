#' Compact data frame list into a single element per table
#'
#' @param .data A list containing multiple data frames
#' @param .data_year Year the data was collected
#' @param .agency The agency collecting the data
#' @param .study The study the field work was conducted under
#' @param .save_raw_csv Do you want to save the raw data to .csv
#'
#' @return a concise list of data frames
#' @export
#'
#' @examples
dmgt_compact <- function(.data,
                     .data_year = NULL,
                     .agency = NULL,
                     .study = NULL,
                     .save_raw_csv = FALSE) {
  raw_file <- list()
  if (exists("site", where = .data)) {
    raw_file$site <- purrr::map_df(.data[grepl("site", names(.data))], dplyr::bind_rows)
  }
  if (exists("water", where = .data)) {
    raw_file$water <- purrr::map_df(.data[grepl("water", names(.data))], dplyr::bind_rows)
  }
  if (exists("rare", where = .data)) {
    raw_file$rare <- purrr::map_df(.data[grepl("rare$", names(.data))], dplyr::bind_rows)
  }
  if (exists("rare_scan", where = .data)) {
    raw_file$rare_scan <- purrr::map_df(.data[grepl("rare_scan", names(.data))], dplyr::bind_rows)
  }
  if (exists("ntf", where = .data)) {
    raw_file$ntf <- purrr::map_df(.data[grepl("ntf", names(.data))], dplyr::bind_rows)
  }
  if (exists("nnf_ct", where = .data)) {
    raw_file$nnf_ct <- purrr::map_df(.data[grepl("nnf_ct", names(.data))], dplyr::bind_rows)
  }

  raw_file <- raw_file |>
    purrr::map(dplyr::rename_with, tolower)

  if (.save_raw_csv == TRUE) {



    names(raw_file) %>%
      purrr::walk(~readr::write_csv(raw_file[[.]],
                                    paste0("./output/",
                                           .agency, "/",
                                           "Raw-DataPlus-download_",
                                           .study, "_",
                                           .agency, "_",
                                           .data_year, "_",
                                           ., ".csv"),
                                    append = file.exists(paste0("./output/",
                                                                .agency, "/",
                                                                "Raw-DataPlus-download_",
                                                                .study, "_",
                                                                .agency, "_",
                                                                .data_year, "_",
                                                                ., ".csv"))))

  }
  return(raw_file)
}
