#' Add QC flags to standard water data frame
#'
#' @param .data A data frame standardized to the water spec
#'
#' @return A data frame
#' @export
#'
#' @examples
flag_water <- function(.data) {
  out <- .data |>
    dplyr::mutate(amb_flg = ifelse(!is.na(cond_amb) &
                                     (cond_amb < 100 |
                                      cond_amb > 1000),
                                   "FLAG",
                                   ""),
                   spec_flg = ifelse(!is.na(cond_spec) &
                                       (cond_spec < 100 |
                                          cond_spec > 1000),
                                     "FLAG",
                                     ""),
                   temp_flg = ifelse(!is.na(rvr_temp) &
                                       (rvr_temp <= 0 |
                                          rvr_temp > 30),
                                     "FLAG",
                                     ""),
                   secchi_flg = ifelse(!is.na(secchi) &
                                         (secchi < 1 |
                                            secchi > 150),
                                       "FLAG",
                                       ""))

  return(out)
}
