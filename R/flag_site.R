#' Add QC flags to standard site data frame
#'
#' @param .data A data frame standardized to the site spec
#'
#' @return A data frame
#' @export
#'
#' @examples
flag_site <- function(.data) {


  out <- .data |>
    dplyr::mutate(rvr_flg = ifelse(!is.na(river) &
                                    river %in% UCRBDataTools::streams_dimensions$vec_rvr, "", "FLAG"),
                  rch_flg = ifelse(is.na(reach) |
                                    (!is.na(reach) &
                                       reach %in% UCRBDataTools::streams_dimensions$vec_rch), "", "FLAG"),
                  date_flg = ifelse(is.na(startdatetime) |
                                     is.na(enddatetime) |
                                     as.numeric(difftime(enddatetime, startdatetime, units = "sec")) <= el_sec,
                                   "FLAG", ""),
                  rmi_flg = ifelse(is.na(start_rmi) |
                                    is.na(end_rmi) |
                                    start_rmi - end_rmi < 0 |
                                    start_rmi - end_rmi > 15,
                                  "FLAG",
                                  ""),
                  gear_flg = ifelse(!is.na(gear) &
                                     gear %in% UCRBDataTools::streams_dimensions$vec_gear, "", "FLAG"),
                  key_flg = ifelse(is.na(key_a), "FLAG", ""),

                  effort_flg = ifelse(gear %in% c("EB", "EF", "EH", "EL", "EN", "EP", "ER", "ES", "EU") &
                                       (is.na(el_sec) | el_sec < 900), "FLAG", ""))

  return(out)
}
