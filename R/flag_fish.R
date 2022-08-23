#' Add QC flags to standard fish data frame
#'
#' @param .data A data frame standardized to the fish spec
#'
#' @return A data frame
#' @export
#'
#' @examples
flag_fish <- function(.data) {

  # Key species total length range table
  # Records containing lengths outside of range will be flagged
  tbl_tot_len <- tibble::tribble(
    ~species, ~min_len, ~max_len,
    "CS", 120, 1000,
    "RZ", 325, 800,
    "BT", 120, 450,
    "HB", 90, 450,
    "SM", 40, 450,
    "WE", 300, 800,
    "NP", 100, 1000,
  )


    out <- .data %>%
      dplyr::left_join(tbl_tot_len, by = "species") |>
      dplyr::mutate(pit_flg = ifelse(grepl('^[A-F0-9]{3}\\.?[A-F0-9]{10}$',
                                            pit_134,
                                            ignore.case = TRUE) |
                                        (is.na(pit_134) &
                                           species %!in% UCRBDataTools::vec_spp$end),
                                      "",
                                      "FLAG"),
                     recap_134_flg = ifelse(!is.na(pit_134) & is.na(recap_134),
                                            "FLAG",
                                            ""),
                     spp_flg = ifelse(species %in% UCRBDataTools::streams_dimensions$vec_spp, "", "FLAG"),
                     len_flg = ifelse(species %in% UCRBDataTools::vec_spp$end & is.na(tot_length) |
                                        species %!in% c(UCRBDataTools::vec_spp$end, "SM", "WE", "NP") |
                                        (species %in% c(UCRBDataTools::vec_spp$end, "SM", "WE", "NP") &
                                           tot_length > min_len &
                                           tot_length < max_len),
                                      "",
                                      "FLAG"),
                     ct_flg = ifelse(is.na(fish_count),
                                     "FLAG",
                                     ""),
                     disp_flg = ifelse(is.na(disp) |
                                         disp %!in% UCRBDataTools::streams_dimensions$vec_disp |
                                         (species %in% UCRBDataTools::vec_spp$end & disp != "RA"), "FLAG", ""),
                     rmi_flg = ifelse(is.na(rmi) |
                                        rmi < end_rmi |
                                        rmi > start_rmi, "FLAG", ""),
                     ripe_flg = ifelse(ripe %in% c("Y", "N") |
                                         is.na(ripe), "", "FLAG"),
                     sex_flg = ifelse(sex %in% c("F", "M", "I") |
                                        is.na(sex), "", "FLAG"),
              ) |>
      select(-c(min_len, max_len, enddatetime, date, time))

    return(out)

}
