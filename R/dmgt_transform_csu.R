#' Transform variables in CSU/CPW nnf removal applications
#'
#' @param .data A list of data frames
#'
#' @return A list of data frames
#' @export
#'
#' @examples
dmgt_transform_csu <- function(.data) {

  transform <- list()

  if (exists("sample", where = .data)) {
    transform$sample <- .data$sample %>%
      dplyr::rename_with(tolower) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "Z"),
                    dplyr::across(dplyr::matches("time"), ~ifelse(is.na(.),
                                                                  "00:00:00",
                                                                  .)),
                    sampledatetime = as.POSIXct(paste(lubridate::mdy(date), sampletime)),         # Replace `date` and `time` with `datetime`
                    #                enddatetime = as.POSIXct(paste(lubridate::mdy(date), endtime)),
                    dplyr::across(where(is.POSIXct), lubridate::force_tz, tzone = "UTC"),
                    .keep = "unused"
                    #                el_sec = effort_sec + (effort_min * 60)
      ) %>%
      tidyr::unite(col = "sample_notes", snotes1, snotes2, snotes3, snotes4, na.rm = TRUE, sep = " | ") %>%

      dplyr::arrange(sampledatetime) %>%                                                   # this orders data for indexing

      dplyr::mutate(idx = dplyr::row_number()) %>%
      dplyr::select(key_a,
                    idx,
                    sample,
                    sampledatetime,
                    hab1,
                    hab2,
                    trip,
                    pass,
                    reach,
                    gear,
                    pass,
                    ets,
                    handheld,
                    tagreader,
                    purpose,
                    boat,
                    crew,
                    sample_notes,
                    count,
                    matches('_cond$|temp$'))

    samp_info <- .data$sample %>%
      select(key_a,
             samp_date = date)
  }

  if (exists("el_effort", where = .data)) {

    transform$effort <- .data$el_effort %>%
      left_join(samp_info, by = "key_a") %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "Z"),
                    dplyr::across(dplyr::matches("time"), ~ifelse(is.na(.),
                                                                  "00:00:00",
                                                                  .)),
                    el_datetime = as.POSIXct(paste(lubridate::mdy(samp_date), eltime)),         # Replace `date` and `time` with `datetime`
                    dplyr::across(where(is.POSIXct), lubridate::force_tz, tzone = "UTC"),
                    el_sec = (as.numeric(str_extract(el_end, ".*(?=\\-)")) * 60) + as.numeric(str_extract(el_end, "(?<=\\-).*")) -
                      (as.numeric(str_extract(el_start, ".*(?=\\-)")) * 60) + as.numeric(str_extract(el_start, "(?<=\\-).*")),
                    .keep = "unused") %>%
      tidyr::unite(col = "effort_notes", efnotes1, efnotes2, efnotes3, na.rm = TRUE, sep = " | ") %>%

      select(-c(samp_date, eltime, el_start, el_end))

    effort_info <- transform$effort %>%
      select(key_aa,
             el_datetime,
             rm_end)

  }

  if (exists("fish", where = .data)) {

    transform$fish <- tmp_dat2$fish %>%
      left_join(effort_info, by = "key_aa") %>%
      rename(enc_datetime = el_datetime,
             enc_rmi = rm_end) %>%
      # dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "Z"),
      #               .keep = "unused") %>%
      tidyr::unite(col = "fish_notes", fnote1, fnote2, fnote3, na.rm = TRUE, sep = " | ") %>%
      select(key_a:fishnum, enc_datetime, enc_rmi, everything())

  }
  return(transform)
}
