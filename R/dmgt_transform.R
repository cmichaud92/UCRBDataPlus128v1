#' Transform DataPlus raw data fram to standard data frame
#'
#' @param .site_data A site data frame (required)
#' @param .rare_data A rare data frame (optional)
#' @param .ntf_data A ntf data frame (optional)
#' @param .water_data A water quality data frame (optional)
#' @param .rare_scan_data A rare scan data frame (optional)
#' @param .nnf_ct_data A non native count data frame (optional)
#'
#' @return A list of data frames restructured to the data standard
#' @export
#'
#' @examples
dmgt_transform <- function(.site_data,
                           .rare_data = NULL,
                           .ntf_data = NULL,
                           .water_data = NULL,
                           .rare_scan_data = NULL,
                           .nnf_ct_data = NULL) {

  # .site_data = tmp2_data$site
  # .rare_data = tmp2_data$rare
  # .water_data = tmp2_data$water
  # .ntf_data = tmp2_data$ntf
  # .rare_scan = NULL
  # .nnf_ct = NULL
  dat <- list()
  tmp_fish <- list()
  # Site data
  dat$site <- .site_data %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "Z"),
                  dplyr::across(dplyr::matches("time"), ~ifelse(is.na(.),
                                                                "00:00:00",
                                                                .)),
                  startdatetime = as.POSIXct(paste(lubridate::mdy(date), starttime)),         # Replace `date` and `time` with `datetime`
                  enddatetime = as.POSIXct(paste(lubridate::mdy(date), endtime)),
                  dplyr::across(where(is.POSIXct), lubridate::force_tz, tzone = "UTC"),
                  el_sec = effort_sec + (effort_min * 60)) %>%
    tidyr::unite(col = "site_notes", site_note1, site_note2, site_note3, na.rm = TRUE, sep = " | ") %>%

    dplyr::arrange(startdatetime) %>%                                                   # this orders data for indexing

    dplyr::mutate(idx = dplyr::row_number()) %>%
    dplyr::select(key_a,
                  idx,
                  project,
                  river,
                  reach,
                  gear,
                  pass,
                  startdatetime,
                  enddatetime,
                  start_rmi,
                  end_rmi,
                  shoreline,
                  el_sec,
                  boat,
                  crew,
                  site_notes,
                  dplyr::everything())


  # Create site_id df and apply to all tables.
  samp_n <- dat$site %>%
    dplyr::mutate(t_stamp = as.POSIXct(paste(lubridate::mdy(date),
                                             starttime))) %>%
    dplyr::select(key_a,
                  s_idx = idx,
                  t_stamp,
                  date,
                  start_rmi,
                  end_rmi,
                  enddatetime)

  # Water data
  if (!is.null(.water_data)) {
    dat$water <- .water_data %>%
      left_join(samp_n, by = "key_a") %>%
      arrange(t_stamp) %>%
      mutate(across(where(is.numeric),
                    ~ifelse(. == 0, NA, .)),
             across(where(is.character),
                    na_if, "Z"),                                       # Converts 0's to NA
             idx = row_number()) %>%                                 # Converts "Z"s to NA

      select(key_a,
             key_ad,
             s_idx,
             idx,
             matches('^cond'),
             rvr_temp,
             secchi,
             water_notes = h2o_notes)

  } else {

    message("No `water` data present")
  }

  if (!is.null(.rare_data)) {
    tmp_rare <- .rare_data %>%
      left_join(select(samp_n, key_a, s_idx, date, start_rmi, end_rmi), by = "key_a") %>%
      mutate(across(c(ilat, ilon, tot_length, weight),                             # Converts 0's to NA
                    ~ifelse(. == 0, NA, .)),
             across(pit_134, toupper),
             across(where(is.character),
                    na_if, "Z"),
             datetime = as.POSIXct(paste(mdy(date), time)),
             fish_count = 1,
             size_class = NA_character_) %>%
      unite(col = "fish_notes", matches("_note"), na.rm = TRUE, sep = " | ") %>%
      rename(rec_num = key_aa)

    loc_rare <- tmp_rare %>%
      select(key_a, rmi, ilon, ilat) %>%
      filter(!(is.na(ilon) | is.na(ilat))) %>%
      distinct(key_a, rmi, .keep_all = TRUE) %>%
      mutate(epsg = 4326)

    tmp_fish$rare <- tmp_rare %>%
      select(-c(ilon, ilat)) %>%
      left_join(loc_rare, by = c("key_a", "rmi"))


  } else {

    warning("No `rare` data present")
  }

  if (!is.null(.rare_scan_data)) {

    tmp_rare_scan <- .rare_scan_data %>%
      filter(!grepl("z", pit_134, ignore.case = TRUE)) %>%
      left_join(select(samp_n, key_a, s_idx, enddatetime, start_rmi, end_rmi), by = "key_a") %>%
      mutate(across(c(ilat, ilon),                             # Converts 0's to NA
                    ~ifelse(. == 0, NA, .)),
             across(pit_134, toupper),
             across(where(is.character),
                    na_if, "Z"),
             datetime = enddatetime,
             fish_count = 1,
             size_class = NA_character_) %>%
      unite(col = "fish_notes", matches("_note"), na.rm = TRUE, sep = " | ") %>%
      rename(rec_num = key_ae)

    loc_rare_scan <- tmp_rare_scan %>%
      select(key_a, rmi, ilon, ilat) %>%
      filter(!(is.na(ilon) | is.na(ilat))) %>%
      distinct(key_a, rmi, .keep_all = TRUE) %>%
      mutate(epsg = 4326)

    tmp_fish$rare_scan <- tmp_rare_scan %>%
      select(-c(ilon, ilat)) %>%
      left_join(loc_rare_scan, by = c("key_a", "rmi"))


  } else {

    warning("No `rare_scan` data present")
  }

  if (!is.null(.ntf_data)) {

    tmp_ntf <- .ntf_data %>%
      left_join(select(samp_n, key_a, s_idx, date, enddatetime, start_rmi, end_rmi), by = "key_a") %>%

      mutate(across(c(ilat, ilon, tot_length, weight),                             # Converts 0's to NA
                    ~ifelse(. == 0, NA, .)),
             across(where(is.character),
                    na_if, "Z"),
             across(rmi, ~ifelse(. == 0 | is.na(.),
                                 end_rmi,
                                 .)),
             datetime = if_else(!is.na(time),
                                as.POSIXct(paste(mdy(date), time)),
                                enddatetime),
             fish_count = 1,
             size_class = NA_character_,
             hab1 = NA_character_,
             hab2 = NA_character_,
             sex_cert = NA_character_,
             tubercles = NA_character_,
             ray_ct = NA_character_,
             pit_400 = NA_character_) %>%                                                # Converts "Z"s to NA
      unite(col = "fish_notes", matches("_note"), na.rm = TRUE, sep = " | ") %>%
      rename(rec_num = key_ab)

    loc_ntf <- tmp_ntf %>%
      select(key_a, rmi, ilon, ilat) %>%
      filter(!(is.na(ilon) | is.na(ilat))) %>%
      distinct(key_a, rmi, .keep_all = TRUE) %>%
      mutate(epsg = 4326)

    tmp_fish$ntf <- tmp_ntf %>%
      select(-c(ilon, ilat)) %>%
      left_join(loc_ntf, by = c("key_a", "rmi"))

  } else {

    warning("No `ntf` data present")
  }

  if(!is.null(.rare_data) |
     !is.null(.ntf_data) |
     !is.null(.rare_scan_data) |
     !is.null(.nnf_ct_data)) {

    dat$fish <- bind_rows(tmp_fish)%>%
      filter(!(is.na(species) &
                 is.na(pit_134) &
                 is.na(recap_134) &
                 is.na(pit_400))) %>%
      arrange(datetime) %>%
      mutate(idx = row_number(),
             across(datetime, force_tz, "UTC")) %>%
      #  group_by(key_a, rmi) %>%
      #  mutate(across(c(ilon, ilat), ~first(na.omit(.)))) %>%
      #  ungroup() %>%
      select(matches("key_"),
             matches("+idx"),
             rec_num,
             rmi,
             ilon,
             ilat,
             epsg,
             hab1,
             hab2,
             datetime,
             species,
             fish_count,
             size_class,
             tot_length,
             weight,
             ripe,
             sex,
             sex_cert,
             tubercles,
             ray_ct,
             disp,
             matches("_134"),
             matches("_400"),
             matches("floy"),
             matches("vial"),
             fish_notes,
             start_rmi,
             end_rmi,
             everything()
      )
  }
  return(dat)

}



