#' Fetch the configuration file
#'
#' @param .config_name Name of the target config file
#' @param .machine The machine you are working on (FWS or CNHP)
#' @param .study The target study
#'
#' @return A list
#' @export
#'
#' @examples
get_config <- function(.config_name, .machine = "FWS", .study) {
  if (.machine == "FWS") {
    CONFIG_PATH <-  paste0("C:/Users/cmichaud/Projects_git/etc/",
                           .config_name)
  } else if (.machine == "CNHP") {
    CONFIG_PATH <- "/Users/jstahli/Documents/etc/config_128_standard.yml"
  } else {
    stop(".machine not recognized")
  }

  {
    Sys.setenv(R_CONFIG_ACTIVE = .study)
    config <- config::get(file = CONFIG_PATH)
  }
}
