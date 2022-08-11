#' Reads multiple dbf files
#'
#' @param .file_path_in The file path to the target directory
#' @param .file_pattern The pattern used to target the files
#'
#' @return A list of data frames
#' @export
#'
#' @examples
 read_dbf <- function(.file_path_in,
                     .file_pattern) {

  files <- list.files(path = .file_path_in,
                      pattern = .file_pattern,
                      full.names = TRUE,
                      ignore.case = TRUE)

  dat_name <- list()

  dat_name <- as.list(stringr::str_extract(files, "(?<=\\+).*(?=\\.[DdBbFf])")) # Creates list-element names from file names


  data <- purrr::map(files, foreign::read.dbf, as.is = TRUE) # Read all dbf files, strings as characters (as.is)

  names(data) <- tolower(dat_name)                                    # Set list-element names

  data <- purrr::map(data, tibble::as_tibble)                        # Converts df to tibbles

  data <- purrr::compact(data)                               # Removes all empty list elements

  return(data)
}

