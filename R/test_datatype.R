#' Check data types and column names from DataPlus raw data frame
#'
#' @param .data The raw DataPlus data frame
#' @param .template The applicable data template
#'
#' @return A data frame and message
#' @export
#'
#' @examples
test_datatype <- function(.data, .template) {
  ck <- .template |>
    dplyr::bind_rows(.data) |>
    dplyr::filter(dplyr::if_any(.cols = dplyr::everything(), ~!is.na(.)))

  if (nrow(ck) == nrow(.data) &
      ncol(ck) == ncol(.data)) {
    message("No datatype or column name issues detected")
  } else {
    stop("Data type or column name issues detected!")
  }
  return(ck)
}
