#' Add a padding row below data
#'
#' @param .data Data to pad
#' @param n Number of rows to pad
#' 
#' @importFrom stringr str_pad
#'
#' @return Dataframe with extra blank rows
#' @export
pad_row <- function(.data, n=1) {
  .data[(nrow(.data)+1):(nrow(.data)+n), ] <- ""
  .data
}

#' Number formatter
#'
#' Format numbers for presentation, with proper rounding of data
#' 
#' @param var Variable to format
#' @param digits Desired number of decimal places
#' @param size String size
#' @param int_len Space allotted for integer side of the decimal
#'
#' @return Formatted string
#' @export 
num_fmt <- Vectorize(function(var, digits=0, size=10, int_len=3) {
  # Formats summary stat strings to align display correctly
  
  if (is.na(var)) return('')
  
  # Set nsmall to input digits
  nsmall = digits
  
  # Incremement digits for to compensate for display
  if (digits > 0) {
    digits = digits + 1
  }
  
  # Form the string
  return(str_pad(
    format(
      # Round
      round(var, nsmall),
      # Set width of format string
      width=(int_len+digits),
      # Decimals to display
      nsmall=nsmall
    ),
    # Overall width padding
    side='right', size
  ))
})

#' style a tooltip produced by the tippy package
#' 
#' @param text String for text in tooltip
#' @param font_size Font size (in pixels)
#'
#' @return HTML with font size applied
#' @export
tooltip_text <- function(text, font_size = 16) {
  glue::glue("<span style='font-size:{font_size}px;'>{text}<span>")
}

#' update data file configuration setting
#' 
#' @param path path to directory containing data files
#'   used within the Shiny application
#' @return Used for side effects
#' @export
set_data_path <- function(path) {
  # assertions on path
  path <- normalizePath(path, mustWork = FALSE)
  
  if (!dir.exists(path)) stop(paste("The path", path, "does not exist. Please use a valid directory path"), call. = FALSE)
  
  # check if data files are present
  data_files <- c("adsl.xpt", "adadas.xpt", "adtte.xpt",  "adlbc.xpt")
  data_check <-sapply(data_files, function(x) file.exists(file.path(path, x)))
  
  if (!all(data_check)) {
    # determine which files are missing
    missing_files <- data_files[!data_check]
    stop(paste("The following data files are missing in the specified path", path, ":", paste(missing_files, collapse = ", ")), call. = FALSE)
  }
  
  # set golem config option
  golem::amend_golem_config("adam_path", path, talkative = FALSE)
  invisible(TRUE)
}

#' check if a filter is active in a teal module
#' 
#' @param datasets instance of teal filtered datasets class
#' 
#' @return boolean, TRUE if a filter is applied, FALSE otherwise
filter_active <- function(datasets) {
  result <- FALSE
  if (length(names(datasets$get_filter_state()) > 0)) {
    filter_use <- purrr::map_lgl(names(datasets$get_filter_state()), ~{
      # grab call of filter code
      f_call <- datasets$get_call(.x)$filter
      f_call != glue::glue("{.x}_FILTERED <- {.x}")
    })
    result <- any(filter_use)
  }
  
  return(result)
}