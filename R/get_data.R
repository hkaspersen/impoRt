#' Import data from multiple files
#'
#' Specify file location and import data from multiple files either recursively or directly in the folder. Outputs a data frame with correct folder names for each row.
#'
#' @param filepath The path to the files/folders
#' @param pattern The pattern in the files you want imported
#' @param recursive Logical, search in sub folders or not?
#' @param delim Single character used to separate fields within a record, (see \code{\link[readr]{read_delim}})
#' @param skip_rows Ignore blank rows? (see \code{\link[readr]{read_delim}})
#' @param progress Show progress for data import? (see \code{\link[readr]{read_delim}})
#' @param trim_ws Should leading and trailing whitespace be trimmed? (see \code{\link[readr]{read_delim}})
#' @param col_types One of NULL, a cols() specification, or a string, (see \code{\link[readr]{read_delim}})
#' @param col_names Logical, does the data have column names or not? (see \code{\link[readr]{read_delim}})
#' @param quoted_na Should missing values inside quotes be treated as missing values (the default) or strings? (see \code{\link[readr]{read_delim}})
#'
#' @export
#'
#' @importFrom readr read_delim
#' @importFrom dplyr bind_rows
#'
get_data <- function(filepath,
                     pattern,
                     recursive = TRUE,
                     delim = "\t",
                     skip_rows = FALSE,
                     progress = FALSE,
                     trim_ws = FALSE,
                     col_types = NULL,
                     col_names = TRUE,
                     quoted_na = TRUE) {

  # Identify file names in filepath
  files <- get_file_names(filepath,
                          pattern = pattern,
                          recursive = recursive)

  # Silence progress bar from read_delim
  if (progress == FALSE) {
    options(readr.num_columns = 0)
  }

  # Import data
  data_list <- lapply(
    files,
    FUN = function(file) {
      read_delim(
        paste0(filepath, "/", file),
        delim = delim,
        trim_ws = trim_ws,
        col_types = col_types,
        col_names = col_names,
        quoted_na = quoted_na
      )
    }
  )

  # Set file names in list
  names(data_list) <- files

  if (skip_rows == FALSE) {
    # Fill empty data frames with NA
    data_list <- lapply(data_list,
                         function(x) create_NA_row(x))
  }

  # Bind to data frame
  data <- bind_rows(data_list, .id = "ref")

  # Clean names
  data$ref <- sub("//*.+", "", data$ref)

  return(data)
}
