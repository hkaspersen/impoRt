#' List file names in folder
#'
#' List file names in folder with pattern and recursive settings.
#'
#' @param filepath The path to the folder
#' @param pattern The pattern to search for
#' @param recursive Logical, search in sub-folders or not?
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#'
get_file_names <-
  function(filepath,
           pattern = ".tsv",
           recursive = TRUE) {
    files <- list.files(path = filepath,
                        pattern = pattern,
                        recursive = recursive)
    return(files)
  }
