#' Create NA row
#'
#' Create a row in empty data frame with 'NA'.
#'
#' @param df The empty data frame name.
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#'
create_NA_row <- function(df) {
  if (nrow(df) == 0) {
    df[nrow(df) + 1,] <- NA
  }
  return(df)
}
