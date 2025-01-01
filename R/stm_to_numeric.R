#' Convert character columns to numeric
#'
#' \code{stm_to_numeric()} converts character columns in a data frame to 
#' numeric. It attempts to convert columns where all values are valid 
#' numeric strings into numeric values.
#'
#' @param dat A data frame or matrix containing the columns to be converted.
#' @param columns A character vector specifying the columns to be converted. 
#'                Default is \code{names(dat)}, meaning all columns will 
#'                be considered.
#'
#' @return A data frame or matrix with the specified columns converted to 
#'         numeric values.
#'
#' @details The function checks each column in the data frame. If a column 
#'          is of type character and contains valid numeric strings, it is 
#'          converted to numeric. Otherwise, the column remains unchanged.
#' 
#' @export
#' 
#' @examples
#' df <- data.frame(
#'   A = c("1", "2", "3"),
#'   B = c("4.5", "5.5", "6.5"),
#'   C = c("X", "Y", "Z")
#' )
#' str(df) # look at data type column
#' 
#' df_new <- stm_to_numeric(df)
#' print(df_new)
#' str(df_new) # look at data type column
#'
#' @export
stm_to_numeric <- function(dat, columns = names(dat)) {
  dat[columns] <- lapply(dat[columns], function(col) {
    if (is.character(col) && all(grepl("^[-+]?[0-9]*\\.?[0-9]+$", col))) {
      as.numeric(col)
    } else {
      col
    }
  })

  return(dat)
}
