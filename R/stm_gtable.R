#' Create a styled table with gt
#'
#' \code{stm_gtable()} converts the provided data to a tibble, replaces any 
#' missing values with a dash ("-"), and generates a styled table using the 
#' \code{gt} package. It applies borders, alignment, and other custom styles 
#' to the table's body and column labels.
#'
#' @param dat A data frame or tibble to be converted into a styled table. 
#'            It will be converted to a tibble if necessary.
#'
#' @return A `gt` table object with styling applied. The table will have missing 
#'         values replaced with a dash ("-"), cell borders, and left alignment 
#'         for character columns.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Converts the input data into a tibble using \code{tibble::as_tibble()}.
#'   \item Replaces any missing values in the data with a dash ("-") using 
#'         \code{tidyr::replace_na()}.
#'   \item Creates a table using the \code{gt::gt()} function.
#'   \item Applies cell border styles to the body and column labels of the table.
#'   \item Left-aligns character columns and applies additional styling such as 
#'         padding and bold column labels.
#' }
#'
#' @seealso \code{\link[gt]{gt}}, \code{\link[tidyr]{replace_na}}, \code{\link[tibble]{as_tibble}}
#'
#' @export
#'
#' @examples
#' tbl <- tibble::tibble(
#'   Name = c("John", "Jane", "Doe"),
#'   Age = c(25, 30, NA),
#'   City = c("New York", "Los Angeles", "Chicago")
#' )
#' stm_gtable(tbl)
#'
stm_gtable <- function(dat) {

  tryCatch({
    dat <- tibble::as_tibble(dat)
    dat <- tidyr::replace_na(dat, list_fill = "-")
  }, error = function(e) {
    stop("Unable to convert 'data' to a tibble. ", e)
  })

  # Create the initial gt table
  gtable <- gt::gt(dat)

  # Replace NA with empty string within gt syntax
  gtable <- gt::sub_missing(
    gtable,
    columns = dplyr::everything(),
    missing_text = ""
  )

  # Apply cell border styles to the body
  gtable <- gt::tab_style(
    gtable,
    style = gt::cell_borders(
      sides = c("left", "right"), color = "#ddd", weight = gt::px(1)
    ),
    locations = gt::cells_body(columns = dplyr::everything())
  )

  # Apply cell border styles to the column labels
  gtable <- gt::tab_style(
    gtable,
    style = gt::cell_borders(
      sides = c("left", "right"), color = "#ccc", weight = gt::px(1)
    ),
    locations = gt::cells_column_labels(columns = dplyr::everything())
  )

  # Apply table options
  gtable <- gt::tab_options(
    gtable,
    column_labels.font.weight = "bold",
    data_row.padding = gt::px(4),
    column_labels.padding = gt::px(4),
    heading.padding = gt::px(4),
    column_labels.background.color = "#EEE",
    table.align = "left"
  )

  # Apply border styles to all sides of the body
  gtable <- gt::tab_style(
    gtable,
    style = gt::cell_borders(
      sides = "all", color = "#ccc", weight = gt::px(0.5), style = "solid"
    ),
    locations = gt::cells_body()
  )

  # Ensure minimum cell width using CSS
  gtable <- gt::tab_style(
    gtable,
    style = list(
      gt::cell_text(whitespace = "nowrap"),
      "min-width: 30px;"
    ),
    locations = gt::cells_body(columns = dplyr::everything())
  )

  # Left-align character columns
  character_columns <- names(dat)[sapply(dat, is.factor)]
  if (length(character_columns) > 0) {
    gtable <- gt::cols_align(
      gtable,
      align = "right",
      columns = character_columns
    )
  }

  return(gtable)
}
