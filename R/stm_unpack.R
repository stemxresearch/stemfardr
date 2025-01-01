#' Unpack elements of a data structure into the environment
#'
#' \code{stm_unpack()} assigns elements of a list, named vector, or data frame
#' to the specified environment, creating variables for each element.
#'
#' @param dat A list, named vector, or data frame to unpack into variables.
#' @param output Logical. If \code{TRUE}, displays the unpacked variable names
#'               and their values. Default is \code{FALSE}.
#' @param envir The environment in which to unpack variables. Default is
#'              \code{.GlobalEnv}.
#'
#' @return Invisibly returns \code{NULL}. The function creates variables in
#'         the specified environment.
#'
#' @details
#' This function allows you to unpack elements of a supported data structure
#' (list, named vector, or data frame) and assign them as variables in the
#' specified environment. If \code{data} is not of a supported type, an error
#' is raised.
#'
#' @export
#'
#' @examples
#' lst <- list(a = 10, b = 20, c = 30)
#' print(lst)
#' stm_unpack(lst)
#' a
#' b
#' c
#'
#' v <- c(x = 1, y = 2, z = 3)
#' print(v)
#' stm_unpack(v)
#' a
#' b
#' c
#'
stm_unpack <- function(dat, output = FALSE, envir = .GlobalEnv) {
  if (is.list(dat)) { # list or DataFrame
    list2env(dat, envir = envir)
  } else if (is.vector(dat) && !is.null(names(dat))) {
    list2env(as.list(dat), envir = envir)
  } else {
    stop("Expected 'dat' to be a list, named vector, or data frame, ",
         "got --> ", class(dat))
  }

  if (output) {
    values <- mget(names(dat), envir = envir)
    message(
      paste(names(values), "=", unlist(values), collapse = "\n")
    )
  }

  invisible(NULL)
}
