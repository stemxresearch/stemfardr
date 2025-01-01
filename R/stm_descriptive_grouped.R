#' Compute grouped data statistics
#'
#' \code{grouped_data_statistics()} statistical summaries for grouped data
#' using class limits and frequencies. The function generates midpoints,
#' frequency products, and optionally adjusts calculations based on an assumed
#' mean and class width.
#'
#' @param statistic Character; the statistic to compute.
#' @param class_limits Numeric vector; the class limits for grouped data,
#'                     given as pairs of lower and upper bounds. This vector
#'                     must have an even number of elements.
#' @param frequency Numeric vector; the frequencies corresponding to each class
#'                  interval. Must match the number of classes derived from
#'                  \code{class_limits}.
#' @param assumed_mean Logical or numeric; if \code{TRUE}, the assumed mean
#'                        is computed automatically. If \code{FALSE},
#'                        calculations are performed without an assumed mean.
#'                        If numeric, it specifies a custom assumed mean. 
#'                        Defaults to \code{TRUE}.
#' @param is_assumed_divide Logical; if \code{TRUE}, divides certain values by
#'                          the class width in the calculations. Defaults to
#'                          \code{TRUE}.
#' @param digits Integer; the number of decimal places to round the result.
#'               Defaults to 4.
#'
#' @return A data frame containing:
#'   \item{lower}{Lower class limits}
#'   \item{upper}{Upper class limits}
#'   \item{mid_point}{Class midpoints}
#'   \item{f}{Frequencies}
#'   \item{fx}{Product of frequency and midpoints,
#'   if \code{assumed_mean = FALSE}}
#'   \item{tt}{Transformed midpoints, if \code{assumed_mean = TRUE}}
#'   \item{ft}{Product of frequency and transformed midpoints,
#'   if \code{assumed_mean = TRUE}}
#' 
#' @export
#'
#' @examples
#' class_limits <- c(
#'   100, 103, 104, 107, 108, 111, 112, 115, 116, 119, 120, 123
#' )
#' freq <- c(1, 15, 42, 31, 8, 3)
#' 
#' result <- stm_descriptive_grouped(
#'   statistic = "mean",
#'   class_limits = class_limits,
#'   frequency = freq
#' )
#' stm_gtable(result)
#' 
stm_descriptive_grouped <- function(
    statistic = c(
      "mean", "variance", "std", "median", "mode", "lower_quartile",
      "upper_quartile", "percentile", "iqr", "polygon", "histogram",
      "orgive"
    ),
    class_limits,
    frequency,
    assumed_mean = NULL,
    is_assumed_divide = TRUE,
    digits = 4
) {
  statistic <- match.arg(statistic)
  check_vector(vec = class_limits, par_name = "class_limits")
  check_vector(vec = frequency, par_name = "frequency")
  is_assumed_divide <- check_logical(is_assumed_divide, default = TRUE)

  class_width <- if (is_assumed_divide) {
    class_limits[2] - class_limits[1] + 2
  } else {
    1
  }
  n <- length(class_limits)
  if (!check_is_even(n)) {
    stop("Expected number of values in 'class_limits' to be even, got --> ", n)
  }
  class_limits_diff <- diff(class_limits)
  if (!all(class_limits_diff[seq(2, length(class_limits_diff), by = 2)] == 1)) {
    stop("Difference between upper and lower class limit of next class must ",
         "be 1")
  }
  if (any(class_limits_diff[seq(1, length(class_limits_diff), by = 2)] < 1)) {
    stop("Expected the classes to be increasing but encounted a case where ",
         "lower limit > upper limit")
  }
  dat <- data.frame(matrix(class_limits, ncol = 2, byrow = TRUE))
  names(dat) <- c("lower", "upper")
  dat$mid_point <- (dat$lower + dat$upper) / 2
  if (is.null(assumed_mean)) {
    assumed_mean <- (class_limits[n] - class_limits[1]) / 2
    t <- dat$mid_point - assumed_mean
    is_assumed_mean = FALSE
  } else {
    check_numeric(num = assumed_mean, par_name = "assumed_mean")
    mid_assumedmean <- dat$mid_point - assumed_mean
    is_assumed_mean = TRUE
  }  

  if (is_assumed_mean) {
    dat$t <- (dat$mid_point - assumed_mean) / class_width
    dat$f <- frequency
    dat$ft <- dat$f * dat$t
  } else {
    dat$f <- frequency
    dat$fx <- dat$f * dat$mid_point
  }

  return(dat)
}
