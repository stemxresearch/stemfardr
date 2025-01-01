#' @noRd
stm_ivps_funcs <- function() {
    return(invisible(NULL))
}

#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_feuler(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_feuler <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "feuler",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_meuler(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_meuler <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "meuler",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_rkmidpoint(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#'  
stm_ivps_rkmidpoint <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "rkmidpoint",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_rkmeuler(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#'  
stm_ivps_rkmeuler <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "rkmeuler",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_ralston2(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#'
stm_ivps_ralston2 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "ralston2",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_heun3(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_heun3 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "heun3",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_nystrom3(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_nystrom3 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "nystrom3",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_rk3(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_rk3 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "rk3",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#'
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_rk4(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_rk4 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "rk4",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_rk38(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_rk38 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "rk38",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_rkmersen(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_rkmersen <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "rkmersen",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_rk5(
#'   odeqtn = f,
#'   exactsol = ft,
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_rk5 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, show_iters = NULL, 
    view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "rk5",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_ab2(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_ab2 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "ab2",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_ab3(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_ab3 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "ab3",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_ab4(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_ab4 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "ab4",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_ab5(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_ab5 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "ab5",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_eheun(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_eheun <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "eheun",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_abm2(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_abm2 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "abm2",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_abm3(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_abm3 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "abm3",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_abm4(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#'  
stm_ivps_abm4 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "abm4",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_abm5(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_abm5 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "abm5",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_msimpson(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_msimpson <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "msimpson",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param start_values_or_method A character string specifying a method for
#'                               generating start values or a vector of
#'                               initial state values.
#' 
#' @export
#' 
#' @examples 
#' f <- "y - t^2 + 1"
#' ft <- "(t + 1)^2 - 0.5 * exp(t)"
#' result <- stm_ivps_mmsimpson(
#'   odeqtn = f,
#'   exactsol = ft,
#'   start_values_or_method = "rk4",
#'   time_span = c(0, 2),
#'   y0 = 0.5,
#'   nsteps = 10,
#'   show_plot = FALSE
#' )
#' stm_gtable(result)
#' 
stm_ivps_mmsimpson <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), 
    start_values_or_method = c("rk4", "feuler", "meuler", "heun3"), 
    time_span = NULL, y0 = NULL, step_size = NULL, nsteps = 10, maxit = 100, 
    show_iters = NULL, view_table = FALSE, show_plot = TRUE, digits = 8
) {
  result <- stm_ivps(
    method = "mmsimpson",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = start_values_or_method,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = NULL,
    hmax = NULL,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}


#' @template ivps
#' 
#' @param hmin Minimum step size for the adaptive step-size control. Default 
#'             is \code{0.01}
#' @param hmax Maximum step size for the adaptive step-size control. Default 
#'             is \code{0.25}
#' @param tolerance Error tolerance for controlling the local truncation error.
#'                  Default is \code{1e-16}
#' 
#' @examples 
#' # None
#' 
stm_ivps_rkf4 <- function(
    odeqtn, exactsol = NULL, vars = c("t", "y"), time_span = NULL, y0 = NULL, 
    step_size = NULL, nsteps = 10, maxit = 100, hmin = 0.01, hmax = 0.25, 
    tolerance = 1e-16, show_iters = NULL, view_table = FALSE, show_plot = TRUE, 
    digits = 8
) {
  result <- stm_ivps(
    method = "rkf4",
    odeqtn = odeqtn,
    exactsol = exactsol,
    vars = vars,
    derivs = NULL,
    start_values_or_method = NULL,
    time_span = time_span,
    y0 = y0,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = 1e-16,
    hmin = hmin,
    hmax = hmax,
    show_iters = show_iters,
    view_table = view_table,
    show_plot = show_plot,
    digits = digits
  )
  return(result)
}