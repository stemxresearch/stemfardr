#' @noRd 
stm_ivps <- function(
    method = c(
      "feuler", "meuler", "rkmidpoint", "rkmeuler", "ralston2", "heun3",
      "nystrom3", "rk3", "rk4", "rk38", "rkmersen", "rk5", "ab2", "ab3",
      "ab4", "ab5", "eheun", "abm2", "abm3", "abm4", "abm5", "msimpson",
      "mmsimpson", "rkf4"
    ),
    odeqtn = NULL,
    exactsol = NULL,
    vars = c("t", "y"),
    derivs = NULL,
    start_values_or_method = c("feuler", "meuler", "heun3", "rk4"),
    time_span = NULL,
    y0 = NULL,
    step_size = NULL,
    nsteps = 10,
    maxit = 100,
    tolerance = 1e-6,
    hmin = NULL,
    hmax = NULL,
    show_iters = NULL,
    view_table = FALSE,
    show_plot = TRUE,
    digits = 8
) {

  # ===========================================================================

  METHODS_LIST <- list(
    "feuler" = "Forward Euler",
    "meuler" = "Modified Euler",
    "rkmidpoint" = "Midpoint Runge-Kutta",
    "rkmeuler" = "Modified Euler Runge-Kutta",
    "ralston2" = "Second order Ralston",
    "heun3" = "Third order Heun",
    "nystrom3" = "Third order Nystrom",
    "rk3" = "Third order Runge-Kutta",
    "rk4" = "Fourth order Runge-Kutta",
    "rk38" = "Fourth order Runge-Kutta - 38",
    "rkmersen" = "Fourth order Runge-Kutta-Mersen",
    "rk5" = "Fifth order Runge-Kutta",
    "ab2" = "Adams-Bashforth - 2-step",
    "ab3" = "Adams-Bashforth - 3-step",
    "ab4" = "Adams-Bashforth - 4-step",
    "ab5" = "Adams-Bashforth - 5-step",
    "eheun" = "Euler Heun",
    "abm2" = "Adams-Bashforth-Moulton - 2-step",
    "abm3" = "Adams-Bashforth-Moulton - 3-step",
    "abm4" = "Adams-Bashforth-Moulton - 4-step",
    "abm5" = "Adams-Bashforth-Moulton - 5-step",
    "msimpson" = "Milne-Simpson",
    "mmsimpson" = "Modified-Milne-Simpson",
    "rkf4" = "Runge-Kutta-Fehlberg-4"
  )
  START_VALUES_VECTOR <- c("feuler", "meuler", "heun3", "rk4")
  TAYLOR_N <- paste0("taylor", 1:9)
  EULER_METHODS <- c("feuler", "meuler")
  EXPLICIT_RK <- c(
    "rkmidpoint", "rkmeuler", "ralston2", "heun3", "nystrom3", "rk3", "rk4",
    "rk38", "rkmersen", "rk5"
  )
  EXPLICIT_MULTISTEP <- c("ab2", "ab3", "ab4", "ab5")
  PREDICTOR_CORRECTOR <- c(
    "eheun", "abm2", "abm3", "abm4", "abm5", "hamming", "msimpson", "mmsimpson"
  )
  ADAPTIVE_VARIABLE_STEP <- c(
    "rkf4", "rfk5", "rkf45", "rkv", "avs", "extra", "tnewton"
  )
  VALID_IVP_METHODS <- c(
    TAYLOR_N, EULER_METHODS, EXPLICIT_RK, EXPLICIT_MULTISTEP, PREDICTOR_CORRECTOR,
    ADAPTIVE_VARIABLE_STEP
  )
  START_VALUES_METHODS <- c(EXPLICIT_MULTISTEP, PREDICTOR_CORRECTOR)

  # ===========================================================================

  method <- match.arg(method)[1]
  f <- stm_function(fexpr = odeqtn, vars = vars, nvars = 2, par_name = "odeqtn")
  if (!is.null(exactsol)) {
    ft <- stm_function(fexpr = exactsol, nvars = 1, par_name = "exactsol")
  } else {
    ft <- NULL
  }
  # taylor
  if (method %in% TAYLOR_N) {
    taylor_order <- stringr::str_sub(method, -1, -1)
    if (!is.null(derivs)) {
      if (length(derivs) == 1) {
        derivs <- strsplit(gsub(", ", ",", derivs), split = ",")[[1]]
      }
    } else {
      s <- check_singular_plural(n = taylor_order)
      stop("'", method, "' expects ", taylor_order, " derivative", s,
           " for the specified odeqtn equation")
    }
  }

  # start method or values
  if (method %in% START_VALUES_METHODS) {
    if (is.character(start_values_or_method)) {
      if (length(start_values_or_method) != 1) {
        start_values_or_method <- "rk4"
      }
      start_method <- start_values_or_method
      start_values <- NULL
    } else {
      start_method <- NULL
      if (start_values_or_method[1] == y0) {
        start_values <- start_values_or_method
      } else {
        start_values <- c(y0, start_values_or_method)
      }
    }
  } else {
    start_method <- NULL
    start_values <- NULL
  }

  methods_n_list = list(
    'ab2' = 1, 'am2' = 1, 'abm2' = 1,
    'ab3' = 2, 'am3' = 2, 'abm3' = 2,
    'ab4' = 3, 'am4' = 3, 'abm4' = 3,
    'ab5' = 4, 'am5' = 4, 'abm5' = 4,
    'hamming' = 3, 'msimpson' = 3, 'mmsimpson' = 3
  )

  start_values_func <- function() {
    if (is.null(start_values)) {
      if (start_method == "feuler"){
        y <- forward_euler(f, t, y)
      } else if (start_method == "meuler") {
        y <- modified_euler(f, t, y)
      } else if (start_method == "heun3") {
        y <- heun3(f, t, y)
      } else if (start_method == "rk4") {
        y <- rk4(f, t, y)
      }
    } else {
      start_values <- check_vector(
        vec = start_values, par_name = "start_values"
      )
      m <- methods_n_list[[method]]
      m = ifelse(is.null(m), 1, m + 1)
      if (length(start_values) != m) {
        stop("Expected ", m, " starting values, got --> ",
             length(start_values), " values instead")
      }
      y[1:m, ] <- matrix(start_values, nrow = m, ncol = 1)
    }
    return(y)
  }
  

  forward_euler <- function(f, t, y) {
    for (i in 1:5) {
      h <- t[i + 1] - t[i]
      y[i + 1, ] <- y[i, ] + h * f(t[i], y[i, ])
    }
    return(y)
  }


  modified_euler <- function(f, t, y) {
    for (i in 1:5) {
      h <- t[i + 1] - t[i]
      ynew <- y[i, ] + h * f(t[i], y[i, ])
      y[i + 1, ] <- y[i, ] + (h / 2) * (f(t[i], y[i, ]) + f(t[i + 1], ynew))
    }
    return(y)
  }


  heun3 <- function(f, t, y) {
    for (i in 1:5) {
      h <- t[i + 1] - t[i]
      k1 <- h * f(t[i], y[i, ])
      k2 <- h * f(t[i] + (1 / 3) * h, y[i, ] + (1 / 3) * k1)
      k3 <- h * f(t[i] + (2 / 3) * h, y[i, ] + (2 / 3) * k2)
      y[i + 1, ] <- y[i, ] + (1 / 4) * (k1 + 3 * k3)
    }
    return(y)
  }


  rk4 <- function(f, t, y) {
    for (i in 1:5) {
      h <- t[i + 1] - t[i]
      k1 <- h * f(t[i], y[i, ])
      k2 <- h * f(t[i] + h / 2, y[i, ] + k1 / 2)
      k3 <- h * f(t[i] + h / 2, y[i, ] + k2 / 2)
      k4 <- h * f(t[i] + h, y[i, ] + k3)
      y[i + 1, ] <- y[i, ] + (1 / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
    }
    return(y)
  }


  time_span <- check_vector(vec = time_span, n = 2, par_name = "time_span")
  t0 <- check_numeric(num = time_span[1], par_name = "time_span:[1]")
  tf <- check_numeric(num = time_span[2], par_name = "time_span:[2]")
  check_limits(
    lower = t0, upper = tf, lower_par_name = "time_span[1]",
    upper_par_name = "time_span[2]"
  )

  index <- stm_get_index(
    dat = ADAPTIVE_VARIABLE_STEP, name_to_find = "avs", is_stop = TRUE
  )
  if (!method %in% ADAPTIVE_VARIABLE_STEP[1:index]) {
    if (is.null(step_size)) { # adaptive methods
      nsteps <- check_numeric(
        num = nsteps, is_integer = TRUE, par_name = "nsteps"
      )
      n <- abs(nsteps)
      h <- (tf - t0) / n
      t <- stm_linspace(t0, tf, n + 1)
      n <- length(t)
    } else {
      step_size <- check_numeric(num = step_size, par_name = "step_size")
      h <- step_size
      t <- stm_arange(from = t0, to = tf, by = h, include_last = TRUE)
      n <- length(t)
    }
  } else { # adaptive methods
    hmin <- check_numeric(num = hmin, par_name = "hmin")
    hmax <- check_numeric(num = hmax, par_name = "hmax")
    check_limits(
      lower = hmin, upper = hmax, lower_par_name = "hmin",
      upper_par_name = "hmax"
    )
    tolerance <- check_numeric(
      num = tolerance, min = 0, max = 1, boundary = "exclusive",
      par_name = "tolerance"
    )
    n <- 10 # placeholder, not required
  }
  y0 <- check_vector( # use vector because of systems of equations
    vec = y0, allow_scalar= TRUE, par_name = "y0"
  )
  maxit <- check_numeric(
    num = maxit, min = 1, is_integer = TRUE, par_name = "maxit"
  )
  m <- if (method == "tnewton") (n + 1) else maxit
  if (is.null(show_iters)) {
    show_iters <- n
  } else {
    show_iters <- check_numeric(
      num = show_iters, min = 1, max = n, is_integer = TRUE,
      par_name = "show_iters"
    )
  }
  show_plot <- check_logical(lgl = show_plot, default = TRUE)
  view_table <- check_logical(lgl = view_table, default = TRUE)

  # initialization
  number_of_odeqtns <- if (length(y0) == 1) 1 else length(y0)
  y <- stm_zeros(nrows = n, ncols = number_of_odeqtns)
  y[1, ] <- y0
  if (method %in% names(methods_n_list)) {
    y <- start_values_func()
  }

  # get all function arguments
  func_args <- list(
    method = method,
    f = f,
    ft = ft,
    derivs = derivs,
    start_values_or_method = start_values_or_method,
    y = y,
    t = t,
    step_size = step_size,
    nsteps = nsteps,
    maxit = maxit,
    tolerance = tolerance,
    hmin = hmin,
    hmax = hmax,
    show_iters = show_iters,
    show_plot = show_plot,
    view_table = view_table,
    digits = digits
  )

  # call the `stm_solve_ivps_functions()` that solves by a given method
  ty_list <- stm_solve_ivps_functions(func_args = func_args)
  t <- ty_list$t
  y <- if (ncol(y) == 1) as.vector(ty_list$y) else ty_list$y
  dframe <- stm_dframe_table(
    t = t, y = y, ft = ft, show_iters = show_iters, digits = digits
  )
  if (view_table) {
    tibble::view(dframe)
  }
  if (show_plot) {
    plot <- stm_plot_results(METHODS_LIST[[method]], t, y, ft)
    plot(plot)
  }
  return(dframe)
}


# =============================================================================
# Helper functions: calling solvers, creating data frame and plotting
# =============================================================================


#' @noRd 
stm_solve_ivps_functions <- function(func_args) {
  method <- func_args$method
  f <- func_args$f
  t <- func_args$t
  y <- func_args$y
  n <- func_args$n
  hmin <- func_args$hmin
  hmax <- func_args$hmax
  tolerance <- func_args$tolerance
  switch(
    method,
    feuler = stm_feuler(f, t, y, n),
    meuler = stm_meuler(f, t, y, n),
    rkmidpoint = stm_rkmidpoint(f, t, y, n),
    rkmeuler = stm_rkmeuler(f, t, y, n),
    ralston2 = stm_ralston2(f, t, y, n),
    heun3 = stm_heun3(f, t, y, n),
    nystrom3 = stm_nystrom3(f, t, y, n),
    rk3 = stm_rk3(f, t, y, n),
    rk4 = stm_rk4(f, t, y, n),
    rk5 = stm_rk5(f, t, y, n),
    rk38 = stm_rk38(f, t, y, n),
    rkmersen = stm_rkmersen(f, t, y, n),
    ab2 = stm_ab2(f, t, y, n),
    ab3 = stm_ab3(f, t, y, n),
    ab4 = stm_ab4(f, t, y, n),
    ab5 = stm_ab5(f, t, y, n),
    eheun = stm_eheun(f, t, y, n),
    abm2 = stm_abm2(f, t, y, n),
    abm3 = stm_abm3(f, t, y, n),
    abm4 = stm_abm4(f, t, y, n),
    abm5 = stm_abm5(f, t, y, n),
    msimpson = stm_msimpson(f, t, y, n),
    mmsimpson = stm_mmsimpson(f, t, y, n),
    rkf4 = {
      yt <- stm_rkf4(f, t, y, hmin, hmax, tolerance)
      list(t = yt$t, y = yt$y)
    },
    stop("'", method, "' is an invalid method")
  )
}


#' @noRd 
stm_dframe_table <- function(t, y, ft, show_iters, digits) {
  if (!is.null(ft)) { # ft is exactsol solution
    y_time_span <- ft(t)
    absolute_error <- abs(y_time_span - y)
    table_results <- t(stm_stack_rows(t, y, y_time_span, absolute_error))
    col_names <- c("Time (t)", "Approx. (yi)", "Exact (y)", "Error: |y - yi|")
  } else {
    table_results <- t(stm_stack_rows(t, y))
    col_names <- c("Time (t)", "Approx. (yi)")
  }
  nrows <- nrow(table_results)
  show_iters <- if (show_iters > nrows) nrows else show_iters
  dframe <- stm_round(
    dat = as.data.frame(table_results[seq_len(show_iters), ]), digits = digits
  )
  names(dframe) <- col_names
  return(dframe)
}


#' @noRd 
stm_plot_results <- function(method, t, y, ft) {
  df <- data.frame(x = t, y = y)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = y, color = "Approx. solution"))
  if (nrow(df) <= 25) {
    p <- p + ggplot2::geom_point(shape = 18, size = 4)
  }
  p <- p + ggplot2::geom_line(size = 1)
  
  if (!is.null(ft)) {
    t_exactsol <- stm_linspace(from = t[1], to = t[length(t)], n = 250)
    yt <- ft(t_exactsol)
    df2 <- data.frame(t_exactsol = t_exactsol, yt = yt)
    p <- p + ggplot2::geom_line(
      dat = df2, 
      ggplot2::aes(x = t_exactsol, y = yt, color = "Exact solution"), 
      size = 1
    )
  }
  
  p <- p + ggplot2::labs(
    x = "Time",
    y = "y = f(t)",
    title = paste0(method, " method"),
    color = "" # legend title
  ) +
    ggplot2::scale_color_manual(
      values = c("Approx. solution" = "blue", "Exact solution" = "red")
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.background = ggplot2::element_rect(color = "gray"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        color = "#0099f9", size = 14, face = "plain", hjust = 0.5
      ),
      plot.subtitle = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position = if (!is.null(ft)) "bottom" else "none"
    )
  p
}


# =============================================================================
# odeqtn solvers
# =============================================================================


# Forward Euler
#' @noRd  
stm_feuler <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    y[i + 1, ] <- y[i, ] + h * f(t[i], y[i, ])
  }
  list(t = t, y = y)
}


# Modified Euler
#' @noRd 
stm_meuler <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    ynew <- y[i, ] + h * f(t[i], y[i, ])
    y[i + 1, ] <- y[i, ] + (h / 2) * (f(t[i], y[i, ]) + f(t[i + 1], ynew))
  }
  list(t = t, y = y)
}


# Midpoint Runge-Kutta
#' @noRd 
stm_rkmidpoint <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + h / 2, y[i, ] + k1 / 2)
    y[i + 1, ] <- y[i, ] + k2
  }
  list(t = t, y = y)
}


# Modified Euler Runge-Kutta
#' @noRd 
stm_rkmeuler <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + h, y[i, ] + k1)
    y[i + 1, ] <- y[i, ] + (1 / 2) * (k1 + k2)
  }
  list(t = t, y = y)
}


# Second order Ralston
#' @noRd 
stm_ralston2 <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + (3 / 4) * h, y[i, ] + (3 / 4) * k1)
    y[i + 1, ] <- y[i, ] + (1 / 3) * (k1 + 2 * k2)
  }
  list(t = t, y = y)
}


# Third order Heun
#' @noRd 
stm_heun3 <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + (1 / 3) * h, y[i, ] + (1 / 3) * k1)
    k3 <- h * f(t[i] + (2 / 3) * h, y[i, ] + (2 / 3) * k2)
    y[i + 1, ] <- y[i, ] + (1 / 4) * (k1 + 3 *  k3)
  }
  list(t = t, y = y)
}


# Third order Nystrom
#' @noRd 
stm_nystrom3 <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + 2 / 3 * h, y[i, ] + 2 / 3 * k1)
    k3 <- h * f(t[i] + 2 / 3 * h, y[i, ] + 2 / 3 * k2)
    y[i + 1, ] <- y[i, ] + (1 / 8) * (2 * k1 + 3 * k2 + 3 * k3)
  }
  list(t = t, y = y)
}


# Third order Runge-Kutta
#' @noRd 
stm_rk3 <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + h / 2, y[i, ] + 1 / 2 * k1)
    k3 <- h * f(t[i] + h, y[i, ] - k1 + 2 * k2)
    y[i + 1, ] <- y[i, ] + (1 / 6) * (k1 + 4 * k2 + k3)
  }
  list(t = t, y = y)
}


# Fourth order Runge-Kutta
#' @noRd 
stm_rk4 <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + h / 2, y[i, ] + k1 / 2)
    k3 <- h * f(t[i] + h / 2, y[i, ] + k2 / 2)
    k4 <- h * f(t[i] + h, y[i, ] + k3)
    y[i + 1, ] <- y[i, ] + 1 / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
  }
  list(t = t, y = y)
}


# Runge-Kutta 3/8
#' @noRd 
stm_rk38 <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + 1 / 3 * h, y[i, ] + 1 / 3 * k1)
    k3 <- h * f(t[i] + 2 / 3 * h, y[i, ] - 1 / 3 * k1 + k2)
    k4 <- h * f(t[i] + h, y[i, ] + k1 - k2 + k3)
    y[i + 1, ] <- y[i, ] + (1 / 8) * (k1 + 3 * k2 + 3 * k3 + k4)
  }
  list(t = t, y = y)
}


# Runge-Kutta-Mersen
#' @noRd 
stm_rkmersen <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + h / 3, y[i, ] + k1 / 3)
    k3 <- h * f(t[i] + h / 2, y[i, ] + k1 / 6 + k2 / 6)
    k4 <- h * f(t[i] + h / 2, y[i, ] + k1 / 8 + 3 / 8 * k3)
    k5 <- h * f(t[i] + h, y[i, ] + k1 / 2 - 3 / 2 * k3 + 2 * k4)
    y[i + 1, ] <- y[i, ] + (1 / 6) * (k1 + 4 * k2 + k5)
  }
  list(t = t, y = y)
}


# Fifth order Runge-Kutta
#' @noRd 
stm_rk5 <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    k1 <- h * f(t[i], y[i, ])
    k2 <- h * f(t[i] + h / 4, y[i, ] + (1 / 4) * k1)
    k3 <- h * f(t[i] + h / 4, y[i, ] + (1 / 8) * k1 + (1 / 8) * k2)
    k4 <- h * f(t[i] + h / 2, y[i, ] - (1 / 2) * k2 + k3)
    k5 <- h * f(t[i] + (3 * h) / 4, y[i, ] + (3 / 16) * k1 + (9 / 16) * k4)
    k6 <- h * f(
      t[i] + h,
      y[i, ] - (3 / 7) * k1 + (2 / 7) * k2 + (12 / 7) * k3 - (12 / 7) * k4
      + (8 / 7) * k5
    )
    y[i + 1, ] <- y[i, ] + (1 / 90) * (
      7 * k1 + 32 * k3 + 12 * k4 + 32 * k5 + 7 * k6
    )
  }
  list(t = t, y = y)
}


# Adams-Bashforth step 2
#' @noRd 
stm_ab2 <- function(f, t, y, n) {
  for (i in 2:n) {
    h <- t[i + 1] - t[i]
    y[i + 1, ] <- y[i, ] + (h / 2) * (
      3 * f(t[i], y[i, ]) - f(t[i - 1], y[i - 1, ])
    )
  }
  list(t = t, y = y)
}


# Adams-Bashforth step 3
#' @noRd 
stm_ab3 <- function(f, t, y, n) {
  for (i in 3:n) {
    h <- t[i + 1] - t[i]
    y[i + 1, ] <- y[i, ] + (h / 12) * (
      23 * f(t[i], y[i, ]) - 16 * f(t[i - 1], y[i - 1, ])
      + 5 * f(t[i - 2], y[i - 2, ])
    )
  }
  list(t = t, y = y)
}


# Adams-Bashforth step 4
#' @noRd 
stm_ab4 <- function(f, t, y, n) {
  for (i in 4:n) {
    h <- t[i + 1] - t[i]
    y[i + 1, ] <- y[i, ] + (h / 24) * (
      55 * f(t[i], y[i, ]) - 59 * f(t[i - 1], y[i - 1])
      + 37 * f(t[i - 2], y[i - 2]) - 9 * f(t[i - 3], y[i - 3])
    )
  }
  list(t = t, y = y)
}


# Adams-Bashforth step 5
#' @noRd 
stm_ab5 <- function(f, t, y, n) {
  for (i in 5:n) {
    h <- t[i + 1] - t[i]
    y[i + 1, ] <- y[i, ] + (h / 720) * (
      1901 * f(t[i], y[i, ]) - 2774 * f(t[i - 1], y[i - 1])
      + 2616 * f(t[i - 2], y[i - 2]) - 1274 * f(t[i - 3], y[i - 3])
      + 251 * f(t[i - 4], y[i - 4])
    )
  }
  list(t = t, y = y)
}


# Euler-Heun
#' @noRd 
stm_eheun <- function(f, t, y, n) {
  for (i in seq_len(n)) {
    h <- t[i + 1] - t[i]
    # Explicit Euler as predictor
    y[i + 1, ] <- y[i, ] + h * f(t[i], y[i, ])
    # Heun as corrector
    y[i + 1, ] <- y[i, ] + (h / 2) * (
      f(t[i + 1], y[i + 1, ]) + f(t[i], y[i, ])
    )
  }
  list(t = t, y = y)
}


# Adams-Bashforth-Moulton step 2
#' @noRd 
stm_abm2 <- function(f, t, y, n) {
  for (i in 2:n) {
    h <- t[i + 1] - t[i]
    # Adams-Bashforth 2-step as predictor
    y[i + 1, ] <- y[i, ] + (h / 2) * (
      3 * f(t[i], y[i, ]) - f(t[i - 1], y[i - 1])
    )
    # Adams-Moulton 2-step as corrector
    y[i + 1, ] <- y[i, ] + (h / 2) * (
      f(t[i + 1], y[i + 1, ]) + f(t[i], y[i, ])
    )
  }
  list(t = t, y = y)
}


# Adams-Bashforth-Moulton step 3
#' @noRd 
stm_abm3 <- function(f, t, y, n) {
  for (i in 3:n) {
    h <- t[i + 1] - t[i]
    # Adams-Bashforth 3-step as predictor
    y[i + 1, ] <- y[i, ] + (h / 12) * (
      23 * f(t[i], y[i, ]) - 16 * f(t[i - 1], y[i - 1])
      + 5 * f(t[i - 2], y[i - 2])
    )
    # Adams-Moulton 2-step as corrector
    y[i + 1, ] <- y[i, ] + (h / 12) * (
      5 * f(t[i + 1], y[i + 1, ]) + 8 * f(t[i], y[i, ])
      - f(t[i - 1], y[i - 1])
    )
  }
  list(t = t, y = y)
}


# Adams-Bashforth-Moulton step 4
#' @noRd 
stm_abm4 <- function(f, t, y, n) {
  for (i in 4:n) {
    h <- t[i + 1] - t[i]
    # Adams-Bashforth 4-step as predictor
    y[i + 1, ] <- y[i, ] + (h / 24) * (
      55 * f(t[i], y[i, ]) - 59 * f(t[i - 1], y[i - 1])
      + 37 * f(t[i - 2], y[i - 2]) - 9 * f(t[i - 3], y[i - 3])
    )
    # Adams-Moulton 3-step as corrector
    y[i + 1, ] <- y[i, ] + (h / 24) * (
      9 * f(t[i + 1], y[i + 1, ]) + 19 * f(t[i], y[i, ])
      - 5 * f(t[i - 1], y[i - 1]) + f(t[i - 2], y[i - 2])
    )
  }
  list(t = t, y = y)
}


# Adams-Bashforth-Moulton step 5
#' @noRd 
stm_abm5 <- function(f, t, y, n) {
  for (i in 5:n) {
    h <- t[i + 1] - t[i]
    # Adams-Bashforth 5-step as predictor
    y[i + 1, ] <- y[i, ] + (h / 720) * (
      1901 * f(t[i], y[i, ]) - 2774 * f(t[i - 1], y[i - 1])
      + 2616 * f(t[i - 2], y[i - 2]) - 1274 * f(t[i - 3], y[i - 3])
      + 251 * f(t[i - 4], y[i - 4])
    )
    # Adams-Moulton 4-step as corrector
    y[i + 1, ] <- y[i, ] + (h / 720) * (
      251 * f(t[i + 1], y[i + 1, ]) + 646 * f(t[i], y[i, ])
      - 264 * f(t[i - 1], y[i - 1]) + 106 * f(t[i - 2], y[i - 2])
      - 19 * f(t[i - 3], y[i - 3])
    )
  }
  list(t = t, y = y)
}


# Milne-Simpson
#' @noRd 
stm_msimpson <- function(f, t, y, n) {
  for (i in 4:n) {
    h <- t[i + 1] - t[i]
    # Milne as predictor
    y[i + 1, ] <- y[i - 3] + (4 * h / 3) * (
      2 * f(t[i], y[i, ]) - f(t[i - 1], y[i - 1]) + 2 * f(t[i - 2], y[i - 2])
    )
    # Hamming as corrector
    y[i + 1, ] <- (9 * y[i, ] - y[i - 2]) / 8 + (3 * h / 8) * (
      f(t[i + 1], y[i + 1, ]) + 2 * f(t[i], y[i, ]) - f(t[i - 1], y[i - 1])
    )
  }
  list(t = t, y = y)
}


# Modified Milne-Simpson
#' @noRd 
stm_mmsimpson <- function(f, t, y, n) {
  for (i in 4:n) {
    h <- t[i + 1] - t[i]
    # Milne as predictor
    y[i + 1, ] <- y[i - 3] + (4 * h / 3) * (
      2 * f(t[i], y[i, ]) - f(t[i - 1], y[i - 1]) + 2 * f(t[i - 2], y[i - 2])
    )
    # Simpson as corrector
    y[i + 1, ] <- y[i - 1] + (h / 3) * (
      f(t[i + 1], y[i + 1, ]) + 4 * f(t[i], y[i, ]) + f(t[i - 1], y[i - 1])
    )
  }
  list(t = t, y = y)
}


# Runge-Kutta-Fehlberg order 4
#' @noRd 
stm_rkf4 <- function(f, t, y, hmin, hmax, tolerance) {
  # a
  a2 <- 1 / 4
  a3 <- 3 / 8
  a4 <- 12 / 13
  a5 <- 1
  a6 <- 1 / 2
  # b
  b21 <- 1 / 4
  b31 <- 3 / 32
  b32 <- 9 / 32
  b41 <- 1932 / 2197
  b42 <- -7200 / 2197
  b43 <- 7296 / 2197
  b51 <- 439 / 216
  b52 <- -8
  b53 <- 3680 / 513
  b54 <- -845 / 4104
  b61 <- -8 / 27
  b62 <- 2
  b63 <- -3544 / 2565
  b64 <- 1859 / 4104
  b65 <- -11 / 40
  # c
  c1 <- 25 / 216
  c3 <- 1408 / 2565
  c4 <- 2197 / 4104
  c5 <- -1 / 5
  # r
  r1 <- 1 / 360
  r3 <- -128 / 4275
  r4 <- -2197 / 75240
  r5 <- 1 / 50
  r6 <- 2 / 55
  # begin
  t0 <- 0
  tf <- 2
  t <- t0
  tt <- t0
  y <- y[1]
  yy <- y
  h <- hmax
  flag <- 1
  while (flag == 1) {
    k1 <- h * f(t, y)
    k2 <- h * f(t + a2 * h, y + b21 * k1)
    k3 <- h * f(t + a3 * h, y + b31 * k1 + b32 * k2)
    k4 <- h * f(t + a4 * h, y + b41 * k1 + b42 * k2 + b43 * k3)
    k5 <- h * f(t + a5 * h, y + b51 * k1 + b52 * k2 + b53 * k3 + b54 * k4)
    k6 <- h * f(
      t + a6 * h, y + b61 * k1 + b62 * k2 + b63 * k3 + b64 * k4 + b65 * k5
    )
    r <- (1 / h) * abs(r1 * k1 + r3 * k3 + r4 * k4 + r5 * k5 + r6 * k6)
    r <- r[1]
    if (r <= tolerance) {
      t <- t + h
      y <- y + c1 * k1 + c3 * k3 + c4 * k4 + c5 * k5
      tt <- c(tt, t)
      yy <- c(yy, y)
    }
    delta <- 0.84 * (tolerance / r) ^ (1 / 4)
    if (delta <= 0.1) {
      h <- 0.1 * h
    } else if (delta >= 4) {
      h <- 4 * h
    } else {
      h <- delta * h
    }
    h <- ifelse(h > hmax, hmax, h)
    if (t >= tf) {
      flag <- 0
    } else if (t + h > tf) {
      h <- tf - t
    } else if (h < hmin) {
      flag <- 0
    }
  }
  return(list(t = tt, y = yy))
}
