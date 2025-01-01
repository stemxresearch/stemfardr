#' Solve initial value problems (IVPs) using numerical methods
#'
#' \code{stm_ivps*()} set of functions solve IVPs for ordinary differential 
#' equations (ODEs) using various numerical methods. Supports solving 
#' single equations or systems of ODEs.
#'
#' @param odeqtn A function defining the ODE system. It should take two 
#'               arguments: the current time and state vector as 
#'               \code{y = f(t, y)}.
#' @param exactsol Optional. A function representing the exact solution for
#'              comparison. Default is \code{NULL}.
#' @param vars Independent and dependent variables.
#' @param time_span A numeric vector specifying the time range for the
#'                  solution. Should be of length \code{2}.
#' @param y0 Initial state values for the ODE system.
#' @param step_size The step size for the numerical method. If \code{NULL},
#'                  it is computed based on the time span and number of steps.
#' @param nsteps The number of time steps to take. Default is \code{10}.
#' @param maxit Maximum number of iterations for iterative methods.
#'              Default is \code{10}.
#' @param show_iters Logical. If \code{TRUE}, iteration details are displayed.
#'                   Default is \code{NULL}.
#' @param view_table Logical. If \code{TRUE}, the results are viewed in the 
#'                   R spreadsheet. Default is \code{FALSE}.
#' @param show_plot Logical. If \code{TRUE}, the results are plotted. Default 
#'                  is \code{TRUE}.
#' @param digits The number of significant digits to display in the results.
#'               Default is \code{8}.
#'
#' @return A data frame containing the time points and corresponding solution
#'         values. If an exact solution is provided, the error at each time
#'         point is also included.
#' \itemize{
#'     \item \strong{Time (t)}: Time points.
#'     \item \strong{Approx. (yi)}: State values at each time point.
#'     \item \strong{Exact (y)}: Exact solution if \code{exactsol} is provided.
#'     \item \strong{Error: |y - yi|}: The error at each time point if 
#'           \code{exactsol} is provided.
#' }
#'
#' @details
#' These functions solve initial value problems for ODEs using a specified
#' numerical method. They can handle both single ODEs and systems of ODEs by
#' providing state variables as a vector. If an exact solution is provided,
#' the error at each time point will be calculated for comparison.
