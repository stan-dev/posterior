#' Execute expressions of random variables
#'
#' Execute an expression to create a random variable.
#'
#' @param expr A bare expression that can (optionally) contain [rvar]s. The expression
#' supports [quasiquotation].
#' @param ndraws When no [rvar]s are supplied in `expr`, this is the number
#' of draws that will be used to construct new random variables. If `NULL`,
#' getOption("rvar.ndraws") is used (default 4000).
#' @template args-rvar-dim
#'
#' @details This function evaluates `expr` possibly multiple times, once for each draw of
#' the [rvar]s it contains, then returns a new [rvar] representing the output of those
#' expressions. To identify [rvar]s, `rdo()` searches the calling environment for any variables
#' named in `expr` for which [is_rvar()] evaluates to `TRUE`. If `expr` contains no [rvar]s,
#' then it will be executed `ndraws` times and an [rvar] with that many draws returned.
#'
#' `rdo()` is not necessarily *fast* (in fact in some cases it may be very slow), but
#' it has the advantage of allowing nearly arbitrary R code to be executed against [rvar]s
#' simply by wrapping it with `rdo( ... )`. This makes it especially useful as a prototyping
#' tool. If you create code with `rdo()` and it is unacceptably slow for your application,
#' consider rewriting it in terms of math operations directly on [rvar]s (which should be fast),
#' or in terms of operations directly on the arrays that back the [rvar]s, using [draws_of()].
#'
#' @return An [rvar].
#'
#' @examples
#'
#' mu <- rdo(rnorm(1, 0, 1))
#' sigma <- rdo(rgamma(1, 1, 1))
#' x <- rdo(rnorm(10, mu, sigma))
#' x
#'
#' @importFrom rlang eval_tidy quo_get_env enquo missing_arg quo_get_expr
#' @export
rdo <- function(expr, dim = NULL, ndraws = NULL) {
  ndraws <- ndraws %||% getOption("rvar.ndraws", 4000)

  # basic idea here is to find all the variables that are used in the expression
  # and which are also random variables in the expression's environment, then
  # build a function that executes the expression and takes those random
  # variables as arguments, then vectorize that function using `rfun()` and execute it.
  f_expr <- enquo(expr)
  f_env <- quo_get_env(f_expr)

  rvars_in_expr <- list()
  f_alist <- alist()
  for (name in all.vars(f_expr)) {
    var <- get(name, f_env)
    if (is_rvar(var)) {
      rvars_in_expr[[name]] <- var
      f_alist[[name]] <- missing_arg()
    }
  }

  f_alist <- append(f_alist, quo_get_expr(f_expr))
  f <- rfun(as.function(f_alist, envir = f_env), ndraws = ndraws)
  result <- do.call(f, rvars_in_expr, envir = f_env)

  if (!is.null(dim)) {
    dim(result) <- dim
  }
  result
}
