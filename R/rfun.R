#' Construct random variable functions
#'
#' Function to create functions that can accept and/or produce random variables.
#'
#' @param .f A function (or a one-sided formula representing a function that can be parsed by
#' [rlang::as_function()]) to turn into a function that accepts and/or produces random variables.
#' @param rvar_args The arguments of `.f` that should be allowed to accept [rvar]s as arguments.
#' If `NULL` (the default), all arguments to `.f` are turned into arguments that accept
#' [rvar]s.
#' @param .ndraws When no [rvar]s are supplied as arguments to the new function, this is the number
#' of draws that will be used to construct random variables returned by the new function.
#'
#' @details This function wraps an existing funtion (`.f`) such that it returns [rvar]s containing
#' whatever type of data `.f` would normally return.
#'
#' @return A function with the same argument specification as `.f`, but which can accept and return
#' [rvar]s.
#'
#' @export
rfun = function (.f, rvar_args = NULL, .ndraws = 4000) {
  # based on base::Vectorize
  .f = rlang::as_function(.f)

  arg_names <- as.list(formals(.f))
  arg_names[["..."]] <- NULL
  arg_names <- names(arg_names)
  rvar_args <- as.character(rvar_args %||% arg_names)

  if (!all(rvar_args %in% arg_names)) {
    stop("must specify names of formal arguments for 'rfun'")
  }

  collisions <- arg_names %in% c("FUN", "SIMPLIFY", "USE.NAMES")
  if (any(collisions)) {
    stop2("'.f' may not have argument(s) named ", comma(arg_names[collisions]))
  }

  FUNV <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    arg_names <-
      if (is.null(names(args))) character(length(args))
      else names(args)

    is_rvar_arg <- (arg_names %in% rvar_args) & as.logical(lapply(args, is_rvar))
    rvar_args = lapply(args[is_rvar_arg], function(x) x$draws)

    if (length(rvar_args) == 0) {
      # no rvar arguments, so just create a random variable by applying this function
      # .ndraws times
      rvar(replicate(.ndraws, do.call(.f, args), simplify = FALSE))
    } else {
      rvar(do.call(mapply, c(FUN = .f, rvar_args, MoreArgs = list(args[!is_rvar_arg]),
        SIMPLIFY = FALSE, USE.NAMES = FALSE)))
    }
  }
  formals(FUNV) <- formals(.f)
  FUNV
}

#' @importFrom rlang eval_tidy quo_get_env enquo missing_arg quo_get_expr
rdo = function(.expr, .ndraws = 4000) {
  # basic idea here is to find all the variables that are used in the expression
  # and which are also random variables in the expression's environment, then
  # build a function that executes the expression and takes those random
  # variables as arguments, then vectorize that function using `rfun()` and execute it.
  f_expr = enquo(.expr)
  f_env = quo_get_env(f_expr)

  rvars_in_expr = list()
  f_alist = alist()
  for (name in all.vars(f_expr)) {
    var = get(name, f_env)
    if (is_rvar(var)) {
      rvars_in_expr[[name]] = var
      f_alist[[name]] = missing_arg()
    }
  }

  f_alist = append(f_alist, quo_get_expr(f_expr))
  f = rfun(as.function(f_alist, envir = f_env), .ndraws = .ndraws)
  do.call(f, rvars_in_expr, envir = f_env)
}
