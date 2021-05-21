#' "Roll up" `draws_summary` objects by collapsing over nonscalar parameters.
#' 
#' By default, all variables containing `[` in the name are rolled up, but 
#' there is an option to pass a list of parameter names, which will roll up
#' any variables that match `^parameter_name\\[.*`
#' 
#' @name draws_summary_rollup
#' @param x a `draws_summary` object or a `draws` object to be summarised
#' @param ... Optional arguments to be passed to `summarise_draws` if x is a `draws` object
#' @param rollup_vars a list of variable names (excluding brackets and indices) to roll up
#' @param min_only a character vector of varable names for which only minimum values are 
#'    desired in the rollup
#' @param max_only a character vector of varable names for which only maximum values are 
#'    desired in the rollup
#'
#' @return
#' The `rollup_summary()` methods return a list of [tibble][tibble::tibble] data frames.
#' The first element is a standard `draws_summary` for the variables that are not rolled up
#' The second element is a rollup of the variables to be rolled up and contains max and min
#' values of the summary functions attained by any element of the variable
#' 
#' @details
#' By default, only the maximum value of `rhat` and the mimum values of [ess_bulk()] and 
#' [ess_tail()] are returned.  `NA`s are ignored unless all elements of the summary are `NA`
#' 
#' @examples
#' ds <- summarise_draws(example_draws())
#' ds2 <- summarise_draws(2 * example_draws())
#' ds2$variable <- c("pi", "upsilon", 
#'                   "omega[1,1]", "omega[2,1]", "omega[3,1]", "omega[4,1]",
#'                                     "omega[1,2]", "omega[2,2]", "omega[3,2]", "omega[4,2]")
#' draws_summary <- rbind(ds, ds2)
#' rollup_summary(draws_summary)
#' rollup_summary(draws_summary, rollup_vars = "theta")
#' rollup_summary(example_draws())
NULL

#' @rdname draws_summary_rollup
#' @export
rollup_summary <- function(x, rollup_vars = NULL,
                           min_only = c("ess_bulk", "ess_tail"),
                           max_only = "rhat") {
  UseMethod("rollup_summary")
}

#' @export
rollup_summary.default <- function(x, ..., rollup_vars = NULL,
                                   min_only = c("ess_bulk", "ess_tail"),
                                   max_only = "rhat") {
  rollup_summary(summarise_draws(x, ...), rollup_vars = NULL,
                 min_only = c("ess_bulk", "ess_tail"),
                 max_only = "rhat")
}

#' @rdname draws_summary_rollup
#' @export
rollup_summary.draws_summary <- function (x, rollup_vars = NULL,
                            min_only = c("ess_bulk", "ess_tail"),
                            max_only = "rhat") {
  # get variable names
  vars <- draws_summary$variable
  # Determine which variable names need to be rolled up
  if (is.null(rollup_vars)) {
    vars_nonscalar <- grepl("\\[", vars)
  } else {
    vars_nonscalar <- as.logical(colSums(do.call(rbind, 
                                                 lapply(paste0("^", rollup_vars, "\\["),
                                                        function(x){grepl(x, vars)}))))
  }
  # Separate out draws_summary into the scalar variables to leave alone and the nonscalar
  # variables for rollup
  ds_scalar <- draws_summary[!vars_nonscalar, ]
  ds_nonscalar <- draws_summary[vars_nonscalar, ]
  # Roll up the nonscalar variables
  varnames_nonscalar <- gsub("\\[(.*)", "", ds_nonscalar$variable)
  summary_names <- names(draws_summary)[-1]
  names_minmax <- summary_names[!(summary_names %in% c(min_only, max_only))]
  split_nonscalar <- split(ds_nonscalar, varnames_nonscalar)[unique(varnames_nonscalar)] 
                          # [unique(varnames_nonscalar)] preserves the order of the names
  min_max <- do.call(rbind, lapply(split_nonscalar, rollup_helper_minmax, 
                                   names = names_minmax))
  min_only <- do.call(rbind, lapply(split_nonscalar, rollup_helper_min, names = min_only))
  max_only <- do.call(rbind, lapply(split_nonscalar, rollup_helper_max, names = max_only))
  variable_column <- data.frame("variable" = unique(varnames_nonscalar))
  indices <- get_indices(ds_nonscalar$variable)
  get_dims <- function(x){paste0("(", 
                                 paste(apply(x[, names(x) != "index", drop = FALSE], 2, function(x){max(as.integer(x))}), collapse = ","),
                                 ")")}
  dimensions <- lapply(indices, get_dims)
  dimension_column <- data.frame("dimension" = unlist(dimensions))
  nonscalar_out <- tibble::as_tibble(cbind(variable_column, dimension_column, min_max, max_only, min_only))
  out <- list(unrolled_vars = ds_scalar, rolled_vars = nonscalar_out)
  out
}

rollup_helper_minmax <- function(x, names){
  x <- x[, names]
  mm <- c(apply(x, 2, function(x) {c(min(x), max(x))}))
  names(mm) <- paste0(rep(names(x), each = 2), c("_min", "_max"))
  mm
}

rollup_helper_min <- function(x, names){
  x <- x[, names]
  min_only <- apply(x, 2, min)
  names(min_only) <- paste0(names(x), "_min")
  min_only
}

rollup_helper_max <- function(x, names){
  x <- x[, names]
  max_only <- apply(x, 2, max)
  names(max_only) <- paste0(names(x), "_max")
  max_only
}
