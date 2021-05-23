#' Rename variables in `draws` objects
#'
#' Rename variables in a [`draws`] object.
#'
#' @param .x (draws) A [`draws`] object.
#' @param ... One or more expressions, separated by commas, indicating the
#'   variables to rename. The variable names can be unquoted
#'   (`new_name = old_name`) or quoted (`"new_name" = "old_name"`). For non-scalar
#'   variables, all elements can be renamed together (`"new_name" = "old_name"`)
#'   or they can be renamed individually (`"new_name[1]" = "old_name[1]"`).
#'
#' @return
#' Returns a [`draws`] object of the same format as `.x`, with variables renamed
#' according to the expressions provided in `...`.
#'
#' @seealso [`variables`], [`mutate_variables`]
#'
#' @examples
#' x <- as_draws_df(example_draws())
#' variables(x)
#'
#' x <- rename_variables(x, mean = mu, sigma = tau)
#' variables(x)
#'
#' x <- rename_variables(x, b = `theta[1]`) # or b  = "theta[1]"
#' variables(x)
#'
#' # rename all elements of 'theta' at once
#' x <- rename_variables(x, alpha = theta)
#' variables(x)
#'
#' @importFrom rlang quos as_name
#' @export
rename_variables <- function(.x, ...) {
  UseMethod("rename_variables")
}

#' @rdname rename_variables
#' @export
rename_variables.draws <- function(.x, ...) {
  old_names <- sapply(quos(...), as_name)
  new_names <- names(old_names)

  if (any(new_names == "")) {
    old_names_without_new_name = old_names[new_names == ""]
    stop_no_call(
      "Cannot rename a variable to an empty name.\n",
      "The following variables did not have a new name provided:\n",
      comma(old_names_without_new_name)
    )
  }

  # loop over names as every old name may correspond to multiple
  # scalar variables if the name targets a non-scalar variable
  # also this allows renaming operations to be chained
  for (i in seq_along(old_names)) {
    old_names_i <- check_existing_variables(old_names[i], .x)
    v_regex <- paste0("^", escape_all(old_names[i]))
    v_indices <- sub(v_regex, "", old_names_i)
    new_names_i <- paste0(new_names[i], v_indices)
    sel <- which(variables(.x) %in% old_names_i)
    variables(.x)[sel] <- new_names_i
  }
  check_new_variables(variables(.x))
  .x
}
