#' Rename variables in `draws` objects
#'
#' Rename variables within a [`draws`] object.
#'
#' @param .x A [`draws`] object.
#' @param ... One or more expressions separated by commas indicating the variables
#'   to rename. Existing variable names can be unquoted expressions, as in
#'   `new_name = old_name`, or character vectors, as in `new_name = "old_name"`.
#'   All elements of non-scalar variables can be renamed at once.
#'
#' @return
#' Returns a [`draws`] object of the same format as `.x`, with variables renamed
#' according to the expressions provided in `...`.
#'
#' @importFrom rlang quos as_name
#'
#' @seealso [`variables`], [`mutate_variables`]
#'
#' @examples
#' x <- as_draws_df(example_draws())
#'
#' x
#' rename_variables(x, mean = mu, sigma = tau)
#' rename_variables(x, b = `theta[1]`)
#' rename_variables(x, b = "theta[1]")
#'
#' # rename all elements of 'theta' at once
#' rename_variables(x, alpha = theta)
#'
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
