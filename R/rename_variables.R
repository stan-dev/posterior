#' Rename Variables in Draws Objects
#'
#' Rename variables within a [`draws`] object.
#'
#' @param .x A [`draws`] object.
#' @param ... One or more expressions separated by commas indicating the variables
#'   to rename. Existing variable names can be unquoted expressions, as in
#'   `new_name = old_name`, or character vectors, as in `new_name = "old_name"`.
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
    stop2(
      "Cannot rename a variable to an empty name.\n",
      "The following variables did not have a new name provided:\n",
      comma(old_names_without_new_name)
    )
  }

  check_existing_variables(old_names, .x)
  old_names_index <- match(old_names, variables(.x))
  variables(.x)[old_names_index] <- new_names
  .x
}
