# tranform posterior object to another representation
transform_posterior_format <- function(x, to, from = NULL) {
  from <- from %||% detect_draws_format(x)
  assert_choice(to, all_posterior_formats())
  assert_choice(from, all_posterior_formats())
  fun <- paste0("posterior_", from, "_to_", to)
  fun <- get(fun, pos = asNamespace("posterior"))
  fun(x)
}

posterior_matrix_to_matrix <- function(x) {
  x
}

posterior_matrix_to_array <- function(x) {
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  x <- rray_reshape(x, dim = c(old_dim[1], 1, old_dim[2]))
  dimnames(x) <- list(
    iteration = as.character(seq_rows(x)),
    chain = "1",
    variable = old_dimnames[[2]]
  )
  x
}

posterior_array_to_matrix <- function(x) {
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  x <- rray_reshape(x, dim = c(old_dim[1] * old_dim[2], old_dim[3]))
  dimnames(x) <- list(
    draw = as.character(seq_rows(x)),
    variable = old_dimnames[[3]]
  )
  x
}

posterior_array_to_array <- function(x) {
  x
}
