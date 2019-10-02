posterior_matrix_to_matrix <- function(x) {
  x
}

posterior_array_to_array <- function(x) {
  x
}

posterior_array_to_matrix <- function(x) {
  assert_class(x, "posterior_array")
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  x <- rray_reshape(x, dim = c(old_dim[1] * old_dim[2], old_dim[3]))
  dimnames(x) <- list(
    draw = as.character(seq_rows(x)),
    par = old_dimnames[[3]]
  )
  change_class(x) <- "posterior_matrix"
  x
}

posterior_matrix_to_array <- function(x) {
  assert_class(x, "posterior_matrix")
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  x <- rray_reshape(x, dim = c(old_dim[1], 1, old_dim[2]))
  dimnames(x) <- list(
    iter = as.character(seq_rows(x)),
    chain = "1",
    par = old_dimnames[[2]]
  )
  change_class(x) <- "posterior_array"
  x
}
