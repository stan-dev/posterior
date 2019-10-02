# In this file, the transformation functions should be ordered in row major
# order if the original class is in the rows and the target class is in the
# columns

posterior_matrix_to_matrix <- function(x) {
  assert_true(is_posterior_matrix(x))
  x
}

posterior_matrix_to_array <- function(x) {
  assert_true(is_posterior_matrix(x))
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  x <- rray_reshape(x, dim = c(old_dim[1], 1, old_dim[2]))
  dimnames(x) <- list(
    iter = as.character(seq_rows(x)),
    chain = "1",
    par = old_dimnames[[2]]
  )
  x
}

posterior_array_to_matrix <- function(x) {
  assert_true(is_posterior_array(x))
  old_dim <- dim(x)
  old_dimnames <- dimnames(x)
  x <- rray_reshape(x, dim = c(old_dim[1] * old_dim[2], old_dim[3]))
  dimnames(x) <- list(
    draw = as.character(seq_rows(x)),
    par = old_dimnames[[3]]
  )
  x
}

posterior_array_to_array <- function(x) {
  assert_true(is_posterior_array(x))
  x
}
