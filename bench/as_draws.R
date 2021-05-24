# Benchmarks for conversion between draws formats
library(posterior)

iterations <- 500
chains <- 4
I <- 30
J <- 30
x_array <- as_draws_array(array(1:(iterations*chains*I*J), dim = c(iterations, chains, I*J), dimnames = list(
  NULL, NULL, paste0("x[", rep(as.roman(1:I), each = J), ",", rep(as.roman(1:J), I), "]")
)))

# conversion from draws_array
bench::mark(check = FALSE, min_iterations = 20, filter_gc = FALSE,
  as_draws_matrix(x_array),
  as_draws_list(x_array),
  as_draws_df(x_array),
  as_draws_rvars(x_array)
)

# conversion from draws_matrix
x_matrix <- as_draws_matrix(x_array)
bench::mark(check = FALSE, min_iterations = 20, filter_gc = FALSE,
  as_draws_array(x_matrix),
  as_draws_list(x_matrix),
  as_draws_df(x_matrix),
  as_draws_rvars(x_matrix)
)

# conversion from draws_list
x_list <- as_draws_list(x_array)
bench::mark(check = FALSE, min_iterations = 20, filter_gc = FALSE,
  as_draws_array(x_list),
  as_draws_matrix(x_list),
  as_draws_df(x_list),
  as_draws_rvars(x_list)
)

# conversion from draws_df
x_df <- as_draws_df(x_array)
bench::mark(check = FALSE, min_iterations = 20, filter_gc = FALSE,
  as_draws_array(x_df),
  as_draws_matrix(x_df),
  as_draws_list(x_df),
  as_draws_rvars(x_df)
)

# conversion from draws_rvars
x_rvars <- as_draws_rvars(x_array)
bench::mark(check = FALSE, min_iterations = 20, filter_gc = FALSE,
  as_draws_array(x_rvars),
  as_draws_matrix(x_rvars),
  as_draws_list(x_rvars),
  as_draws_df(x_rvars)
)
