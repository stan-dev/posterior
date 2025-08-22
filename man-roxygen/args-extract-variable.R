#' @param variable (string) The name of the variable to extract. The name must
#'   correspond to a scalar variable or must include indices for array variables
#'   (e.g. `"x[1]"`, `"y[1,2]"`) that result in a scalar variable. To extract
#'   all dimensions from variables with indices, use [extract_variable_array()].
