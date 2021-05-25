#' Parse the indices from a vector of variables (extracted e.g. from a 
#' `draws_summary` object or a `draws` object)
#' 
#' @name variable_indices
#' @param x a character vector of variables
#'
#' @return
#' A list with index information for each unique variable name V in `x`. Top-level list names are 
#' the variable names. Each element contains: 
#'    $ndim  the number of dimensions of V. Returns 0 for scalars with no brackets
#'    but 1 for `y[1]` even if `y` has no other entries in `x`.
#'    
#'    $dim  a vector of the actual dimensions of V, as determined by the number of unique
#'    elements at each index position. Set to `NA` if the ndim is zero.
#'    
#'    $implied_dim  a vector of the implied dimensions of V, where any position in V that 
#'    contains exclusively integers is filled in to include all integers from the lesser of one 
#'    and its minimum up to its maximum. Set to `NA` if ndim is zero.
#'    
#'    $index_names  a list of length corresponding to ndim, where each element is the
#'    unique levels of the corresponding index if the index is parsed as factor, and NULL otherwise.
#'    Set to `NULL` if ndim is zero.
#'    
#'    $indices  if ndim is zero, returns 1.
#'    if ndim is 1 or greater, returns a dataframe of every implied combination of indices
#'    
#'    $position  the position of each combination of indices from $indices in the the argument `x`
#' 
#' @details
#' Assumes that variable indexing uses square brackets in the variable names
#' 
NULL

parse_variable_indices <- function(x){
  vars_indices <- strsplit(x, "(\\[|\\])")
  vars <- sapply(vars_indices, `[[`, 1)
  var_names <- unique(vars)
  # Check that no variables contain unpaired or non-terminal square brackets
  if_indexed <- lengths(vars_indices) > 1
  if_indexed2 <- grepl("\\[.*\\]$", x)
  extra_bracket <- grepl("\\].", x) | grepl("\\[.*\\[", x)
  if (any(extra_bracket) | (!identical(if_indexed, if_indexed2))) {
    stop_no_call(paste("Some variable names contain unpaired square brackets",
                  "or are multi-indexed."))
  }
  missing_index <- grepl("\\[,|,\\]|,,", x) # check for `[,` `,,` or `,]`
  if (any(missing_index)) {
    stop_no_call(paste("Some variables contain missing indices. Each comma between square", 
                       "brackets must be both preceeded and succeeded by an index."))
  }

  # Get ndim. Variables with no brackets are given as ndim zero.
  # Variables with brackets are given ndim 1, even if they contain just one element.
  ndim_elementwise <- sapply(vars_indices, function(x){
    if (length(x) == 2) {
      out <- length(strsplit(x[2], ",")[[1]]) # number of commas plus one
    } else {
      out <- 0
    }
    out
  })
  ndim <- sapply(var_names, function(x){
    out <- unique(ndim_elementwise[vars == x])
    if (length(out) != 1) {
      stop_no_call(paste0("Inconsistent indexing found for variable ", x, " ."))
    }
    out
  })
  variable_indices_info <- lapply(var_names, function (x) {
    indices_info <- named_list(c("ndim", "dimensions", "implied_dimensions", "index_names", "indices", "position"))
    indices_info$ndim <- ndim[[x]]
    var_i <- vars == x
    var_length <- sum(var_i)
    if (ndim[x] == 0) {
      # single variable, no indices
      indices_info$dimensions <- indices_info$implied_dimensions <- NA
      indices_info$indices <- 1L
      indices_info$implied_dimensions <- NA
      indices_info$internal_position <- 1
    } else {
      indices <- sapply(vars_indices[var_i], `[[`, 2)
      indices <- as.data.frame(do.call(rbind, strsplit(indices, ",")),
                               stringsAsFactors = FALSE)
      indices_info$dimensions <- unname(apply(indices, 2, function(x){length(unique(x))}))
      unique_indices <- vector("list", length(indices))
      .dimnames <- vector("list", length(indices))
      names(unique_indices) <- names(indices)
      for (i in seq_along(indices)) {
        numeric_index <- suppressWarnings(as.numeric(indices[[i]]))
        if (!anyNA(numeric_index) && rlang::is_integerish(numeric_index)) {
          # for integer indices, we need to convert them to integers
          # so that we can sort them in numerical order (not string order)
          if (min(numeric_index) >= 1) {
            # integer indices >= 1 are forced to lower bound of 1 + no dimnames
            indices[[i]] <- as.integer(numeric_index)
            unique_indices[[i]] <- seq.int(1, max(numeric_index))
          } else {
            # indices with values < 1 are filled in between the min and max
            indices[[i]] <- numeric_index
            unique_indices[[i]] <- seq.int(min(numeric_index), max(numeric_index))
          }
        } else {
          # we convert non-numeric indices to factors so that we can force them
          # to be ordered as they appear in the data (rather than in alphabetical order)
          factor_levels <- unique(indices[[i]])
          indices[[i]] <- factor(indices[[i]], levels = factor_levels)
          # these aren't sorted so they appear in original order
          unique_indices[[i]] <- factor(factor_levels, levels = factor_levels)
          .dimnames[[i]] <- unique_indices[[i]]
        }
      }
      
      indices_info$index_names <- .dimnames
      indices_info$implied_dimensions <- unname(lengths(unique_indices))
      
      # sort indices and fill in missing indices as NA to ensure
      # (1) even if the order of the variables is something weird (like 
      # x[2,2] comes before x[1,1]) the result
      # places those columns in the correct cells of the array
      # (2) if some combination of indices is missing (say x[2,1] isn't
      # in the input) that cell in the array gets an NA
      
      # Use expand.grid to get all cells in output array. We reverse indices
      # here because it helps us do the sort after the merge, where
      # we need to sort in reverse order of the indices (because
      # the value of the last index should move slowest)
      all_indices <- expand.grid(rev(unique_indices))
      # merge with all.x = TRUE (left join) to fill in missing cells with NA
      indices <- merge(all_indices, cbind(indices, index = seq_len(nrow(indices))),
                       all.x = TRUE, sort = FALSE)
      # need to do the sort manually after merge because when sort = TRUE, merge
      # sorts factors as if they were strings, and we need factors to be sorted as factors
      indices <- indices[do.call(order, as.list(indices[, -ncol(indices), drop = FALSE])),]
      
      indices_info$indices <- unname(rev(indices[, names(indices) != "index", drop = FALSE]))
      indices_info$position <- which(var_i)[indices$index]
    }
    indices_info
  })
  names(variable_indices_info) <- var_names
  variable_indices_info
}
