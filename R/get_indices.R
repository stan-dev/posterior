#' Parse the indices from a vector of variables (extracted e.g. from a 
#' `draws_summary` object or a `draws` object)
#' 
#' @name variable_indices
#' @param x a character vector of variables
#'
#' @return
#' A list of indices for each variable 
#' 
#' @details
#' Assumes that variable indexing is reflected by square brackes in the names
#' 
NULL

get_indices <- function(x){
  vars_indices <- strsplit(variables, "(\\[|\\])")
  vars <- sapply(vars_indices, `[[`, 1)
  var_names <- unique(vars)
  
  var <- var_names[6]
  
  indices_list <- lapply(var_names, function (var) {
    var_i <- vars == var
    # reset class here as otherwise the draws arrays in the output rvars
    # have type draws_matrix, which makes inspecting them hard
    var_length <- sum(var_i)
    
    if (var_length == 1) {
      # single variable, no indices
      out <- 1
      dimnames(out) <- NULL
    } else {
      # variable with indices => we need to reshape the array
      # basically, we're going to do a bunch of work up front to figure out
      # a single array slice that does most of the work for us.
      
      # first, pull out the list of indices into a data frame
      # where each column is an index variable
      indices <- sapply(vars_indices[var_i], `[[`, 2)
      indices <- as.data.frame(do.call(rbind, strsplit(indices, ",")),
                               stringsAsFactors = FALSE)
      
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
            # indices with values < 1 are sorted but otherwise left as-is, and will create dimnames
            indices[[i]] <- numeric_index
            unique_indices[[i]] <- sort(unique(numeric_index))
            .dimnames[[i]] <- unique_indices[[i]]
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
    }
    indices
  })
  indices_list
}