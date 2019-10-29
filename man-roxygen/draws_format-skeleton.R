#' @description The `<%=paste0("as_", draws_format, "()")%>` methods convert
#'   objects to the `<%=draws_format%>` format. See **Details**.
#'
#' @param x An object to convert to a `<%=draws_format%>` object.
#' @param ... Arguments passed to individual methods (if applicable).


# The @return template also uses a base_class template variable.
# We could determine base_class just from sub("draws_", "", draws_format)
# except that doesnt' work because draws_df isn't a df but a tibble, so I
# added a base_class template variable.

#' @return A `<%=draws_format%>` object, which has classes
#' `c("<%=draws_format%>", "draws", "<%=base_class%>")`.


# An example that can be used for all formats to demonstrate the structure, but
# we can always add format-specific examples by also explicitly adding an
# @examples section to the format-specific pages

#' @examples
#' x <- <%=paste0("as_", draws_format)%>(draws_eight_schools)
#' class(x)
#' str(x)
