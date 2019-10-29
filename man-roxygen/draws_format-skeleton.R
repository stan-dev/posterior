#' @description The `<%=paste0("as_", draws_format, "()")%>` methods convert
#'   objects to the `<%=draws_format%>` format. See **Details**.

# Shared arguments

#' @param x An object to convert to a `<%=draws_format%>` object.
#' @param ... Arguments passed to individual methods (if applicable).

# The @return template also uses a base_class template variable because
# although could determine base_class just from sub("draws_", "", draws_format),
# that doesn't work for draws_df since it isn't a "df" but rather a
# c("tbl_df", "tbl", "data.frame").

#' @return A `<%=draws_format%>` object, which has classes
#' `c("<%=draws_format%>", "draws", <%=base_class%>)`.

# An example that can be used for all formats to demonstrate the structure, but
# we can always add format-specific examples by also explicitly adding an
# @examples section to the format-specific pages. See for example the draws_df
# documentation, which includes this example plus another one.

#' @examples
#' x <- <%=paste0("as_", draws_format)%>(draws_eight_schools)
#' class(x)
#' str(x)
