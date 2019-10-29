#' @description The `<%=paste0("as_", draws_format, "()")%>` methods convert
#'   objects to the `<%=draws_format%>` format. See **Details**.
#'
#' @param x An object to convert to a `<%=draws_format%>` object.
#' @param ... Arguments passed to individual methods (if applicable).
#'
#' @return A `<%=draws_format%>` object, which has classes
#' `c("<%=draws_format%>", "draws", "<%=base_class%>")`.
#'
#' @examples
#' x <- <%=paste0("as_", draws_format)%>(draws_eight_schools)
#' class(x)
#' str(x)
