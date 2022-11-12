#' Check whether color specifications exists.
#'
#' @export
#' @import grDevices
#' @description Function to check whether all specified colors are
#' actual colors.
#' @param x Vector of any of the three kinds of R color specifications,
#' i.e., either a color name (as listed by
#' \code{\link[grDevices]{palette}colors()}), a hexadecimal string of the form
#' '#rrggbb' or '#rrggbbaa' (see rgb), or a positive integer i meaning
#' \code{\link[grDevices]{palette}()[i]}.
#' @param return.colors Logical: logical values (FALSE, default) or
#' returning colors (TRUE)
#' @note Adapted from plotfunctions packaage https://cran.r-project.org/web/packages/plotfunctions/index.html
#' @author Jacolien van Rij
#' @return Logical value (or colors)
#' @examples
#' # correct color definitions:
#' is.color(c('#FF0000FF', '#00FF00FF', '#0000FFFF'))
#' is.color(c('red', 'steelblue', 'green3'))
#' is.color(c(1,7,28))
#' # mixtures are possible too:
#' is.color(c('#FF0000FF', 'red', 1, '#FF0000', rgb(.1,0,0)))
#'
#' # return colors:
#' # note that 28 is converted to 4...
#' is.color(c(1,7,28), return.colors=TRUE)
#' is.color(c('#FF0000CC', 'red', 1, '#FF0000'), return.colors=TRUE)
#'
#' # 4 incorrect colors, 1 correct:
#' test <- c('#FH0000', 3, '#FF00991', 'lavendel', '#AABBCCFFF')
#' is.color(test)
#' is.color(test, return.colors=TRUE)
#'
is.color <- function(x, return.colors = FALSE) {
  # numeric colors, max 8
  if (is.numeric(x)) {
    if (!return.colors) {
      return(x > 0 & (x%%1 == 0))
    } else {
      return(palette()[(x - 1)%%8 + 1])
    }
  }
  # convert any numeric values:
  if (any(grepl("^[0-9]+$", x))) {
    x[grepl("^[0-9]+$", x)] <- palette()[(as.numeric(x[grepl("^[0-9]+$", x)]) - 1)%%8 + 1]
  }

  # color names and hexadecimal colors
  y <- grepl("^\\#[a-fA-F0-9]{6}$", x) | grepl("^\\#[a-fA-F0-9]{8}$", x) | (x %in% colors())
  if (!return.colors) {
    return(y)
  } else {
    x[!y] <- NA
    return(x)
  }
}
