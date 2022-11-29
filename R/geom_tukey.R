#' Tukey Geom
#' @param test Which test to run for pairwise comparisons. Default is \code{tukey}.
#' @param type If a grouping variable is provided, determines whether to run
#' separate tests for each facet (\code{local}) or one (\code{global}) test with
#' an interaction term between \code{x} and \code{group}. Defaults to \code{global}.
#' @param where Where to put the letters. Either above the box (\code{box}) or
#' above the upper whisker (\code{whisker}).
#' @param hjust Horizontal adjustment of label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param vjust Vertical adjustment of label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param size Label size. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param na.rm Logical. Whether to remove observations with NAs for the provided
#' factors (i.e. \code{x} and \code{group}) before plotting.
#' @author Ethan Bass
#' @references
#' * Piepho, Hans-Peter (2004) "An Algorithm for a Letter-Based
#' Representation of All-Pairwise Comparisons", Journal of Computational and
#' Graphical Statistics, 13(2)456-466.
#'
#' * Graves S, Piepho H, Dorai-Raj LSwhfS (2019). _multcompView: Visualizations of Paired Comparisons_. R package
#' version 0.1-8, (https://CRAN.R-project.org/package=multcompView).
#'
#' @note Thank you to Hiroaki Yutani for a very helpful blog post describing the
#' ggplot_add syntax (https://yutani.rbind.io/post/2017-11-07-ggplot-add/).
#' @examples
#' library(ggplot2)
#' set.seed(1)
#' data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
#'                   "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)),
#'                   "Size" = c("Big","Small"))
#' data |> ggplot(aes(x=Category, y=Value)) + geom_boxplot() + facet_wrap(~Size) + geom_tukey()
#' data |> ggplot(aes(x=Size, y=Value)) + geom_boxplot() + facet_wrap(~Category) + geom_tukey()
#' @export


geom_tukey <- function(test = "tukey",
                       type=c("global", "local"), where = c("box","whisker", "mean", "median"),
                       hjust=0, vjust=0, size = 4, na.rm = TRUE){
  # store inputs in classed output that can
  # be passed to a `ggplot_add` method
  type <- match.arg(type, c("global", "local"))
  where <- match.arg(where,  c("box","whisker", "mean", "median"))
  structure(
    "geom_tukey",
    class = "geom_tukey",
    fn = "geom_tukey_",
    test = test,
    type = type,
    where = where,
    hjust = hjust,
    vjust = vjust,
    size = size,
    na.rm = na.rm
  )
}

#' @importFrom tidyr drop_na
#' @noRd
geom_tukey_ <- function(p, test = "tukey",
                        type=c("global", "local"), where = c("box","whisker", "mean","median"),
                        hjust=0, vjust=0, size = 4, na.rm = TRUE) {
  data <- p$data
  if (na.rm){
    data <- drop_na(data, !!p$mapping$x, !!p$facet$params$facets[[1]])
  }
  if (length(p$facet$params) == 0){
    data <- get_tukey_letters(data, p$mapping$x, p$mapping$y, where = where)
  } else{
    if (type == "global"){
      data <- get_tukey_letters(data = data,
                                         x = c(p$mapping$x, p$facet$params$facets[[1]]),
                                         y= p$mapping$y, where = where, type = type)
    } else if (type == "local"){
      data <- get_tukey_letters(data = data,
                                         x = p$mapping$x,
                                         y= p$mapping$y,
                                         group = p$facet$params$facets[[1]],
                                         where = where, type = type)
    }
  }
  geom_text(data = data,
            aes(y = .data$Placement.Value, label = .data$Letter), hjust = hjust,
            vjust = vjust, size = size)
}

#' @name ggplot_add.geom_tukey
#' @export
#' @noRd
ggplot_add.geom_tukey <- function(object, plot, object_name) {
  # a method for the `+` operator for fancy_layer objects.
  # - "object to add" (arguments to the RHS of the `+`)
  # - plot is the existing plot (on the LHS of the `+`)
  # - object_name is the unevaluated call on the RHS of the `+`

  # extract the `fn` attribute from `fancy_layer` output
  fn <- attr(object, "fn")

  # extract arguments `arg1` and `arg2` from `fancy_layer` output
  fancy_args <- attributes(object)[!names(attributes(object)) %in%
                                     c("class", "fn")]

  # call `fn` with the arguments `plot`, `arg1`, and `arg2`
  new_layer <- do.call(
    fn,
    c(list(plot), fancy_args)
  )
  # return the new plot
  plot$layers <- append(plot$layers, new_layer)
  plot
}
