#' Create Compact Letter Display Layer
#' Groups with at least one letter in common are not significantly different.
#' @param test Which test to run for pairwise comparisons. Either \code{tukey}
#' (the default) or \code{kruskalmc}.
#' @param type If a grouping variable is provided, determines whether to run
#' separate tests for each facet (\code{one-way}) or one (\code{two-way}) test with
#' an interaction term between \code{x} and \code{group}. Defaults to \code{two-way}.
#' @param where Where to put the letters. Either above the box (\code{box}),
#' above the upper whisker (\code{whisker}), or at the \code{mean} or
#' \code{median}.
#' @param hjust Horizontal adjustment of the label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param vjust Vertical adjustment of the label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param size Label size. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param na.rm Logical. Whether to remove observations with NAs for the provided
#' factors (i.e. \code{x} and \code{group}) before plotting. Defaults to TRUE.
#' @param threshold Statistical threshold for significance. Defaults to 0.05.
#' @author Ethan Bass
#' @references
#' * Piepho, Hans-Peter. An Algorithm for a Letter-Based Representation of
#' All-Pairwise Comparisons. Journal of Computational and Graphical Statistics
#' 13, no. 2 (June 1, 2004): 456–66. \doi{10.1198/1061860043515}.
#'
#' * Piepho, Hans-Peter. “Letters in Mean Comparisons: What They Do and Don’t Mean.”
#' Agronomy Journal 110, no. 2 (2018): 431–34. \doi{10.2134/agronj2017.10.0580}
#'
#'#' * Graves S, Piepho H, Dorai-Raj LSwhfS (2019). multcompView: Visualizations
#' of Paired Comparisons. R package version 0.1-8. \url{https://CRAN.R-project.org/package=multcompView}
#'
#' @note Thank you to Hiroaki Yutani (\url{https://yutani.rbind.io/post/2017-11-07-ggplot-add/})
#' and Simon P Couch (\url{https://www.simonpcouch.com/blog/ggplot-pipe-plus/})
#' for a couple of very helpful blog posts describing the \code{\link[ggplot2]{ggplot_add}}
#' syntax.
#' @examples
#' library(ggplot2)
#' set.seed(1)
#' data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
#'                   "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)),
#'                   "Size" = c("Big","Small"))
#' data |> ggplot(aes(x=Category, y=Value)) + geom_boxplot() + facet_wrap(~Size) + geom_tukey()
#' data |> ggplot(aes(x=Size, y=Value)) + geom_boxplot() + facet_wrap(~Category) + geom_tukey()
#' @export

geom_tukey <- function(test = c("tukey","kruskalmc"), threshold = 0.05,
                       type=c("two-way", "one-way"),
                       where = c("box","whisker", "mean", "median"),
                       hjust=0, vjust=0, size = 4, na.rm = TRUE){
  # store inputs in classed output that can
  # be passed to a `ggplot_add` method
  test <- match.arg(test, c("tukey", "kruskalmc"))
  type <- match.arg(type, c("two-way", "one-way"))
  where <- match.arg(where,  c("box","whisker", "mean", "median"))
  if (test == "kruskalmc"){
    type <- "one-way"
  }
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
    na.rm = na.rm,
    threshold = threshold
  )
}

#' @importFrom tidyr drop_na
#' @noRd
geom_tukey_ <- function(p, test = c("tukey","kruskalmc"), threshold = 0.05,
                        type=c("two-way", "one-way"),
                        where = c("box","whisker", "mean","median"),
                        hjust=0, vjust=0, size = 4, na.rm = TRUE) {
  data <- p$data
  if (na.rm){
    data <- drop_na(data, !!p$mapping$x, !!p$facet$params$facets[[1]])
  }
  if (length(p$facet$params) == 0){
    data <- get_tukey_letters(data = data, x = p$mapping$x, y = p$mapping$y,
                              test = test, where = where, threshold = threshold)
  } else{
    if (type == "two-way"){
      data <- get_tukey_letters(data = data,
                                x = c(p$mapping$x, p$facet$params$facets[[1]]),
                                y= p$mapping$y, test = test,
                                where = where,type = type,
                                threshold = threshold)
    } else if (type == "one-way"){
      data <- get_tukey_letters(data = data,
                                x = p$mapping$x, y= p$mapping$y,
                                group = p$facet$params$facets[[1]],
                                test = test,
                                where = where, type = type,
                                threshold = threshold)
    }
  }
  geom_text(data = data,
            aes(y = .data$Placement.Value, label = .data$Letter), hjust = hjust,
            vjust = vjust, size = size)
}

#' @name ggplot_add.geom_tukey
#' @export
#' @noRd
#' @note Adapted from (https://www.simonpcouch.com/blog/ggplot-pipe-plus/)
ggplot_add.geom_tukey <- function(object, plot, object_name) {

  fn <- attr(object, "fn")

  tukey_args <- attributes(object)[!names(attributes(object)) %in%
                                     c("class", "fn")]

  new_layer <- do.call(
    fn,
    c(list(plot), tukey_args)
  )

  # return the new plot
  plot$layers <- append(plot$layers, new_layer)
  plot
}
