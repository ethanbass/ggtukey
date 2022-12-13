#' Create ggplot boxplot with compact letter display
#'
#' Performs pairwise comparisons using \code{\link[stats]{TukeyHSD}} and produces
#' boxplots with compact letter display showing significance pairwise differences.
#' Letters are produced by \code{\link[multcompView]{multcompLetters}}. Plots are
#' produced by \code{\link[ggplot2]{ggplot2}}. Raw data can also be overlaid in
#' various ways according to the value of \code{raw}.
#'
#' Allows group variable for faceting
#' @param data A data.frame in "long" format.
#' @param x variable to plot on x axis.
#' @param y variable to plot on y axis.
#' @param fill column or color to fill boxplots
#' @param group A grouping variable (to allow faceting).
#' @param test Which test to run for pairwise comparisons. Either \code{tukey}
#' (the default) or \code{kruskalmc}.
#' @param type If a grouping variable is provided, determines whether to run
#' separate tests for each facet (\code{one-way}) or a single (\code{two-way})
#' test (with an interaction term between \code{x} and \code{group}). Defaults
#' to \code{two-way}. This argument only applies if the Tukey test is selected,
#' since there is no two-way Kruskal-Wallis test.
#' @param where Where to place the letters. Either above the box (\code{box}),
#' above the upper whisker (\code{whisker}).
#' @param raw Whether to plot raw data and (if so), how. The current options are
#' \code{none}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{geom_dotplot}}, or
#' \code{\link[ggplot2]{geom_jitter}}.
#' @param pt_col Color of points, if raw data is plotted.
#' @param ... Additional arguments to \code{\link[ggplot2]{geom_point}},
#' \code{\link[ggplot2]{geom_dotplot}}, or \code{\link[ggplot2]{geom_jitter}},
#' according to the value of \code{raw}.
#' @param hjust Horizontal adjustment of label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param vjust Vertical adjustment of label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param lab_size Label size. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param na.rm Logical. Whether to remove observations with NAs for the provided
#' factors (i.e. \code{x} and \code{group}) before plotting.
#' @param threshold Statistical threshold for significance. Defaults to 0.05.
#' @import dplyr
#' @import egg
#' @import multcompView
#' @import ggplot2
#' @importFrom stats TukeyHSD aov as.formula median quantile reorder
#' @import rlang
#' @author Ethan Bass
#' @note Adapted from https://www.mathiasecology.com/code/add-tukeys-significant-letters-to-ggplots
#' @return Returns the specified plot as a \code{ggplot} object.
#' @examples
#' set.seed(1)
#' data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
#'                    "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)),
#'                    "Size" = c("Big","Small"))
#' boxplot_letters(data, Category, Value)
#' boxplot_letters(data, x=Category, y=Value, group=Size)
#' @export

boxplot_letters <- function(data, x, y, fill, group, test = c("tukey", "kruskalmc"),
                            type=c("two-way", "one-way"), where = c("box","whisker"),
                            raw = c('none', 'points', 'dots', 'jitter'),
                            pt_col = "slategray", ..., hjust=0, vjust=0,
                            lab_size = 4, na.rm = TRUE, threshold = 0.05){
  test <- match.arg(test, c("tukey", "kruskalmc"))
  raw <- match.arg(raw, c('none', 'points', 'dots', 'jitter'))
  type <- match.arg(type, c("two-way","one-way"))
  where <- match.arg(where, c("box","whisker"))
  x.s <- deparse(substitute(x))
  y.s <- deparse(substitute(y))

  if (grepl('\"', x.s) | grepl('\"', y.s) | grepl('\"', deparse(substitute(group)))){
    stop("Variables should be provided directly. Please do not use quotes!")
  }
  data <- mutate_at(data, vars({{x}}, {{group}}), as.factor)
  if (na.rm){
    data <- filter(data, !is.na({{x}}), !is.na({{y}}))
    if (!missing(group)){
      data <- filter(data, !is.na({{group}}))
    }
  }
  if (missing(fill)){
    geom_box <- purrr::partial(geom_boxplot, color = "black", alpha = 0)
  } else if (deparse(substitute(fill)) %in% colnames(data)){
    geom_box <- purrr::partial(geom_boxplot, color = "black", aes(fill = {{fill}}))
  } else if (is.color(fill)){
    geom_box <- purrr::partial(geom_boxplot, color = "black", fill = {{fill}})
  }

  p <- data %>% #Dataframe from which data will be drawn
    ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_box() +
    theme_article() + #Clean, minimal theme courtesy of the "egg" package
    xlab(x.s)

  if (raw == "points"){
    if (deparse(substitute(pt_col)) %in% colnames(data)){
      p <- p + geom_point(aes(col={{pt_col}}), position = position_dodge(width = 0.1), ...)
    } else if (is.color(pt_col)){
      p <- p + geom_point(position = position_dodge(width = 0.1), col = pt_col, ...)
    }
  } else if (raw == "dots"){
    if (deparse(substitute(pt_col)) %in% colnames(data)){
      p <- p + geom_dotplot(aes(fill={{pt_col}}),
                            binaxis = 'y', stackdir = 'center', ...)
    } else if (is.color(pt_col)){
      p <- p + geom_dotplot(fill = pt_col,
                            binaxis = 'y', stackdir = 'center', ...)
    }
  } else if (raw == "jitter"){
    if (deparse(substitute(pt_col)) %in% colnames(data)){
      p <- p + geom_jitter(aes(col = {{pt_col}}), ...)
    } else if (is.color(pt_col)){
      p <- p + geom_jitter(col = pt_col, ...)
    }
  }
  if (!missing(group)){
    p <- p <- p + facet_wrap(vars({{group}}))
  }
  p + geom_tukey(test = test, type = type, where = where,
                 hjust = hjust, vjust = vjust, size = lab_size,
                 threshold = threshold)
}
