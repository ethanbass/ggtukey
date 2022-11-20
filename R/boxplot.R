#' Create ggplot boxplot with Tukey HSD letters
#'
#' Does pairwise comparison using \code{\link[stats]{TukeyHSD}} and
#'
#' Allows group variable for faceting
#' @param data A data.frame in long format
#' @param x variable to plot on x axis
#' @param y variable to plot on y axis
#' @param fill column or color to fill boxplots
#' @param group grouping variable (to allow faceting)
#' @param test Which test to run for pairwise comparisons. Default is \code{tukey}.
#' @param type Whether to run separate tests for each facet (\code{local}) or
#' one (\code{global}) test with an interaction term between \code{x} and
#' \code{group}. Defaults to \code{global}.
#' @param raw whether to plot raw data and (if so) which format
#' @param pt_col Color of points, if raw data is plotted.
#' @param ... Additional arguments to \code{\link[ggplot2]{geom_point}},
#' \code{\link[ggplot2]{geom_dotplot}}, or \code{\link[ggplot2]{geom_jitter}},
#' according to the value of \code{raw}.
#' @import dplyr
#' @import egg
#' @import multcompView
#' @import ggplot2
#' @importFrom stats TukeyHSD aov as.formula median quantile reorder
#' @import rlang
#' @author Ethan Bass
#' @note Adapted from https://www.mathiasecology.com/code/add-tukeys-significant-letters-to-ggplots
#' @examples
#' set.seed(1)
#' data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
#'                    "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)),
#'                    "Size" = c("Big","Small"))
#' boxplot_letters(data, Category, Value)
#' boxplot_letters(data, x=Category, y=Value, group=Size)
#' @export

boxplot_letters <- function(data, x, y, fill, group, test = "tukey",
                            type=c("global", "local"),
                             raw = c('none', 'points', 'dots', 'jitter'),
                             pt_col = "slategray", ...){

  raw <- match.arg(raw, c('none', 'points', 'dots', 'jitter'))
  type <- match.arg(type, c("global","local"))
  x.s <- deparse(substitute(x))
  y.s <- deparse(substitute(y))

  if (grepl('\"', x.s) | grepl('\"', y.s) | grepl('\"', deparse(substitute(group)))){
    stop("Variables should be provided directly. Please do not use quotes!")
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
    p <- p + geom_point(position = position_dodge(0.1), col = pt_col, ...)
  } else if (raw == "dots"){
    p <- p + geom_dotplot(binaxis = 'y', stackdir = 'center', fill = pt_col, ...)
  } else if (raw == "jitter"){
    p <- p + geom_jitter(col = pt_col, ...)
  }

  if (!missing(group)){
    p <- p + facet_wrap(vars({{group}}))
    add_letters_facet(p, x = {{x}}, y = {{y}}, group = {{group}},
                      test = test, type = type)
  } else{
    add_letters_single(p, x={{x}}, y={{y}}, test=test)
  }
}

#' Do Tukey Test and calculate letter placement
#' @importFrom stats TukeyHSD aov as.formula median quantile reorder
#' @importFrom dplyr left_join
#' @param data A data.frame in long format
#' @param x Independent variable or vector of variables to plot on x axis
#' @param y Response variable to plot on y axis
#' @param test Which test to run for pairwise comparisons. Default is \code{tukey}.
#' @noRd

get_tukey_letters <- function(data, x, y, test = "tukey"){
  if (length(x) == 1){
    form <- as.formula(paste(y, x, sep="~"))
    xlab <- x
  } else {
    form <- as.formula(paste(y, paste(x, collapse="*"), sep="~"))
    xlab<-paste(x, collapse=":")
    data[,xlab] <- apply(data[,x], 1, paste, collapse = ":")
  }

  tukey <-TukeyHSD(aov(form, data = data))[[xlab]][,4]
  letters.df <- data.frame("Letter" = multcompLetters(tukey)$Letters)
  letters.df[[xlab]] <- rownames(letters.df) #Create column based on rownames

  placement <- data %>% #We want to create a dataframe to assign the letter position.
    group_by(.data[[xlab]]) %>%
    summarise("Placement.Value" = quantile(.data[[y]])[4])
  letters.df <- suppressMessages(left_join(letters.df, placement)) # Merge dataframes
  if (length(x) > 1){
    letters.df <- left_join(letters.df, unique(data[,c(xlab, x)]))
  }
  letters.df
}

#' Add Tukey letters to existing ggplot object
#'
#' @param p A \code{ggplot} object
#' @param x variable to plot on x axis
#' @param y variable to plot on y axis
#' @param group grouping variable (to allow faceting)
#' @param test Which test to run for pairwise comparisons. Default is \code{tukey}.
#' @param type Whether to run separate tests for each facet (\code{local}) or
#' one (\code{global}) test with an interaction term between \code{x} and
#' \code{group}. Defaults to \code{global}.
#' @import ggplot2
#' @import dplyr
#' @import multcompView
#' @author Ethan Bass
#' @export

add_letters<- function(p, x, y, group=NULL, test="tukey",
                       type=c("global","local")){
  type <- match.arg(type, c("global", "local"))
  x.s <- deparse(substitute(x))
  y.s <- deparse(substitute(y))
  data <- p$data
  if (is.null(group)){
    letters.df <- get_tukey_letters(data, x = x.s, y = y.s)
  } else{
    if (type == "local"){
      letters.df <- purrr::map_dfr(unique(data[[deparse(substitute(group))]]), function(gr){
        data %>% filter({{group}} == gr) %>% get_tukey_letters(x = x.s, y = y.s) %>%
          mutate({{group}} := gr) %>% tibble::remove_rownames()
      })
    } else if (type == "global"){
      letters.df <- data %>% get_tukey_letters(x = c(x.s, deparse(substitute(group))), y = y.s)
    }
  }
  p + geom_text(data = letters.df, aes(x = {{x}},
                                     y = .data$Placement.Value,
                                     label = .data$Letter),
              size = 4, color = "black",
              hjust = -1.25,vjust = -0.8,
              fontface = "bold")
}

#' @param p A \code{ggplot} object created by \code{\link{boxplot_letters}}.
#' @param x variable to plot on x axis
#' @param y variable to plot on y axis
#' @param group grouping variable (to allow faceting)
#' @param test Which test to run for pairwise comparisons. Default is \code{tukey}.
#' @param type Whether to run separate tests for each facet (\code{local}) or
#' one (\code{global}) test with an interaction term between \code{x} and
#' \code{group}.
#' @import ggplot2
#' @import dplyr
#' @import multcompView
#' @author Ethan Bass
#' @noRd
add_letters_facet <- function(p, x, y, group=NULL, test="tukey",
                              type=c("global","local")){
  x.s <- gsub("~","",deparse(enquo(x)))
  y.s <- gsub("~","",deparse(enquo(y)))
  g.s <- gsub("~","",deparse(enquo(group)))
  data <- p$data
  type <- match.arg(type, c("global","local"))
  if (test == "tukey"){
    if (type == "local"){
      letters.df <- purrr::map_dfr(unique(data[[g.s]]), function(gr){
        data %>% filter({{group}} == gr) %>% get_tukey_letters(x = x.s, y = y.s) %>%
          mutate({{group}} := gr) %>% tibble::remove_rownames()
      })
    } else if (type == "global"){
      letters.df <- data %>% get_tukey_letters(x = c(x.s, g.s), y = y.s)
    }
  }
  p + geom_text(data = letters.df, aes(x = {{x}},
                                       y = .data$Placement.Value,
                                       label = .data$Letter),
                size = 4, color = "black",
                hjust = -1.25,vjust = -0.8,
                fontface = "bold")
}

#' @param p A \code{ggplot} object created by \code{\link{boxplot_letters}}.
#' @param x variable to plot on x axis
#' @param y variable to plot on y axis
#' @param group grouping variable (to allow faceting)
#' @param test Which test to run for pairwise comparisons. Default is \code{tukey}.
#' @import ggplot2
#' @import dplyr
#' @import multcompView
#' @author Ethan Bass
#' @noRd
add_letters_single <- function(p, x, y, test="tukey"){
  data <- p$data
  x.s <- gsub("~","",deparse(enquo(x)))
  y.s <- gsub("~","",deparse(enquo(y)))

  if (test == "tukey"){
    letters.df <- get_tukey_letters(data, x = x.s, y = y.s)
  }
  p + geom_text(data = letters.df, aes(x = {{x}},
                                       y = .data$Placement.Value,
                                       label = .data$Letter),
                size = 4, color = "black",
                hjust = -1.25,vjust = -0.8,
                fontface = "bold")
}
