#' Create ggplot boxplot with compact letter display
#'
#' Does pairwise comparison using \code{\link[stats]{TukeyHSD}} and produces
#' boxplots with compact letter display showing significance pairwise differences.
#' Letters are produced by \code{\link[multcompView]{multcompLetters}}. Plots are
#' produced by \code{\link[ggplot2]{ggplot2}}.
#'
#' Allows group variable for faceting
#' @param data A data.frame in "long" format.
#' @param x variable to plot on x axis.
#' @param y variable to plot on y axis.
#' @param fill column or color to fill boxplots
#' @param group A grouping variable (to allow faceting).
#' @param test Which test to run for pairwise comparisons. Default is \code{tukey}.
#' @param type If a grouping variable is provided, determines whether to run
#' separate tests for each facet (\code{local}) or one (\code{global}) test with
#' an interaction term between \code{x} and \code{group}. Defaults to \code{global}.
#' @param where Where to put the letters. Either above the box (\code{box}) or
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

boxplot_letters <- function(data, x, y, fill, group, test = "tukey",
                            type=c("global", "local"), where = c("box","whisker"),
                            raw = c('none', 'points', 'dots', 'jitter'),
                            pt_col = "slategray", ..., hjust=0, vjust=0,
                            lab_size = 4, na.rm = TRUE){

  raw <- match.arg(raw, c('none', 'points', 'dots', 'jitter'))
  type <- match.arg(type, c("global","local"))
  where <- match.arg(where, c("box","whisker"))
  x.s <- deparse(substitute(x))
  y.s <- deparse(substitute(y))

  if (grepl('\"', x.s) | grepl('\"', y.s) | grepl('\"', deparse(substitute(group)))){
    stop("Variables should be provided directly. Please do not use quotes!")
  }
  data <- mutate_at(data, vars({{x}}, {{group}}), as.factor)
  if (na.rm){
    data <- filter(data, !is.na({{x}}), !is.na({{group}}))
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
      p <- p + geom_point(aes(col={{pt_col}}), position = position_dodge(0.1), ...)
    } else if (is.color(pt_col)){
      p <- p + geom_point(position = position_dodge(0.1), col = pt_col, ...)
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
    p <- p + facet_wrap(vars({{group}}))
    add_letters_facet(p, x = {{x}}, y = {{y}}, group = {{group}},
                      test = test, type = type, where = where,
                      hjust=hjust, vjust=vjust, lab_size = lab_size)
  } else{
    add_letters_single(p, x={{x}}, y={{y}}, test = test, where = where,
                       hjust=hjust, vjust=vjust, lab_size=lab_size)
  }
}

#' Do Tukey Test and calculate letter placement
#' @importFrom stats TukeyHSD aov as.formula median quantile reorder
#' @importFrom dplyr left_join
#' @param data A data.frame in long format
#' @param x Independent variable or vector of variables to plot on x axis
#' @param y Response variable to plot on y axis
#' @param test Which test to run for pairwise comparisons. Default is \code{tukey}.
#' @param where Where to put the letters. Either above the box (\code{box}) or
#' above the upper whisker (\code{whisker}).
#' @noRd

get_tukey_letters <- function(data, x, y, group=NULL, test = "tukey",
                              type = c("global", "local"),
                              where=c("box", "whisker", "mean", "median")){
  where <- match.arg(where, c("box", "whisker", "mean","median"))
  type <- match.arg(type, c("global", "local"))
  if (inherits(x, "quosure") & is.null(group)){
    letters.df <- place_tukey_letters(data, as_name(x), as_name(y), test = "tukey",
                                      where = where)
  } else{
    if (type == "global"){
      letters.df <- place_tukey_letters(data, sapply(x, as_name), as_name(y), test = "tukey",
                                        where = where)
    } else if (type == "local"){
      letters.df <- purrr::map_dfr(unique(data[[as_name(group)]]), function(gr){
        data %>% filter(!!group == gr) %>% place_tukey_letters(x = as_name(x), y = as_name(y),
                                                               where = where) %>%
          mutate(!!group := gr) %>% tibble::remove_rownames()
      })
    }
  }
  letters.df
}

place_tukey_letters <- function(data, x, y, test = "tukey",
                                where = c("box","whisker")){
  if (length(x) == 1){
    form <- as.formula(paste(y, x, sep="~"))
    xlab <- x
  } else {
    form <- as.formula(paste(y, paste(x, collapse="*"), sep="~"))
    xlab<-paste(x, collapse=":")
    data[,xlab] <- apply(data[,x], 1, paste, collapse = ":")
  }
  tukey <- TukeyHSD(aov(form, data = data))[[xlab]][,4]
  tukey <- tukey[which(!is.na(tukey))]
  letters.df <- data.frame("Letter" = multcompLetters(tukey)$Letters)
  letters.df[[xlab]] <- rownames(letters.df) #Create column based on rownames

  placement_fnc <- switch(where,
                          "box" = get_quantile,
                          "whisker" = get_whisker,
                          "mean" = mean,
                          "median" = median)
  placement <- data %>% #We want to create a dataframe to assign the letter position.
    group_by(.data[[xlab]]) %>%
    summarise("Placement.Value" = placement_fnc(.data[[y]]))
  letters.df <- left_join(letters.df, placement, by = xlab) # Merge dataframes
  if (length(x) > 1){
    letters.df <- left_join(letters.df, unique(data[,c(xlab, x)]), by = xlab)
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
#' @param where Where to put the letters. Either above the box (\code{box}) or
#' above the upper whisker (\code{whisker}).
#' @param type Whether to run separate tests for each facet (\code{local}) or
#' one (\code{global}) test with an interaction term between \code{x} and
#' \code{group}. Defaults to \code{global}.
#' @import ggplot2
#' @import dplyr
#' @import multcompView
#' @author Ethan Bass
#' @export

add_letters<- function(p, x, y, group=NULL, test="tukey",
                       type=c("global","local"), where = c("box","whisker")){
  type <- match.arg(type, c("global", "local"))
  where <- match.arg(where, c("box", "whisker"))
  x.s <- deparse(substitute(x))
  y.s <- deparse(substitute(y))
  data <- p$data
  if (is.null(group)){
    letters.df <- get_tukey_letters(data, x = x.s, y = y.s, where = where)
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
#' @param where Where to put the letters. Either above the box (\code{box}) or
#' above the upper whisker (\code{whisker}).
#' @param hjust Horizontal adjustment of label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param vjust Vertical adjustment of label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param lab_size Label size. Argument to \code{\link[ggplot2]{geom_text}}.
#' @import ggplot2
#' @import dplyr
#' @import multcompView
#' @author Ethan Bass
#' @noRd
add_letters_facet <- function(p, x, y, group=NULL, test="tukey",
                              type=c("global","local"), where = c("box","whisker"),
                              hjust =0, vjust=0, lab_size = 4){
  x.s <- gsub("~","",deparse(enquo(x)))
  y.s <- gsub("~","",deparse(enquo(y)))
  g.s <- gsub("~","",deparse(enquo(group)))
  data <- p$data
  type <- match.arg(type, c("global","local"))
  where <- match.arg(where, c("box","whisker"))
  if (test == "tukey"){
    if (type == "local"){
      letters.df <- purrr::map_dfr(unique(data[[g.s]]), function(gr){
        data %>% filter({{group}} == gr) %>% get_tukey_letters(x = x.s, y = y.s) %>%
          mutate({{group}} := gr) %>% tibble::remove_rownames()
      })
    } else if (type == "global"){
      letters.df <- data %>% get_tukey_letters(x = c(x.s, g.s), y = y.s, where=where)
    }
  }
  p + geom_text(data = letters.df, aes(x = {{x}},
                                       y = .data$Placement.Value,
                                       label = .data$Letter),
                size = lab_size, color = "black",
                hjust = hjust, vjust = vjust,
                fontface = "bold")
}

#' @param p A \code{ggplot} object created by \code{\link{boxplot_letters}}.
#' @param x variable to plot on x axis
#' @param y variable to plot on y axis
#' @param group grouping variable (to allow faceting)
#' @param test Which test to run for pairwise comparisons. Default is \code{tukey}.
#' @param where Where to put the letters. Either above the box (\code{box}) or
#' above the upper whisker (\code{whisker}).
#' @param hjust Horizontal adjustment of label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param vjust Vertical adjustment of label. Argument to \code{\link[ggplot2]{geom_text}}.
#' @param lab_size Label size. Argument to \code{\link[ggplot2]{geom_text}}.
#' @import ggplot2
#' @import dplyr
#' @import multcompView
#' @author Ethan Bass
#' @noRd
add_letters_single <- function(p, x, y, test="tukey", where = c("box","whisker"),
                               hjust=0, vjust=0, lab_size=4){
  data <- p$data
  x.s <- gsub("~","",deparse(enquo(x)))
  y.s <- gsub("~","",deparse(enquo(y)))
  where <- match.arg(where, c("box","whisker"))
  if (test == "tukey"){
    letters.df <- get_tukey_letters(data, x = x.s, y = y.s, where = where)
  }
  p + geom_text(data = letters.df, aes(x = {{x}},
                                       y = .data$Placement.Value,
                                       label = .data$Letter),
                size = lab_size, color = "black",
                hjust = hjust, vjust = vjust,
                fontface = "bold")
}

#' @importFrom stats quantile
#' @noRd
get_quantile <- function(x){
  quantile(x, na.rm=TRUE)[4]
}

#' @importFrom stats IQR
#'@noRd
get_whisker <- function(x){
  r <- quantile(x, na.rm=TRUE)[4] + 1.5*IQR(x, na.rm=TRUE)
  x <- x[x<=r]
  max(x, na.rm=TRUE)
}
