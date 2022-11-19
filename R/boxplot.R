#' Create ggplot boxplot with Tukey HSD letters
#'
#' Allows group variable for faceting
#' @param data A data.frame in long format
#' @param x variable to plot on x axis
#' @param y variable to plot on y axis
#' @param fill column or color to fill boxplots
#' @param group grouping variable (to allow faceting)
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
#' data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
#'                         "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)))
#' boxplot_letters(data, Category, Value)
#' @export

boxplot_letters <- function(data, x, y, fill, group,
                             raw = c('none', 'points', 'dots', 'jitter'),
                             pt_col = "slategray", ...){

  raw <- match.arg(raw, c('none', 'points', 'dots', 'jitter'))

  x.s <- deparse(substitute(x))
  y.s <- deparse(substitute(y))

  if (grepl('\"', x.s) | grepl('\"', y.s)){
    stop("X and Y variables should be provided directly. Please do not use quotes!")
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
    add_letters_facet(p, x={{x}}, y={{y}}, group={{group}})
  } else{
    add_letters_single(p,x={{x}},y={{y}})
  }
}

#' Do Tukey Test and calculate letter placement
#' @importFrom stats TukeyHSD aov as.formula median quantile reorder
#' @noRd

get_tukey_letters <- function(data, x, y){
    form <- as.formula(paste(y, x, sep="~"))
  letters.df <- data.frame("Letter" = multcompLetters(TukeyHSD(aov(form, data = data))[[x]][,4])$Letters)
  letters.df[[x]] <- rownames(letters.df) #Create column based on rownames

  placement <- data %>% #We want to create a dataframe to assign the letter position.
    group_by(.data[[x]]) %>%
    summarise("Placement.Value" = quantile(.data[[y]])[4])
  letters.df <- suppressMessages(left_join(letters.df, placement)) # Merge dataframes
  letters.df
}

#' Add Tukey letters to existing ggplot object
#'
#' @param p A \code{ggplot} object
#' @param x variable to plot on x axis
#' @param y variable to plot on y axis
#' @param group grouping variable (to allow faceting)
#' @import ggplot2
#' @import dplyr
#' @import multcompView
#' @author Ethan Bass
#' @export

add_letters<- function(p, x, y, group=NULL){
  x.s <- deparse(substitute(x))
  y.s <- deparse(substitute(y))
  data <- p$data
  if (is.null(group)){
    letters.df <- get_tukey_letters(data, x = x.s, y = y.s)
  } else{
  letters.df <- purrr::map_dfr(unique(data[[deparse(substitute(group))]]), function(gr){
    data %>% filter({{group}} == gr) %>% get_tukey_letters(x = x.s, y = y.s) %>%
      mutate({{group}} := gr) %>% tibble::remove_rownames()
  })
  }
  p + geom_text(data = letters.df, aes(x = {{x}},
                                     y = .data$Placement.Value,
                                     label = .data$Letter),
              size = 4, color = "black",
              hjust = -1.25,vjust = -0.8,
              fontface = "bold")
}

add_letters_facet <- function(p, x, y, group=NULL){
  x.s <- gsub("~","",deparse(enquo(x)))
  y.s <- gsub("~","",deparse(enquo(y)))
  data <- p$data

  letters.df <- purrr::map_dfr(unique(data[[gsub("~","",deparse(enquo(group)))]]), function(gr){
    data %>% filter({{group}} == gr) %>% get_tukey_letters(x = x.s, y = y.s) %>%
      mutate({{group}} := gr) %>% tibble::remove_rownames()
  })

  p + geom_text(data = letters.df, aes(x = {{x}},
                                       y = .data$Placement.Value,
                                       label = .data$Letter),
                size = 4, color = "black",
                hjust = -1.25,vjust = -0.8,
                fontface = "bold")
}

add_letters_single <- function(p, x, y){
  data <- p$data
  x.s <- gsub("~","",deparse(enquo(x)))
  y.s <- gsub("~","",deparse(enquo(y)))

  letters.df <- get_tukey_letters(data, x = x.s, y = y.s)

  p + geom_text(data = letters.df, aes(x = {{x}},
                                       y = .data$Placement.Value,
                                       label = .data$Letter),
                size = 4, color = "black",
                hjust = -1.25,vjust = -0.8,
                fontface = "bold")
}

# get_tukey_letters_interaction <- function(data, x, y){
#   form <- as.formula(paste(y, paste0(x, collapse = "*"), sep="~"))
#
#   tk.idx <- ifelse(length(x)==1, x,
#                    paste0(x,collapse=":"))
#   letters.df <- data.frame("Letter" = multcompLetters(TukeyHSD(aov(form, data = data))[[tk.idx]][,4])$Letters)
#   letters.df[[tk.idx]] <- rownames(letters.df) #Create column based on rownames
#   df <- stringr::str_split_fixed(c(tk.idx,letters.df[,tk.idx]),":",2)
#   colnames(df) <- df[1,]
#   df <- df[-1,]
#   letters.df <- cbind(letters.df,df)
#   placement <- data %>% #We want to create a dataframe to assign the letter position.
#     group_by(.data[[x[1]]], .data[[x[2]]]) %>%
#     summarise("Placement.Value"=quantile(.data[[y]])[4])
#   letters.df <- suppressMessages(left_join(letters.df, placement)) # Merge dataframes
#   letters.df
# }
