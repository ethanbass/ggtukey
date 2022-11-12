#' Create ggplot boxplot with Tukey HSD letters
#' @param data A data.frame in long format
#' @param x variable to plot on x axis
#' @param y variable to plot on y axis
#' @param raw whether to plot raw data and (if so) which format
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
boxplot_letters <- function(data, x, y, raw = c('none', 'points', 'dots')){
  raw <- match.arg(raw, c('none', 'points', 'dots'))

  # if (is.character(x) | is.character(y)){
  #   stop("Variables should be provided directly. Please do not use quotes!")
  # }

  x.c <- deparse(substitute(x))
  y.c <- deparse(substitute(y))

  # get letters
  form <- as.formula(paste(y.c, x.c, sep="~"))
  letters.df <- data.frame(multcompLetters(TukeyHSD(aov(form, data = data))[[x.c]][,4])$Letters)
  letters.df

  colnames(letters.df)[1] <- "Letter" #Reassign column name
  letters.df[[x.c]] <- rownames(letters.df) #Create column based on rownames

  placement <- data %>% #We want to create a dataframe to assign the letter position.
    group_by({{x}}) %>%
    summarise(quantile({{y}})[4])

  colnames(placement)[2] <- "Placement.Value"
  letters.df <- left_join(letters.df, placement) #Merge dataframes

  p <- data %>% #Dataframe from which data will be drawn
    ggplot(aes(x = reorder({{x}}, {{y}}, median), y = {{y}})) + #Instead of hard-coding a factor reorder, you can call it within the plotting function
    geom_boxplot(color = "black", alpha = 0) + #I like to set the color of boxplots to black with the alpha at 0 (fully transparent). I also like geom_jitter() but do not use it here for simplicity.
    theme_article() + #Clean, minimal theme courtesy of the "egg" package
    xlab(x.c) +
    geom_text(data = letters.df, aes(x = {{x}},
                                     y = .data$Placement.Value,
                                     label = .data$Letter),
              size = 4, color = "black",
              hjust = -1.25,vjust = -0.8,
              fontface = "bold")

  if (raw == "points"){
    p + geom_point(position = position_dodge(0.1), col="slategray")
  } else if (raw == "dots"){
    p + geom_dotplot(binaxis='y', stackdir='center', stack.ratio=1.5)
  } else p
}
