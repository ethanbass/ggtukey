
#' Do Tukey Test and calculate letter placement
#' @importFrom stats TukeyHSD aov as.formula median quantile reorder
#' @importFrom dplyr left_join
#' @param data A data.frame in long format
#' @param x Independent variable or vector of variables to plot on x axis
#' @param y Response variable to plot on y axis
#' @param test Which test to run for pairwise comparisons. Either \code{tukey}
#' (\code{\link[stats]{TukeyHSD}}) or \code{\link[pgirmess]{kruskalmc}}. Defaults
#' to \code{tukey}.
#' @param where Where to put the letters. Either above the box (\code{box}) or
#' above the upper whisker (\code{whisker}).
#' @param threshold Statistical threshold for significance. Defaults to 0.05.
#' @noRd

get_tukey_letters <- function(data, x, y, group=NULL, test = c("tukey", "kruskalmc"),
                              type = c("two-way", "one-way"),
                              where=c("box", "whisker", "mean", "median"),
                              threshold = 0.05){
  test <- match.arg(test, c("tukey", "kruskalmc"))
  where <- match.arg(where, c("box", "whisker", "mean","median"))
  type <- match.arg(type, c("two-way", "one-way"))
  if (inherits(x, "quosure") & is.null(group)){
    letters.df <- place_tukey_letters(data, as_name(x), as_name(y), test = test,
                                      where = where, threshold = threshold)
  } else{
    if (type == "two-way"){
      letters.df <- place_tukey_letters(data, sapply(x, as_name), as_name(y),
                                        test = test, where = where,
                                        threshold = threshold)
    } else if (type == "one-way"){
      letters.df <- purrr::map_dfr(unique(data[[as_name(group)]]), function(gr){
        data %>% filter(!!group == gr) %>%
          place_tukey_letters(x = as_name(x), y = as_name(y), test = test,
                              where = where, threshold = threshold) %>%
          mutate(!!group := gr) %>% tibble::remove_rownames()
      })
    }
  }
  letters.df
}

#' @noRd
place_tukey_letters <- function(data, x, y, test = c("tukey", "kruskalmc"),
                                where = c("box","whisker"),
                                threshold=0.05){
  if (length(x) == 1){
    form <- as.formula(paste(y, x, sep="~"))
    xlab <- x
  } else {
    form <- as.formula(paste(y, paste(x, collapse="*"), sep="~"))
    xlab<-paste(x, collapse=":")
    data[,xlab] <- apply(data[,x], 1, paste, collapse = ":")
  }
  if (test == "tukey"){
    tukey <- TukeyHSD(aov(form, data = data))[[xlab]][,4]
    tukey <- tukey[which(!is.na(tukey))]
    letters.df <- data.frame("Letter" = multcompLetters(tukey, threshold = threshold)$Letters)
  } else if (test == "kruskalmc"){
    test <- pgirmess::kruskalmc(form, data=data, probs = threshold)
    diff <- test$dif.com[,"difference"]
    names(diff) <- rownames(test$dif.com)
    letters.df <- data.frame("Letter" = multcompLetters(diff)$Letters)
  }
  # letters.df <- data.frame("Letter" = multcompLetters(tukey, threshold = threshold)$Letters)
  letters.df[[xlab]] <- rownames(letters.df) #Create column based on rownames

  placement_fnc <- switch(where,
                          "box" = get_quantile,
                          "whisker" = get_whisker,
                          "mean" = mean,
                          "median" = median)
  placement <- data %>% # Create a dataframe to assign the letter position.
    group_by(.data[[xlab]]) %>%
    summarise("Placement.Value" = placement_fnc(.data[[y]]))
  letters.df <- left_join(letters.df, placement, by = xlab) # Merge dataframes
  if (length(x) > 1){
    letters.df <- left_join(letters.df, unique(data[,c(xlab, x)]), by = xlab)
  }
  letters.df
}

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
