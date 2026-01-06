#' Perform Tukey Test and calculate letter placement
#' @importFrom stats TukeyHSD aov as.formula median quantile reformulate reorder
#' @importFrom dplyr left_join
#' @importFrom rlang as_name
#' @param data A data.frame in long format
#' @param x Independent variable or vector of variables to plot on x axis
#' @param y Response variable to plot on y axis
#' @param test Which test to run for pairwise comparisons. Either \code{tukey}
#' (\code{\link[stats]{TukeyHSD}}), \code{\link[pgirmess]{kruskalmc}}, or
#' \code{\link[rstatix]{dunn_test}}. Defaults
#' to \code{tukey}.
#' @param where Where to put the letters. Either above the box (\code{box}),
#' above the upper whisker (\code{whisker}), or at the \code{mean} or
#' \code{median}.
#' @param threshold Statistical threshold for significance. Defaults to 0.05.
#' @param reversed Logical. Argument to
#' \code{\link[multcompView]{multcompLetters3}}. Determines whether order of
#' letters should be reversed. Defaults to \code{FALSE}.
#' @noRd

get_tukey_letters <- function(data, x, y, group = NULL,
                              test = c("tukey", "kruskalmc", "dunn"),
                              type = c("two-way", "one-way"),
                              where = c("box", "whisker", "mean",
                                        "median", "se", "sd",
                                        "cl_normal", "cl_boot"),
                              threshold = 0.05, reversed = FALSE){
  test <- match.arg(test, c("tukey", "kruskalmc", "dunn"))
  where <- match.arg(where, c("box", "whisker", "mean","median", "se", "sd",
                              "cl_normal", "cl_boot"))
  type <- match.arg(type, c("two-way", "one-way"))
  # if (any(grepl("-", levels(data[,rlang::as_name(x), drop = TRUE])))){
  #   stop("Factor names cannot contain dashes. Please recode factor levels before proceeding.")
  # }
  if (inherits(x, "quosure") & is.null(group)){
    letters.df <- place_tukey_letters(data = data, x = as_name(x), y = as_name(y),
                                      test = test, where = where,
                                      threshold = threshold, reversed = reversed)
  } else{
    if (type == "two-way"){
      letters.df <- place_tukey_letters(data = data, x = sapply(x, as_name),
                                        y = as_name(y), test = test,
                                        where = where, threshold = threshold,
                                        reversed = reversed)
    } else if (type == "one-way"){
      letters.df <- purrr::map_dfr(unique(data[[as_name(group)]]), function(gr){
        data %>% filter(!!group == gr) %>%
          place_tukey_letters(x = as_name(x), y = as_name(y), test = test,
                              where = where, threshold = threshold,
                              reversed = reversed) %>%
          mutate(!!group := gr) %>% tibble::remove_rownames()
      })
    }
  }
  letters.df
}

#' Place Tukey Letters
#' @noRd
place_tukey_letters <- function(data, x, y, test = c("tukey", "kruskalmc", "dunn"),
                                where = c("box", "whisker"),
                                threshold = 0.05, p.adjust, reversed = FALSE){
  if (length(x) == 1){
    form <- form_let <- reformulate(x,y)
    xlab <- x
  } else {
    form <- reformulate(termlabels = paste(x, collapse="*"), y)
    form_let <- reformulate(termlabels = paste(x, collapse=":"), y)
    xlab <- paste(x, collapse=":")
    data[,xlab] <- apply(data[,x], 1, paste, collapse = ":")
  }
  if (test == "tukey"){
    tukey <- TukeyHSD(mod <- aov(form, data = data))
    # tukey <- tukey[which(!is.na(tukey))]
    # letters.df <- data.frame("Letter" = multcompLetters(tukey, threshold = threshold)$Letters)
    # let <- multcompView::multcompLetters2(form_let, tukey[[xlab]][,"p adj"],
    #                                       data = as.data.frame(data))
    diff <- tukey[[xlab]][,"p adj"]
    # multcompLetters4(mod, tukey)$treatments
  } else if (test == "kruskalmc"){
    test <- pgirmess::kruskalmc(form, data = data, probs = threshold)
    diff <- test$dif.com[,"stat.signif"]
    names(diff) <- rownames(test$dif.com)
    # letters.df <- data.frame("Letters" = multcompLetters2(formula = diff)$Letters)
  } else if (test == "dunn"){
    test <- rstatix::dunn_test(form_let, data = data)
    diff <- test$p
    names(diff) <- paste(test$group1, test$group2, sep="-")
  }
  let <- multcompView::multcompLetters3(z = xlab, y = y,
                                        x = diff,
                                        data = as.data.frame(data),
                                        reversed = reversed)
  letters.df <- data.frame(Letter = let$Letters)
  # letters.df <- data.frame("Letter" = multcompLetters(tukey, threshold = threshold)$Letters)
  letters.df[[xlab]] <- rownames(letters.df) #Create column based on rownames

  placement_fnc <- switch(where,
                          "box" = get_quantile,
                          "whisker" = get_whisker,
                          "mean" = mean,
                          "median" = median,
                          "se" = get_sem,
                          "sd" = get_sd,
                          "cl_normal" = get_cl_normal,
                          "cl_boot" = get_cl_boot)
  placement <- data %>% # Create a dataframe to assign the letter position.
    dplyr::group_by(.data[[xlab]]) %>%
    dplyr::summarise("Placement.Value" = placement_fnc(.data[[y]]))
  letters.df <- dplyr::left_join(letters.df, placement, by = xlab) # Merge dataframes
  if (length(x) > 1){
    letters.df <- dplyr::left_join(letters.df, unique(data[,c(xlab, x)]), by = xlab)
  }
  letters.df
}

#' Calculate standard error of the mean error bar
#' @importFrom stats sd
#' @noRd
get_sem <- function(x){
  mean(x) + sd(x)/sqrt(length(x))
}

#' Calculate standard deviation error bar
#' @importFrom stats sd
#' @noRd
get_sd <- function(x){
  mean(x) + sd(x)
}

#' Calculate cl normal error bar
#' @noRd
get_cl_normal <- function(x){
  Hmisc::smean.cl.normal(x)[[3]]
}

#' Calculate cl boot error bar
#' @noRd
get_cl_boot <- function(x){
  Hmisc::smean.cl.boot(x)[[3]]
}

#' Check whether color specifications exists.
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
#' @export
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

#' Calculate quantile
#' @importFrom stats quantile
#' @noRd
get_quantile <- function(x){
  quantile(x, na.rm = TRUE)[4]
}

#' Calculate boxplot whisker
#' @importFrom stats IQR
#'@noRd
get_whisker <- function(x){
  r <- quantile(x, na.rm = TRUE)[4] + 1.5*IQR(x, na.rm = TRUE)
  x <- x[x <= r]
  max(x, na.rm = TRUE)
}
