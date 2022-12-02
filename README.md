# ggtukey
<!-- badges: start -->
  [![ggtukey status badge](https://ethanbass.r-universe.dev/badges/ggtukey)](https://ethanbass.r-universe.dev)
  [![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/emersion/stability-badges#experimental)
<!-- badges: end -->

# Introduction

This package provides a simple way to visualize paired comparisons by adding [compact letter displays](https://en.wikipedia.org/wiki/Compact_letter_display) (i.e. "Tukey letters") to 'ggplot2' figures.

# Installation

It is currently recommended to install `ggtukey` from GitHub:


```
install.packages("devtools")
devtools::install_github("https://github.com/ethanbass/ggtukey/")
```

or from [R Universe](https://ethanbass.r-universe.dev/):

```
install.packages("ggtukey", repos="https://ethanbass.r-universe.dev/", type="source")
```


# Usage

`ggtukey` provides a new geom (`geom_tukey`) for overlaying compact letter displays (CLDs) on ggplot figures.

```
library(ggtukey)
library(ggplot2)
data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
                   "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)),
                   "Size" = c("Big","Small"))
                   
ggplot(data, aes(x=Category,y=Value)) + 
  geom_boxplot() + 
  facet_wrap(~Size) +
  geom_tukey(hjust=-0.2,vjust=-0.2)
```
 
There is also a convenience function (`boxplot_letters`) for quickly producing these types of figures as shown below.

```
boxplot_letters(data, x=Category, y=Value, group=Size)
```

For more examples and details on different options for customization, please consult the package [documentation](https://ethanbass.github.io/ggtukey/reference/index.html) and [vignette](https://ethanbass.github.io/ggtukey/articles/ggtukey.html). 

### Citation

You can cite `ggtukey` as follows:

Bass, E. (2022). ggtukey: Compact Letter Displays for 'ggplot2' (v0.1.0). (https://ethanbass.github.io/ggtukey/). 

# Further Reading

Piepho, Hans-Peter. An Algorithm for a Letter-Based Representation of All-Pairwise Comparisons. Journal of Computational and Graphical Statistics 13, no. 2 (June 1, 2004): 456–66. https://doi.org/10.1198/1061860043515.

Piepho, Hans-Peter. “Letters in Mean Comparisons: What They Do and Don’t Mean.” Agronomy Journal 110, no. 2 (2018): 431–34.  https://doi.org/10.2134/agronj2017.10.0580.

Graves S, Piepho H, Dorai-Raj LSwhfS (2019). multcompView: Visualizations of Paired Comparisons. R package version 0.1-8. https://CRAN.R-project.org/package=multcompView.

