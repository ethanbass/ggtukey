# ggtukey
<!-- badges: start -->
  [![ggtukey status badge](https://ethanbass.r-universe.dev/badges/ggtukey)](https://ethanbass.r-universe.dev)
  [![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/emersion/stability-badges#experimental)
<!-- badges: end -->

# Introduction

This package provides a simple way to visualize paired comparisons by adding [compact letter displays](https://en.wikipedia.org/wiki/Compact_letter_display) (i.e. "Tukey letters") to 'ggplot2' figures.

# Usage

`ggtukey` provides a new geom (`geom_tukey`) for overlaying compact letter displays (CLDs) on ggplot figures as well as a convenience function (`boxplot_letters`) for quickly creating figures. A brief example of the syntax is reproduced below. See the [vignette](https://ethanbass.github.io/ggtukey/articles/ggtukey.html) for more examples on how to use the package. 

```
data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
                   "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)),
                   "Size" = c("Big","Small"))
boxplot_letters(data, x=Category, y=Value, group=Size)
```

### Citation

You can cite ggtukey as follows:

Bass, E. (2022). ggtukey: Compact Letter Displays for 'ggplot2' (v0.1.0). (https://ethanbass.github.io/ggtukey/). 

# References


