# ggtukey
<!-- badges: start -->
  [![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/emersion/stability-badges#experimental)
<!-- badges: end -->

Provides a simple interface to visualize paired comparisons by adding "Tukey" letters to ggplot figures.

# Example

```
data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
                   "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)),
                   "Size" = c("Big","Small"))

boxplot_letters(data, x=Category, y=Value, group=Size)
```
