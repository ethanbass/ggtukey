# Create Compact Letter Display Layer Groups with at least one letter in common are not significantly different.

Create Compact Letter Display Layer Groups with at least one letter in
common are not significantly different.

## Usage

``` r
geom_tukey(
  test = c("tukey", "kruskalmc", "dunn"),
  type = c("two-way", "one-way"),
  threshold = 0.05,
  where = c("box", "whisker", "mean", "median", "se", "sd", "cl_normal", "cl_boot"),
  hjust = 0,
  vjust = -0.2,
  geom = "text",
  size = 4,
  color = "black",
  fill = "white",
  alpha = 1,
  na.rm = TRUE,
  reversed = FALSE
)
```

## Arguments

- test:

  Which test to run for pairwise comparisons. Either `tukey` (the
  default),
  [`kruskalmc`](https://rdrr.io/pkg/pgirmess/man/kruskalmc.html), or
  [`dunn_test`](https://rpkgs.datanovia.com/rstatix/reference/dunn_test.html).

- type:

  If a grouping variable is provided, determines whether to run separate
  tests for each facet (`one-way`) or one (`two-way`) test with an
  interaction term between `x` and `group`. Defaults to `two-way`.

- threshold:

  Statistical threshold for significance. Defaults to 0.05.

- where:

  Where to put the letters. Either above the box (`box`) or upper
  whisker (`whisker`) of a boxplot; at the `mean` or `median`; or at the
  top of the error bars calculated from the standard error (`se`),
  standard deviation `sd`, or 95% confidence intervals returned by
  [`smean.cl.normal`](https://rdrr.io/pkg/Hmisc/man/smean.sd.html), or
  [`smean.cl.boot`](https://rdrr.io/pkg/Hmisc/man/smean.sd.html).

- hjust:

  Horizontal adjustment of the label. (Argument to
  [`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html)).

- vjust:

  Vertical adjustment of the label. (Argument to
  [`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html)).

- geom:

  Which geom to use to plot letters. Options are `text` and `label`.

- size:

  Label size. Argument to
  [`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- color:

  Label color.

- fill:

  Label fill (only applies if `geom == "label"`).

- alpha:

  Label transparency. Defaults to 1.

- na.rm:

  Logical. Whether to remove observations with NAs for the provided
  factors (i.e. `x` and `group`) before plotting. Defaults to `TRUE`.

- reversed:

  Logical. Argument to
  [`multcompLetters3`](https://rdrr.io/pkg/multcompView/man/multcompLetters.html).
  Determines whether order of letters should be reversed. Defaults to
  `FALSE`.

## Note

Thank you to [Hiroaki Yutani](https://github.com/yutannihilation) and
[Simon P. Couch](https://github.com/simonpcouch) for a couple of very
helpful blog posts
([1](https://yutani.rbind.io/post/2017-11-07-ggplot-add/),
[2](https://www.simonpcouch.com/blog/ggplot-pipe-plus/)) describing the
[`ggplot_add`](https://ggplot2.tidyverse.org/reference/update_ggplot.html)
syntax.

## References

- Piepho, Hans-Peter. An Algorithm for a Letter-Based Representation of
  All-Pairwise Comparisons. Journal of Computational and Graphical
  Statistics 13, no. 2 (June 1, 2004): 456–66.
  [doi:10.1198/1061860043515](https://doi.org/10.1198/1061860043515) .

- Piepho, Hans-Peter. “Letters in Mean Comparisons: What They Do and
  Don’t Mean.” Agronomy Journal 110, no. 2 (2018): 431–34.
  [doi:10.2134/agronj2017.10.0580](https://doi.org/10.2134/agronj2017.10.0580)

- Graves S, Piepho H, Dorai-Raj LSwhfS (2019). multcompView:
  Visualizations of Paired Comparisons. R package version 0.1-8.
  <https://CRAN.R-project.org/package=multcompView>

## Author

Ethan Bass

## Examples

``` r
library(ggplot2)
set.seed(1)
data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
                  "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)),
                  "Size" = c("Big","Small"))
data |> ggplot(aes(x=Category, y=Value)) + geom_boxplot() + facet_wrap(~Size) + geom_tukey()

data |> ggplot(aes(x=Size, y=Value)) + geom_boxplot() + facet_wrap(~Category) + geom_tukey()
```
