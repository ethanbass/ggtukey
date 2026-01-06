# Create ggplot boxplot with compact letter display

Performs pairwise comparisons using
[`TukeyHSD`](https://rdrr.io/r/stats/TukeyHSD.html) and produces
boxplots with compact letter display showing significance pairwise
differences. Letters are produced by
[`multcompLetters`](https://rdrr.io/pkg/multcompView/man/multcompLetters.html).
Plots are produced by
[`ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html).
Raw data can also be overlaid in various ways according to the value of
`raw`.

## Usage

``` r
boxplot_letters(
  data,
  x,
  y,
  fill,
  group,
  test = c("tukey", "kruskalmc", "dunn"),
  type = c("two-way", "one-way"),
  where = c("box", "whisker", "mean", "median", "se", "sd", "cl_normal", "cl_boot"),
  raw = c("none", "points", "dots", "jitter"),
  pt_col = "slategray",
  hjust = 0,
  vjust = -0.2,
  lab_size = 4,
  na.rm = TRUE,
  threshold = 0.05,
  reversed = FALSE,
  ...
)
```

## Arguments

- data:

  A data.frame in "long" format.

- x:

  variable to plot on x axis.

- y:

  variable to plot on y axis.

- fill:

  column or color to fill boxplots

- group:

  A grouping variable (to allow faceting).

- test:

  Which test to run for pairwise comparisons. Either `tukey` (the
  default),
  [`kruskalmc`](https://rdrr.io/pkg/pgirmess/man/kruskalmc.html), or
  [`dunn_test`](https://rpkgs.datanovia.com/rstatix/reference/dunn_test.html).

- type:

  If a grouping variable is provided, determines whether to run separate
  tests for each facet (`one-way`) or a single (`two-way`) test (with an
  interaction term between `x` and `group`). Defaults to `two-way`. This
  argument only applies if the Tukey test is selected, since there is no
  two-way Kruskal-Wallis test.

- where:

  Where to put the letters. Either above the box (`box`) or upper
  whisker (`whisker`) of a boxplot; at the `mean` or `median`; or at the
  top of the error bars calculated from the standard error (`se`),
  standard deviation `sd`, or 95% confidence intervals returned by
  [`smean.cl.normal`](https://rdrr.io/pkg/Hmisc/man/smean.sd.html), or
  [`smean.cl.boot`](https://rdrr.io/pkg/Hmisc/man/smean.sd.html).

- raw:

  Whether to plot raw data and (if so), how. The current options are
  `none`,
  [`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html),
  [`geom_dotplot`](https://ggplot2.tidyverse.org/reference/geom_dotplot.html),
  or
  [`geom_jitter`](https://ggplot2.tidyverse.org/reference/geom_jitter.html).

- pt_col:

  Color of points, if raw data is plotted.
  [`geom_dotplot`](https://ggplot2.tidyverse.org/reference/geom_dotplot.html),
  or
  [`geom_jitter`](https://ggplot2.tidyverse.org/reference/geom_jitter.html),
  according to the value of `raw`.

- hjust:

  Horizontal adjustment of label. Argument to
  [`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- vjust:

  Vertical adjustment of label. Argument to
  [`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- lab_size:

  Label size. Argument to
  [`geom_text`](https://ggplot2.tidyverse.org/reference/geom_text.html).

- na.rm:

  Logical. Whether to remove observations with NAs for the provided
  factors (i.e. `x` and `group`) before plotting.

- threshold:

  Statistical threshold for significance. Defaults to 0.05.

- reversed:

  Logical. Argument to
  [`multcompLetters3`](https://rdrr.io/pkg/multcompView/man/multcompLetters.html).
  Determines whether order of letters should be reversed. Defaults to
  `FALSE`.

- ...:

  Additional arguments to
  [`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html),

## Value

Returns the specified plot as a `ggplot` object.

## Details

Allows group variable for faceting

## Note

Adapted from a helpful [blog
post](https://www.mathiasecology.com/code/add-tukeys-significant-letters-to-ggplots)
by [Justin Mathias](https://github.com/justinmathias).

## Author

Ethan Bass

## Examples

``` r
set.seed(1)
data <- data.frame("Category" = c(rep("Low", 10), rep("Medium", 10), rep("High", 10)),
                   "Value" = c(rnorm(10, 5), rnorm(10, 5.5), rnorm(10, 10)),
                   "Size" = c("Big","Small"))
boxplot_letters(data, Category, Value)

boxplot_letters(data, x=Category, y=Value, group=Size)
```
