# Check whether color specifications exists.

Function to check whether all specified colors are actual colors.

## Usage

``` r
is.color(x, return.colors = FALSE)
```

## Arguments

- x:

  Vector of any of the three kinds of R color specifications, i.e.,
  either a color name (as listed by
  [`palette`](https://rdrr.io/r/grDevices/palette.html)`colors()`), a
  hexadecimal string of the form '#rrggbb' or '#rrggbbaa' (see rgb), or
  a positive integer i meaning
  [`palette`](https://rdrr.io/r/grDevices/palette.html)`()[i]`.

- return.colors:

  Logical: logical values (FALSE, default) or returning colors (TRUE)

## Value

Logical value (or colors)

## Note

Adapted from plotfunctions packaage
https://cran.r-project.org/web/packages/plotfunctions/index.html

## Author

Jacolien van Rij

## Examples

``` r
# correct color definitions:
is.color(c('#FF0000FF', '#00FF00FF', '#0000FFFF'))
#> [1] TRUE TRUE TRUE
is.color(c('red', 'steelblue', 'green3'))
#> [1] TRUE TRUE TRUE
is.color(c(1,7,28))
#> [1] TRUE TRUE TRUE
# mixtures are possible too:
is.color(c('#FF0000FF', 'red', 1, '#FF0000', rgb(.1,0,0)))
#> [1] TRUE TRUE TRUE TRUE TRUE

# return colors:
# note that 28 is converted to 4...
is.color(c(1,7,28), return.colors=TRUE)
#> [1] "black"   "#F5C710" "#2297E6"
is.color(c('#FF0000CC', 'red', 1, '#FF0000'), return.colors=TRUE)
#> [1] "#FF0000CC" "red"       "black"     "#FF0000"  

# 4 incorrect colors, 1 correct:
test <- c('#FH0000', 3, '#FF00991', 'lavendel', '#AABBCCFFF')
is.color(test)
#> [1] FALSE  TRUE FALSE FALSE FALSE
is.color(test, return.colors=TRUE)
#> [1] NA        "#61D04F" NA        NA        NA       
```
