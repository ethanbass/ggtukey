# Changelog

## ggtukey 0.5.0

- Added support for Dunn’s test as implemented in the `rstatix` package.
- Changed default letter order, so the group with the largest value is
  assigned the ‘a’.
- Added `reversed` argument to reverse letter order (so that the small
  value is assigned the ‘a’).

## ggtukey 0.4.0

- Fixed bug when using `kruskalmc` (Dunn test) due to changes in
  [`pgirmess::kruskalmc`](https://rdrr.io/pkg/pgirmess/man/kruskalmc.html)
  output.

## ggtukey 0.3.0

- Added additional parameters to `geom_tukey` (`geom`, `color`, `fill,`
  & `alpha`) for further customization of letters.

## ggtukey 0.2.0

- Added additional options to `where` parameter for placement of
  letters.
- Allowed numeric arguments to `type` argument in `geom_tukey`.
- Added citation file.
- Changed `vjust` default to `-0.2`.

## ggtukey 0.1.0

- Added a `NEWS.md` file to track changes to the package.
