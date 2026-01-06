library(palmerpenguins)
data(penguins)

test_that("geom_tukey works for simple boxplot", {
  skip_if_not_installed("vdiffr")
  p <- penguins |> tidyr::drop_na(sex, species) |>
    ggplot(aes(x=species, y=body_mass_g)) +
    geom_boxplot() + geom_tukey()
  vdiffr::expect_doppelganger("basic_tukey_plot", p)
})

test_that("geom_tukey works for boxplot with facet", {
  skip_if_not_installed("vdiffr")
  p2 <- penguins |> tidyr::drop_na(sex, species) |>
    ggplot(aes(x=species, y=body_mass_g)) + facet_wrap(~sex) +
    geom_boxplot() + geom_tukey(type=2)
  vdiffr::expect_doppelganger("faceted_tukey_plot", p2)

  p1 <- penguins |> tidyr::drop_na(sex, species) |>
    ggplot(aes(x=species, y=body_mass_g)) + facet_wrap(~sex) +
    geom_boxplot() + geom_tukey(type=1)
  vdiffr::expect_doppelganger("faceted_tukey_plot_type1", p1)

  p_r <- penguins |> tidyr::drop_na(sex, species) |>
    ggplot(aes(x=species, y=body_mass_g)) + facet_wrap(~sex) +
    geom_boxplot() + geom_tukey(reversed = TRUE)
  vdiffr::expect_doppelganger("faceted_tukey_plot_reversed", p_r)
})

test_that("boxplot_letters functions", {
  p <- penguins |> tidyr::drop_na(sex, species) |>
    boxplot_letters(x = species, y=body_mass_g, fill="white") +
    theme_gray()
  vdiffr::expect_doppelganger("boxplot_letters_basic", p)
})

test_that("boxplot_letters works with facet", {
  p <- penguins |> tidyr::drop_na(sex, species) |>
    boxplot_letters(x = species, y=body_mass_g, group=sex, fill="white") +
    theme_gray()
  vdiffr::expect_doppelganger("boxplot_letters_faceted", p)
  p_r <- penguins |> tidyr::drop_na(sex, species) |>
    boxplot_letters(x = species, y=body_mass_g, group=sex, fill="white",
                    reversed = TRUE) +
    theme_gray()
  vdiffr::expect_doppelganger("boxplot_letters_faceted_reversed", p_r)
})

test_that("geom_tukey works for boxplot with facet", {
  skip_if_not_installed("vdiffr")
  p <- penguins |> tidyr::drop_na(sex, species) |>
    ggplot(aes(x=species, y=body_mass_g)) + facet_wrap(~sex) +
    geom_boxplot() + geom_tukey(test="kruskalmc", type = 1)
  vdiffr::expect_doppelganger("faceted_kruskalmc_plot", p)

  expect_warning(p2 <- penguins |> tidyr::drop_na(sex, species) |>
    ggplot(aes(x=species, y=body_mass_g)) + facet_wrap(~sex) +
    geom_boxplot() + geom_tukey(test="kruskalmc", type = 2))
})

test_that("geom_tukey works for boxplot with facet", {
  skip_if_not_installed("vdiffr")
  p1 <- penguins |> tidyr::drop_na(sex, species) |>
    ggplot(aes(x=species, y=body_mass_g)) + facet_wrap(~sex) +
    geom_boxplot() + geom_tukey(test="dunn", type = 1)
  vdiffr::expect_doppelganger("dunn_type1", p1)

  p2 <- penguins |> tidyr::drop_na(sex, species) |>
    ggplot(aes(x=species, y=body_mass_g)) + facet_wrap(~sex) +
    geom_boxplot() + geom_tukey(test="dunn", type = 2)
  vdiffr::expect_doppelganger("dunn_type2", p2)
})

