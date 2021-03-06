context("test-vdiffr")

# Thanks to Lionel Henry for the suggestion on using vdiffr conditionally
# See https://github.com/lionel-/ggstance for the original
expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

five_moons <- data.frame(
  x = 1:5,
  y = 1,
  size = 2^(1:5),
  ratio = 0:4 * 0.25,
  group = letters[1:5]
)

moon_size <- ggplot(five_moons, aes(x = x, y = y, size = size)) +
  geom_moon(ratio = 0.5, right = FALSE) +
  scale_size_area(max_size = 30)

expect_doppelganger("moons scale correctly", moon_size)

moon_missing <- ggplot(
  five_moons, aes(x = x, y = y, ratio = ratio, fill = group, color = group)) +
  geom_moon(size = 10)

expect_doppelganger("zero ratio moons are invisible", moon_missing)

full_moons <- ggplot(five_moons, aes(x = x, y = y)) +
  geom_moon(aes(ratio = ratio), fill = "black", size = 10) +
  geom_moon(aes(ratio = 1 - ratio), right = FALSE, size = 10)

expect_doppelganger("moons can be complemented", full_moons)
