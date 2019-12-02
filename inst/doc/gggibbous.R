## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

required <- c("maps", "mapproj", "knitr")
if (!all(sapply(required, requireNamespace, quietly = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ---- echo = FALSE, fig.width = 3, fig.height = 1.5----------------------
ggplot2::ggplot(
  data.frame(
    y = c(1, 3), group = c("a", "b"),
    facet = factor(c(rep("pie", 2)), levels = c("pie", "moon"))
  ),
  ggplot2::aes(y = y, fill = group)
) +
  ggplot2::geom_col(ggplot2::aes(x = factor(0)), width = 2, color = "black") +
  gggibbous::geom_moon(
    data = data.frame(
      x = factor(rep(-0.5, 2)), y = rep(0, 2), ratio = c(0.75, 0.25),
      right = c(TRUE, FALSE), group = c("b", "a"), facet = c("moon", "moon")
    ),
    ggplot2::aes(x = x, y = y, ratio = ratio, right = right),
    size = 36, stroke = 0.5
  ) +
  ggplot2::coord_polar(theta = "y") +
  ggplot2::facet_wrap(~facet) +
  ggplot2::scale_fill_manual(values = c("white", "black"), guide = "none") +
  ggplot2::theme_void(16)

## ----setup---------------------------------------------------------------
library(gggibbous)

## ---- fig.width = 4, fig.height = 2--------------------------------------
ggplot(data.frame(x = 1:5, y = 1, size = 2^(0:4)), aes(x, y, size = size)) +
  geom_moon() +
  geom_point(y = 2) +
  lims(x = c(0.5, 5.5), y = c(0.5, 2.5)) +
  scale_size(range = c(5, 10))

## ---- fig.width = 5, fig.height = 1.25-----------------------------------
ggplot(data.frame(x = 1:5, y = 0, ratio = 0:4 * 0.25), aes(x = x, y = y)) +
  geom_moon(aes(ratio = ratio), size = 20, fill = "black") +
  geom_text(aes(y = y + 1, label = ratio)) +
  lims(x = c(0.5, 5.5), y = c(-1, 1.4)) +
  theme_void()

## ---- fig.width = 3.5, fig.height = 2.5----------------------------------
tidymoons <- data.frame(
  x = rep(1:3, 6),
  y = rep(rep(3:1, each = 3), 2),
  ratio = c(1:9 / 10, 9:1 / 10),
  right = rep(c(TRUE, FALSE), each = 9)
)

ggplot(tidymoons) +
  geom_moon(aes(x, y, ratio = ratio, right = right, fill = right)) +
  lims(x = c(0.5, 3.5), y = c(0.5, 3.5))

## ---- fig.width = 3.5, fig.height = 2.5----------------------------------
ggplot(tidymoons, aes(x, y, ratio = ratio, right = right, size = 2^x)) +
  geom_moon(data = subset(tidymoons, right), fill = "violetred") +
  geom_moon(
    data = subset(tidymoons, !right), fill = "turquoise3",
    key_glyph = draw_key_moon_left
  ) +
  lims(x = c(0.5, 3.5), y = c(0.5, 3.5)) +
  scale_size("size", range = c(5, 10), breaks = 2^(1:3))

## ---- fig.width = 4.5, fig.height = 2.5----------------------------------
ggplot(tidymoons) +
  geom_moon(
    aes(x, y, ratio = ratio, right = right, fill = right, size = 2^x),
    key_glyph = draw_key_full_moon
  ) +
  lims(x = c(0.5, 3.5), y = c(0.5, 3.5)) +
  scale_size("size", range = c(5, 10), breaks = 2^(1:3)) +
  scale_fill_manual(values = c("firebrick1", "dodgerblue2")) +
  theme(legend.box = "horizontal")

## ---- fig.width = 6, fig.height = 5.45-----------------------------------
dmeladh_adj <- dmeladh
dmeladh_adj$long <- dmeladh$Longitude + c(
  -2, 0, -2, 2, -3, 3, 3, 2, 3, 4, -2.5, -2.5, -1, -2, -2.5, -4, 2.5,
  5, 6, 7, 2, -7, -5.5, -3, 0, -7, -2, 3, 5.5, 0.5, -1, -1.5, -3, 2)
dmeladh_adj$lat <- dmeladh$Latitude + c(
  -2, 2, 0, 1, 0, 0, 0, 2, 0.5, -1, 1, -1.5, 2, 4, 1.5, 0, 2,
  1, -1, -3, -2, 1, -1, -2, -3, -2, -4, -3, -1, 1.5, 2, 2, -2, 0)

moonmap <- ggplot(dmeladh_adj, aes(long, lat)) +
  geom_polygon(
    data = map_data(
      "world", region = "(Australia)|(Indonesia)|(Papua New Guinea)"),
    aes(group = group),
    fill = "gray80"
  ) +
  geom_segment(aes(xend = Longitude, yend = Latitude), color = "gray20") +
  geom_point(aes(Longitude, Latitude), size = 0.75, color = "gray20") +
  scale_size(range = c(4, 10)) +
  coord_map(xlim = c(110, 160), ylim = c(-45, -5)) +
  theme_void() +
  theme(
    legend.position = c(0.05, 0.05),
    legend.direction = "horizontal",
    legend.justification = c(0, 0)
  )

moonmap +
  geom_moon(
    aes(ratio = AdhS / 100, size = N),
    right = FALSE, fill = "gold", color = "gold",
    key_glyph = draw_key_moon_left
  ) +
  geom_moon(
    aes(ratio = AdhF / 100, size = N),
    fill = "forestgreen", color = "forestgreen"
  )

## ---- fig.width = 6, fig.height = 5.45-----------------------------------
tidyadh <- reshape(
  dmeladh_adj,
  varying = c("AdhF", "AdhS"),
  v.names = "percent",
  timevar = "allele",
  times = c("AdhF", "AdhS"),
  idvar = c("Locality", "Latitude", "Longitude", "long", "lat", "N"),
  direction = "long"
)
tidyadh$right <- rep(c(TRUE, FALSE), each = nrow(dmeladh_adj))

moonmap +
  geom_moon(
    data = tidyadh, key_glyph = draw_key_full_moon,
    aes(ratio = percent / 100, fill = allele, color = allele, right = right,
        size = N)
  ) +
  scale_fill_manual(values = c("forestgreen", "gold")) +
  scale_color_manual(values = c("forestgreen", "gold"))

## ---- fig.width = 7, fig.height = 3.5------------------------------------
moonphase <- subset(lunardist, !is.na(phase))
moonphase$percent <- ifelse(
  moonphase$phase == "new", 0, ifelse(moonphase$phase == "full", 1, 0.5))

ggplot(lunardist, aes(date, distance)) +
  geom_line() +
  # Plotting the lower layer as a full circle also works in most cases
  geom_moon(data = moonphase, ratio = 1, size = 5, fill = "black") +
  geom_moon(
    data = moonphase, aes(ratio = percent),
    size = 5, fill = "yellow", right = moonphase$phase == "first quarter"
  )

## ------------------------------------------------------------------------
rest_names <- c(
  "Anscombe's Luncheonette", "Chai Squared", "Tukey's Honest Southern Diner",
  "Bagels ANOVA", "Spearmint Row"
)
restaurants <- data.frame(
  Restaurant = factor(rest_names, levels = rest_names),
  Food = c(5, 3, 4, 4, 1),
  Decor = c(2, 5, 3, 1, 5),
  Service = c(4, 2, 3, 3, 5),
  Price = c(4, 5, 2, 5, 2)
)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(restaurants, align = "lcccc")

## ---- fig.width = 4.5, fig.height = 3------------------------------------
# First we reshape the data into "long" format to facilitate plotting
rest_cats <- c("Food", "Decor", "Service", "Price")
tidyrest <- reshape(
  restaurants,
  varying = rest_cats,
  v.names = "Score",
  timevar = "Category",
  times = factor(rest_cats, levels = rest_cats),
  idvar = "Restaurant",
  direction = "long"
)

ggplot(tidyrest, aes(0, 0)) +
  geom_moon(aes(ratio = (Score - 1) / 4), fill = "black") +
  geom_moon(aes(ratio = 1 - (Score - 1) / 4), right = FALSE) +
  facet_grid(Restaurant ~ Category, switch = "y") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 180, hjust = 1),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

