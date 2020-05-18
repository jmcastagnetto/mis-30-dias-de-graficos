library(tidyverse)

f <- function(x, y) {
  (y**cos(x)^2 - x**sin(y)^2) / (x + y)
}

x <- seq(1, 10, length.out = 300)
y <- seq(1, 10, length.out = 300)

df <- expand_grid(x = x, y = y) %>%
  mutate(
    z = f(x, y)
  )

min_z <- sprintf("%.1f", min(df$z))
max_z <- sprintf("%.1f", max(df$z))

ggplot(df, aes(x = x, y = y)) +
  geom_contour(aes(z = z, color = ..level..),
               breaks = seq(-.8, .8, by = .05)) +
  coord_equal() +
  labs(
    x = bquote(italic(x)),
    y = bquote(italic(y)),
    title = bquote("Graficando la función" ~ italic(f)[x][y] == frac(y**italic(cos)(x)^2 - x**italic(sin)(y)^2, x + y)),
    subtitle = paste0("Rangos: x,y en [1, 10]; z en [", min_z, ", ", max_z ,"]"),
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  scale_color_viridis_c(name = bquote(italic(f)[x][y] ~ " "),
                        option = "plasma",
                        limits = c(-.8, .8),
                        breaks = seq(-.8, .8, by = .2)) +
  theme_minimal(12) +
  theme(
    plot.caption = element_text(family = "Inconsolata"),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(.25, "cm"),
    panel.grid = element_blank(),
    axis.ticks = element_line()
  )

ggsave(
  filename = "08-graficos-de-contorno-funcion-periodica.png",
  width = 6,
  height = 6
)
