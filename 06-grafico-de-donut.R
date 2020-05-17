# Este no es un tipo de gráficos que me guste pues
# no es simple de entender y es poco claro si se tienen
# varias categorías
#
# Adaptado de https://www.r-graph-gallery.com/128-ring-or-donut-plot.html

library(tidyverse)
library(ggtext)
library(lubridate)
library(HistData)

df <- Cholera %>%
  group_by(region) %>%
  summarise(
    deaths = sum(cholera_deaths)
  ) %>%
  mutate(
    pct_deaths = 100 * deaths / sum(deaths),
    ymax = cumsum(pct_deaths),
    ymin = lag(ymax) %>%
      replace_na(0),
    label_pos = ymin + (ymax - ymin) / 2,
    label = paste0(
      "**", region, "**",
      "<br/>*N = ", format(deaths, big.mark = ","),
      "*<br/>(", sprintf("%.1f%%", pct_deaths), ")"
    )
  ) %>%
  ungroup()

ggplot(df,
       aes(ymin = ymin, ymax = ymax,
           xmin = 3, xmax = 4, fill = region,
           y = label_pos, label = label)) +
  geom_rect(show.legend = FALSE) +
  geom_richtext(
    fill = NA, label.color = NA,
    x = 3.5, size = 5) +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  coord_polar(theta = "y") +
  xlim(2, 4) +
  theme_void(16) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(family = "Inconsolata"),
    plot.margin = unit(rep(1, 4), "cm")
  ) +
  labs(
    title = "Fallecimientos por cólera en Londres por región (1849)",
    subtitle = "Fuente: Datos de William Farr en el paquete \"HistData\"",
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  )

ggsave(
  filename = "06-grafico-de-donut-colera-londres-1849.png",
  width = 9,
  height = 9
)
