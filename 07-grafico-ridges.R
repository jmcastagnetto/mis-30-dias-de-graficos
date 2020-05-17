library(tidyverse)
library(ggridges)

diamantes <- diamonds %>%
  mutate(
    corte = case_when(
      cut == "Ideal" ~ "Corte Ideal",
      cut == "Premium" ~ "Corte Premium",
      cut == "Very Good" ~ "Corte Muy Bueno",
      cut == "Good" ~ "Corte Bueno",
      cut == "Fair" ~ "Corte Aceptable"
    ),
    corte = factor(
      corte,
      levels = rev(c("Corte Ideal", "Corte Premium", "Corte Muy Bueno", "Corte Bueno", "Corte Aceptable")),
      ordered = TRUE
    )
  )

ggplot(diamantes, aes(x = price, y = corte,
                     fill = corte, color = corte)) +
  geom_density_ridges(show.legend = FALSE, alpha = 0.3) +
  theme_ridges(18) +
  scale_x_log10(labels = scales::dollar) +
  annotation_logticks(sides = "b") +
  labs(
    x = "Precio (escala logarítmica)",
    y = "",
    title = "Distribución de Precios de Diamantes por Tipo de Corte",
    subtitle = "Fuente: datos \"diamonds\" de {ggplot2}",
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto\nCon ayuda de mi Hijo (él escribió el código) 😉😉😉"
  ) +
  theme(
    plot.caption = element_text(family = "Inconsolata"),
    plot.title.position = "plot"
  )

ggsave(
  filename = "07-grafico-ridges-precio-diamantes.png",
  width = 11,
  height = 7
)
