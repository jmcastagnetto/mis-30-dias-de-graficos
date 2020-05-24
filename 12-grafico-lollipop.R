library(tidyverse)

csv_url <- "https://github.com/jmcastagnetto/covid-19-peru-limpiar-datos-minsa/raw/master/datos/FALLECIDOS_CDC-utf8-limpio.csv.gz"

minsa_url <- "https://bit.ly/covid19minsafallecidos"

repo_url <- "https://bit.ly/2ZxSnaC"

fallecimientos <- read_csv(csv_url)

por_departamento <- fallecimientos %>%
  mutate(
    departamento = fct_infreq(departamento, ordered = TRUE)
  ) %>%
  group_by(departamento) %>%
  tally()

ggplot(
  por_departamento,
  aes(x = n, y = departamento,
      color = departamento)
) +
  geom_segment(aes(xend = 0, yend = departamento),
               size = 2,
               show.legend = FALSE) +
  geom_point(size = 4, show.legend = FALSE) +
  scale_color_viridis_d() +
  scale_y_discrete(limits = rev(por_departamento$departamento)) +
  labs(
    x = "",
    y = "",
    title = "Perú: Fallecimientos por COVID-19 por Departamento.",
    subtitle = paste0("Hasta el 2020-05-21.\nFuentes: ", minsa_url, " (MINSA) y ",
                      repo_url, "."),
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"

  ) +
  ggthemes::theme_tufte(20) +
  theme(
    plot.caption = element_text(family = "Inconsolata"),
    plot.margin = unit(rep(1, 4), "cm")
  )

ggsave(
  filename = "12-grafico-lollipop-fallecidos-covid-peru.png",
  height = 10,
  width = 12
)
