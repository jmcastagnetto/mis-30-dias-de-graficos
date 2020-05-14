library(tidyverse)

# casos de COVID-19 en Perú por día
casos_prueba <- read_csv("https://raw.githubusercontent.com/jmcastagnetto/covid-19-peru-data/master/datos/covid-19-peru-test-results.csv")

plot_df <- casos_prueba %>%
  filter(resultado == "positivo") %>%
  arrange(fecha) %>%
  group_by(tipo_prueba) %>%
  mutate(
    nuevos_positivos = personas - lag(personas),
    tipo_prueba = str_to_title(tipo_prueba)
  )

ggplot(plot_df,
       aes(x = fecha, y = nuevos_positivos,
           group = tipo_prueba,
           color = tipo_prueba)) +
  geom_line(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(name = "Tipo de prueba", type = "qual") +
  theme_minimal(18) +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "",
    title = "Nuevos casos de COVID-19 detectados por tipo de prueba (Perú)",
    subtitle = "Fuente: https://github.com/jmcastagnetto/covid-19-peru-data/",
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  )

ggsave(
  filename = "02-grafico-de-lineas-casos-nuevos-covid19-peru.png",
  width = 12,
  height = 8
)
