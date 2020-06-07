library(tidyverse)
library(readxl)
library(countrycode)
library(GGally)

# Indicadores de educación del BID
iadb_learning_indicators <- read_csv("datos/Learning_Improvement_Information_Center___Regional_Indicators_for_Learning.csv.gz")

iadb_selected <- iadb_learning_indicators %>%
  filter(Fuente_Especifica == "PISA" &
           Desagregacion == "No_Desagregado" &
           Nombre_Indicador == "Puntaje_Prom") %>%
  arrange(Materia, Ano, Valor) %>%
  select(Materia, Ano, Valor, Pais) %>%
  filter(!str_detect(Pais, "_")) %>%
  mutate(
    nombre_pais = countrycode(Pais,
                              origin = "iso3c",
                              destination = "un.name.es")
  ) %>%
  select(-Pais) %>%
  pivot_wider(
    names_from = Materia,
    values_from = Valor
  ) %>%
  filter(Ano == 2018)

lbls <- iadb_selected %>%
  select(Matematicas, nombre_pais) %>%
  mutate(
    Matematicas = if_else(nombre_pais == "Uruguay",
                          Matematicas + 5, # para evitar cruce con Chile
                          Matematicas),
    Matematicas = if_else(nombre_pais == "Costa Rica",
                          Matematicas + 1.5, # para evitar cruce con Perú
                          Matematicas),
  )


p <- ggparcoord(
  iadb_selected,
  columns = 3:5,
  groupColumn = 2,
  showPoints = TRUE,
  scale = "globalminmax"
)

p +
  geom_vline(xintercept = 1:3, color = "grey60",
             linetype = "dashed") +
  labs(
    y = "",
    x = "",
    title = "Puntajes promedio en PISA 2018: Latinoamérica y el Caribe",
    subtitle = "Fuente: IADB Data (https://bit.ly/2LsB2Yb)",
    caption = "2020-06-09 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  scale_color_viridis_d(name = "") +
  annotate(
    "text",
    y = lbls$Matematicas,
    label = lbls$nombre_pais,
    color = lbls$nombre_pais,
    x = rep(3.05, nrow(lbls)),
    hjust = 0,
    size = 5
  ) +
  ggthemes::theme_pander(20) +
  theme(
    legend.position = "none",
    axis.title = element_text(hjust = 1),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.caption = element_text(family = "Inconsolata", size = 14)
  )

