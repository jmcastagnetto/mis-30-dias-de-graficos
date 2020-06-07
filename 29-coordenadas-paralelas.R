library(tidyverse)
library(readxl)
library(countrycode)
library(GGally)
library(RColorBrewer)


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
  filter(Ano == 2018) %>%
  rename(
    "Matemáticas" = Matematicas
  )

lbls <- iadb_selected %>%
  select(Matematicas, nombre_pais) %>%
  mutate(
    Matematicas = if_else(nombre_pais == "Uruguay",
                          Matematicas + 5, # para evitar cruce con Chile
                          Matematicas),
    Matematicas = if_else(nombre_pais == "Costa Rica",
                          Matematicas + 1.5, # para evitar cruce con Perú
                          Matematicas),
  ) %>%
  arrange(Matematicas)

colores <- wesanderson::wes_palette("FantasticFox1",
                                    10, "continuous")
cmap <- c()
for (k in 1:length(lbls$nombre_pais)) {
  cmap[lbls$nombre_pais[k]] = colores[k]
}


p <- ggparcoord(
  iadb_selected,
  columns = 3:5,
  groupColumn = 2,
  showPoints = TRUE,
  scale = "globalminmax"
) +
  geom_vline(xintercept = 1:3, color = "grey20",
             linetype = "dashed") +
  labs(
    y = "",
    x = "",
    title = "Puntajes promedio en PISA 2018: Latinoamérica y el Caribe",
    subtitle = "Fuente: IADB Data (https://bit.ly/2LsB2Yb)",
    caption = "2020-06-09 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  annotate(
    "text",
    y = lbls$Matematicas,
    label = lbls$nombre_pais,
    color = cmap,
    x = rep(3.05, nrow(lbls)),
    hjust = 0,
    size = 5
  ) +
  scale_color_manual(name = "", values = cmap) +
  theme_light(22) +
  theme(
    legend.position = "none",
    panel.grid = element_line(colour = "grey80",
                              linetype = "dashed"),
    axis.title = element_text(hjust = 1),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.caption = element_text(family = "Inconsolata", size = 14)
  )

ggsave(
  plot = p,
  filename = "29-coordenadas-paralelas-pisa2018-promedios-latamcaribe.png",
  height = 9,
  width = 14
)

img <- magick::image_read("29-coordenadas-paralelas-pisa2018-promedios-latamcaribe.png")
img_resized <- magick::image_scale(img, "25%")
magick::image_write(
  img_resized,
  "29-coordenadas-paralelas-pisa2018-promedios-latamcaribe-resized25.png"
)
