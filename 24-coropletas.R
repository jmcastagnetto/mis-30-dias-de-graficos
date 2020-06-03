library(tidyverse)
library(tmap)
library(rnaturalearth)

sunedu_raw <- read_delim(
  file = "datos/sunedu_licenciamiento_universidades-utf8.txt",
  delim = "|"
) %>%
  filter(!is.na(CODIGO_ENTIDAD)) %>%
  janitor::clean_names() %>%
  mutate_at(
    vars(tipo_gestion, estado_licenciamiento,
         departamento_local, provincia_local, distrito_local),
    factor
  )

sunedu_df <- sunedu_raw %>%
  filter(estado_licenciamiento == "LICENCIA OTORGADA") %>%
  group_by(departamento_local) %>%
  tally(name = "N°")

pe_shp <- ne_states(country = "Peru",  returnclass = "sf") %>%
  mutate(
    departamento = str_to_upper(name)
  ) %>%
  left_join(
    sunedu_df,
    by = c("departamento" = "departamento_local")
  )

tmap_mode("plot")

pe_map <- tm_shape(pe_shp) +
  tm_fill("N°",
          palette = "YlOrRd",
          style = "cat") +
  tm_text("N°") +
  tm_borders() +
  tm_compass(position = c(.05, .3)) +
  tm_credits(
    text = "#30diasdegraficos\n2020-06-04\n@jmcastagnetto, Jesus M. Castagnetto",
    size = 0.9,
    fontfamily = "Inconsolata",
    position = c("left", "bottom")
  ) +
  tm_layout(
    main.title = "Universidades licenciadas por\nSUNEDU (al 2020-05-30)",
    main.title.position = "center",
    main.title.size = 1.5,
    title = "Fuente: SUNEDU\nDatos abiertos.",
    title.position = c(.1, .9),
    title.size = .9,
    bg.color = "grey85",
    legend.show = FALSE,
    outer.margins = rep(.01, 4)
  )

pe_map

tmap_save(
  tm = pe_map,
  filename = "24-coropletas-sunedu-universidades-licenciadas.png",
  width = 6,
  height = 7
)

