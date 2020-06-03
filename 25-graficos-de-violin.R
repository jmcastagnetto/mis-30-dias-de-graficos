library(tidyverse)
library(readxl)

contratos_covid_url <- "https://www.datosabiertos.gob.pe/sites/default/files/CONOSCE_CONTRATACIONDIRECTA.xlsx"

dest_file <- "datos/CONOSCE_CONTRATACIONDIRECTA.xlsx"

if(!file.exists(dest_file)) {
  download.file(
    url = contratos_covid_url,
    destfile = dest_file
  )
}

contratos_covid_raw <- read_excel(dest_file)

contratos_covid_df <- contratos_covid_raw %>%
  select(TIPOENTIDADOEE,
         OBJETOCONTRACTUAL,
         MONTOADJUDICADOSOLES) %>%
  filter(OBJETOCONTRACTUAL %in% c("Bien", "Servicio")) %>%
  mutate(
    TIPOENTIDADOEE = str_to_title(TIPOENTIDADOEE) %>%
      str_wrap(10)
  )

violin_plot <- ggplot(contratos_covid_df,
       aes(y = MONTOADJUDICADOSOLES,
           x = TIPOENTIDADOEE)) +
  geom_violin(aes(fill = OBJETOCONTRACTUAL),
              alpha = .7,
              draw_quantiles = c(.25, .5, .75)) +
  scale_y_log10(labels = scales::dollar_format(prefix = "S/.")) +
  annotation_logticks(sides = "l") +
  labs(
    fill = "",
    x = "",
    y = "",
    title = "Distribución de montos adjudicados en contrataciones directas\ndurante la emergencia de COVID-19 en Perú (al 2020-05-03)",
    subtitle = "Fuente: https://www.datosabiertos.gob.pe/dataset/contrataciones-ante-la-emergencia-sanitaria-por-la-existencia-del-coronavirus-organismo",
    caption = "2020-06-05 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  ggthemes::theme_few(26) +
  scale_fill_manual(values = c("peru", "cyan")) +
  theme(
    legend.position = c(.9,.9),
    legend.background = element_blank(),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(family = "Inconsolata"),
    plot.margin = unit(rep(1, 4), "cm")
  )

# violin_plot

ggsave(
  plot = violin_plot,
  filename = "25-graficos-de-violin-contratos-covid19-peru.png",
  width = 16,
  height = 10
)

# reducir el tamaño para Twitter
img <- magick::image_read("25-graficos-de-violin-contratos-covid19-peru.png")
img_resized <- magick::image_scale(img, "1024")
magick::image_write(img_resized, "25-graficos-de-violin-contratos-covid19-peru-resized.png")
