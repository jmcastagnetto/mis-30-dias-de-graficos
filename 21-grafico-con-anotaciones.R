library(tidyverse)

# Fuente: https://www.inei.gob.pe/media/MenuRecursivo/publicaciones_digitales/Est/Lib1173/Mapas.pdf
grupo_pobreza_enaho_2013 <- data.frame(
  departamento = c(
    # Grupo 1
    "AMAZONAS",
    "AYACUCHO",
    "CAJAMARCA",
    "HUANCAVELICA",
    "PASCO",
    # Grupo 2
    "APURIMAC",
    "HUANUCO",
    "LORETO",
    "PIURA",
    # Grupo 3
    "LA LIBERTAD",
    "PUNO",
    "SAN MARTIN",
    # Grupo 4
    "ANCASH",
    "LAMBAYEQUE",
    "CUSCO",
    "JUNIN",
    # Grupo 5
    "CALLAO",
    "LIMA",
    "TACNA",
    "TUMBES",
    "UCAYALI",
    # Grupo 6
    "AREQUIPA",
    "MOQUEGUA",
    # Grupo 7
    "ICA",
    "MADRE DE DIOS"
  ),
  grupo = c(
    rep(1, 5),
    rep(2, 4),
    rep(3, 3),
    rep(4, 4),
    rep(5, 5),
    rep(6, 2),
    rep(7, 2)
  )
)



fallecidos_url <- "https://raw.githubusercontent.com/jmcastagnetto/covid-19-peru-limpiar-datos-minsa/master/datos/fallecidos_covid-utf8-limpio.csv"


fallecidos_raw <- read_csv(fallecidos_url)

fallecidos_df <- fallecidos_raw %>%
  left_join(
    grupo_pobreza_enaho_2013,
    by = "departamento"
  ) %>%
  mutate(
    grupo_lbl = paste("Grupo", grupo),
    sexo_lbl = if_else(sexo == "Femenino", "♀" , "♂")
  ) %>%
  filter(sexo %in% c("Femenino", "Masculino"))

fallecidos_promedio_df <- fallecidos_df %>%
  group_by(sexo_lbl, grupo_lbl) %>%
  summarise(
    n_total = n(),
    edad_promedio = mean(edad, na.rm = TRUE),
    edad_sd = sd(edad, na.rm = TRUE),
    edad_lbl = sprintf("Promedio: %.1f (±%.1f)\nN = %d", edad_promedio, edad_sd, n_total)
  )

binwidth <- 2

hist_plot <- ggplot(fallecidos_df, aes(x = edad, group = sexo_lbl)) +
  geom_histogram(aes(fill = sexo_lbl), binwidth = binwidth) +
  geom_density(aes(y = binwidth * ..count..)) +
  geom_vline(data = fallecidos_promedio_df,
             aes(xintercept = edad_promedio),
             color = "black", size = 2, alpha = .5) +
  geom_text(
    data = fallecidos_promedio_df,
    aes(label = edad_lbl),
    x = 25, y = Inf, fontface = "bold",
    size = 6, hjust = .5, vjust = 1.2
  ) +
  # geom_curve(
  #   data = fallecidos_promedio_df,
  #   aes(x = 25, y = 25, xend = edad_promedio, yend = 80),
  #   angle = 225, lineend = "round", linetype = "dashed",
  #   arrow = arrow(length = unit(.25, "cm"), type = "closed")
  # ) +
  labs(
    y = "Frecuencia",
    x = "Edad (en años)",
    title = "Edades de fallecimiento por COVID-19 en Perú (al 2020-05-29)",
    subtitle = "Fuentes: MINSA (https://bit.ly/covid19minsafallecidos)\n\tINEI (ENAHO 2013, grupos de pobreza)",
    caption = "2020-06-01 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  facet_grid(grupo_lbl ~ sexo_lbl, scales = "free_y") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw(24) +
  theme(
    legend.position = "none",
    plot.margin = unit(rep(1, 4), "cm"),
    plot.caption = element_text(family = "Inconsolata"),
    strip.text.x = element_text(size = 46)
  )

hist_plot

ggsave(
  plot = hist_plot,
  filename = "21-grafico-con-anotaciones-fallecimientos-covid19-peru-sexo-lugar.png",
  width = 14,
  height = 16
)
