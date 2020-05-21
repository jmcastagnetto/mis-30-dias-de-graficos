library(tidyverse)
library(patchwork)

bid_data <- read_csv("datos/Learning_Improvement_Information_Center___Regional_Indicators_for_Learning.csv.gz")

mate_df <- bid_data %>%
  filter(Desagregacion == "No_Desagregado" &
           Materia == "Matematicas" &
           Nombre_Indicador == "Puntaje_Prom") %>%
  mutate(
    yr = factor(Ano),
    src = Fuente_Especifica %>% replace_na("Desconocido"),
    src = if_else(src %in% c("SERCE", "TERCE"), "SERCE/TERCE", src) %>%
      factor()
  )

p_base <- ggplot(mate_df,
       aes(y = yr, x = Valor,
           group = yr,
           fill = src)) +
  geom_boxplot(varwidth = TRUE, show.legend = FALSE) +
  facet_wrap(~src, nrow = 1, scales = "free")


p1 <- p_base +
  labs(
    x = "",
    y = "",
    subtitle = "artyfarty::theme_monokai + wesanderson::GrandBudapest2"
  ) +
  artyfarty::theme_monokai() +
  paletteer::scale_fill_paletteer_d("wesanderson::GrandBudapest2")

p2 <- p_base +
  theme_linedraw() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "",
    y = "",
    subtitle = "theme_linedraw + scale_fill_brewer"
  )

p3 <- p_base +
  labs(
    x = "",
    y = "",
    subtitle = "ggthemes::theme_tufte + wesanderson::Chevalier1"
  ) +
  ggthemes::theme_tufte() +
  paletteer::scale_fill_paletteer_d("wesanderson::Chevalier1")

p4 <- p_base +
  labs(
    x = "",
    y = "",
    subtitle = "ggdark::dark_theme_linedraw + scale_color_viridis_d"
  ) +
  ggdark::dark_theme_linedraw() +
  scale_color_viridis_d()

p_comb <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Distribución de los promedios de Matemáticas - Cuatro reinterpretaciones visuales en ggplot2",
    subtitle = "Fuente: IADB Data (https://bit.ly/2LsB2Yb). Pruebas: PISA (OECD), y SERCE/TERCE (UNESCO)",
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto",
    tag_levels = "A"
  ) &
  theme(
    text = element_text(size = 18, family = "Roboto"),
    plot.tag = element_text(size = 12, face = "bold.italic"),
    plot.caption = element_text(family = "Inconsolata")
  )

p_comb

ggsave(
  plot = p_comb,
  filename = "10-explorando-paletas-de-colores-4-temas.png",
  width = 16,
  height = 9
)
