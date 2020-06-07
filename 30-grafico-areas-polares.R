library(tidyverse)
library(patchwork)

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
  filter(estado_licenciamiento != "NINGUNO") %>%
  group_by(departamento_local,
           estado_licenciamiento) %>%
  tally() %>%
  group_by(departamento_local) %>%
  mutate(
    pct = n/sum(n),
    estado_licenciamiento = str_wrap(estado_licenciamiento, 24)
  )

lima_df <- sunedu_df %>%
  filter(departamento_local == "LIMA")

otros_df <- sunedu_df %>%
  filter(departamento_local != "LIMA")

n_col <- length(unique(sunedu_df$estado_licenciamiento))
colores <- wesanderson::wes_palette("IsleofDogs1",
                                    n_col,
                                    "continuous")
estados <- unique(as.character(sunedu_df$estado_licenciamiento))
cmap <- c()
for (k in 1:length(estados)) {
  cmap[estados[k]] <- colores[k]
}

p1 <- ggplot(otros_df,
       aes(x = departamento_local,
           y = n,
           fill = estado_licenciamiento)) +
  geom_col(width = .97) +
  coord_polar() +
  scale_fill_manual(
    name = "Estado",
    values = cmap
  ) +
  labs(
    x = "",
    y = ""
  ) +
  # guides(
  #   fill = guide_legend(nrow = 4, byrow = TRUE)
  # ) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "grey70",
                              linetype = "dashed"),
    axis.text.x = element_text(size = 14,
                               color = "black"),
    axis.text.y = element_blank()
  )
p1

p2 <- ggplot(lima_df, aes(x = departamento_local,
                    y = n,
                    fill = estado_licenciamiento)) +
  geom_col(width = 1, show.legend = FALSE) +
  labs(
    x = "",
    y = ""
  ) +
  scale_fill_manual(
    name = "",
    values = cmap
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
  )

p12 <- p1 + p2 +
  plot_layout(
    widths = c(12, 1),
    nrow = 1
  ) +
  plot_annotation(
    title = "Estado de licenciamiento de las Universidades en Perú (SUNEDU)",
    subtitle = "Fuente: SUNEDU (https://www.datosabiertos.gob.pe/dataset/sunedu-estado-licenciamiento-universidades)",
    caption = "2020-06-10 // #30diasdegraficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) &
  theme(
    plot.title = element_text(size = 32),
    plot.subtitle = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata",
                                size = 20),
    plot.margin = unit(rep(1, 4), "cm"),
    legend.text = element_text(size = 14),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.height = unit(1.5, "cm"),
    legend.title = element_text(size = 16, face = "bold.italic")
  )

ggsave(
  plot = p12,
  filename = "30-grafico-areas-polares-licenciamiento-universidades.png",
  height = 12,
  width = 18
)

img <- magick::image_read("30-grafico-areas-polares-licenciamiento-universidades.png")
img_resized <- magick::image_scale(img, "20%")
magick::image_write(
  img_resized,
  "30-grafico-areas-polares-licenciamiento-universidades-resized20.png"
)

