library(tidyverse)
library(tidygraph)
library(ggraph)

dune_raw <- read_csv(here::here("datos/dune-libros.csv"))

dune_df <- dune_raw %>%
  filter(genre == "Novel") %>%
  mutate(
    from = 1965,
    series = replace_na(series, "Otra")
  ) %>%
  rename(
    to = pub_year,
    "Serie:" = series
  )

edges_df <- tbl_graph(edges = dune_df, directed = TRUE)

ggraph(edges_df, layout = "linear") +
  geom_vline(xintercept = 1986, size = 1, linetype = "dotted") +
  geom_edge_arc(aes(color = `Serie:`), width = 2, alpha = .5) +
  scale_edge_color_brewer(type = "qual", palette = "Dark2") +
  ylim(-8, 24) +
  geom_text(
    data = dune_df,
    aes(x = to, y = -1, label = title),
    angle = 90, hjust = 1
  ) +
  scale_x_continuous(position = "top") +
  theme_linedraw(18) +
  annotate("text", x = 1970, y = 22, vjust = 1,
           size = 7, label = "Frank Herbert") +
  annotate("text", x = 2010, y = 22, vjust = 1,
           size = 7, label = "Brian Herbert y\nKevin J. Anderson") +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank()
  ) +
  #coord_equal() +
  labs(
    x = "",
    title = "Las novelas de \"Dune\" (1965) por fecha de publicación, serie y autor",
    subtitle = "Fuente: https://en.wikipedia.org/wiki/Dune_(franchise)",
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  )

ggsave(
  filename = "05-grafico-de-arcos-novelas-dune.png",
  width = 10,
  height = 10
)
