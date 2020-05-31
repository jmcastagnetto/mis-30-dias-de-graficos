library(tidyverse)
library(rvest)
library(ggraph)
library(tidygraph)

# Fuente: "Los peruanos del Lava Jato", El Comercio

url <- "https://especiales.elcomercio.pe/?q=especiales/los-peruanos-del-lavajato/index.html"
xpath_table <- '//*[@id="example"]'

doc <- read_html(url)
tab_raw <- doc %>%
  html_node(xpath = xpath_table) %>%
  html_table(fill = TRUE) %>%
  select(-6) %>%
  janitor::clean_names() %>%
  rename(
    etapa = 4,
    delitos = 5
  )

pe_lavajato <- tab_raw %>%
  separate_rows(
    delitos,
    sep = "\\|"
  ) %>%
  mutate(
    delitos = str_trim(str_squish(delitos))
  ) %>%
  mutate_all(.funs = factor)

save(
  pe_lavajato,
  file = "datos/elcomercio-peruanos-lavajato-2019.Rdata"
)

# relacionar investigados por casos


inv_casos <- pe_lavajato %>%
  select(investigados, casos, etapa) %>%
  distinct() %>%
  mutate(
    flag = TRUE
  )

investigados <- pe_lavajato %>%
  select(investigados) %>%
  arrange(investigados) %>%
  distinct() %>%
  mutate(
    nombre = investigados
  )

investigados_pares <- pe_lavajato %>%
  select(investigados) %>%
  mutate(copia = investigados) %>%
  distinct() %>%
  expand(investigados, copia) %>%
  filter(investigados != copia) %>%
  left_join(
    inv_casos,
    by = c("investigados" = "investigados")
  ) %>%
  rename(
    caso1 = casos,
    flag1 = flag
  ) %>%
  left_join(
    inv_casos,
    by = c("copia" = "investigados", "caso1" = "casos", "etapa")
  ) %>%
  rename(
    flag2 = flag
  ) %>%
  filter(flag1 & flag2) %>%
  select(-flag1, -flag2) %>%
  rename(
    caso = caso1
  )

g_investigados <- tbl_graph(
  nodes = investigados,
  edges = investigados_pares) %>%
  mutate(
    Conexiones = centrality_degree(mode = "in")
  )

red_investigados <- ggraph(g_investigados, layout = "kk") +
  geom_edge_fan(aes(color = caso), width = .1, show.legend = FALSE) +
  geom_node_text(aes(label = str_wrap(nombre, 10), color = Conexiones),
                 size = 4, repel = FALSE) +
  labs(
    title = "Investigados por el caso \"Lava Jato\" en Perú",
    subtitle = "Fuente: El Comercio, 2019 (https://especiales.elcomercio.pe/?q=especiales/los-peruanos-del-lavajato/index.html)",
    caption = "2020-05-31 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_graph() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "in"),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.caption = element_text(family = "Inconsolata", face = "plain", size = 24),
    plot.title = element_text(size= 62),
    plot.subtitle = element_text(size= 34)
  )

#red_investigados

ggsave(
  plot = red_investigados,
  filename = "20-grafico-de-redes-peruanos-lavajato-elcomercio-2019.png",
  width = 24,
  height = 24
)
