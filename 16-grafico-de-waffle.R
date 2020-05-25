library(tidyverse)
library(waffle)
library(hrbrthemes)

load("datos/2020-bonos-covid19-midis.Rdata")

plot_df <- bonos_covid19 %>%
  filter(DE_DEPARTAMENTO == "TACNA") %>%
  select(DE_PROVINCIA, CO_HOGAR) %>%
  distinct() %>%
  # mutate(
  #   DE_PROVINCIA = str_replace(DE_PROVINCIA,
  #                              "CA�ETE",
  #                              "CAÑETE")
  # ) %>%
  group_by(DE_PROVINCIA) %>%
  tally() %>%
  arrange(DE_PROVINCIA) %>%
  mutate(
    prov_lbl = glue::glue("{prov} (N = {n})",
                          prov = str_to_title(DE_PROVINCIA),
                          n = format(n, big.mark = ",")) %>%
      str_squish()
  )

ggplot(plot_df, aes(values = n, fill = prov_lbl)) +
  geom_waffle(n_rows = 10, make_proportional = TRUE,
              #show.legend = FALSE,
              radius = grid::unit(0.1, "npc")) +
  scale_fill_viridis_d(
    name = "Provincias"
  ) +
  coord_equal() +
  labs(
    title = "Hogares de Tacna en el registro del \"Bono COVID-19\"",
    subtitle = "Fuente: Bonos COVID-19 (MIDIS) https://bit.ly/bonos-covid19-midis-peru",
    caption = "2020-05-27 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_ipsum_rc(24, grid = "") +
  theme_enhance_waffle() +
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "italic"),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.caption.position = "plot"
  )

ggsave(
  filename = "16-grafico-de-waffle-bono-covid19-tacna.png",
  width = 14,
  height = 9
)
