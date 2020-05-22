library(tidyverse)
library(countrycode)

if (file.exists("datos/beach-volleyball.Rdata")) {
  load("datos/beach-volleyball.Rdata")
} else {
  vb_matches <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

save(
    vb_matches,
    file = here::here("datos/beach-volleyball.Rdata")
  )
}

df <- vb_matches %>%
  mutate(
    w_p1_continent = countrycode(w_p1_country,
                                 origin = "country.name",
                                 destination = "continent"),
    w_p1_continent = if_else(w_p1_country %in% c("England", "Scotland"),
                             "Europe", w_p1_continent),
    w_p1_continent = if_else(w_p1_country == "Virgin Islands",
                             "Americas", w_p1_continent),
    w_p2_continent = countrycode(w_p2_country,
                                 origin = "country.name",
                                 destination = "continent"),
    w_p2_continent = if_else(w_p2_country %in% c("England", "Scotland"),
                             "Europe", w_p1_continent),
    w_p2_continent = if_else(w_p2_country == "Virgin Islands",
                             "Americas", w_p1_continent),
    l_p1_continent = countrycode(l_p1_country,
                                 origin = "country.name",
                                 destination = "continent"),
    l_p1_continent = if_else(l_p1_country %in% c("England", "Scotland"),
                             "Europe", l_p1_continent),
    l_p1_continent = if_else(l_p1_country == "Virgin Islands",
                             "Americas", l_p1_continent),
    l_p2_continent = countrycode(l_p2_country,
                                 origin = "country.name",
                                 destination = "continent"),
    l_p2_continent = if_else(l_p2_country %in% c("England", "Scotland"),
                             "Europe", l_p1_continent),
    l_p2_continent = if_else(l_p2_country == "Virgin Islands",
                             "Americas", l_p1_continent),
  ) %>%
  filter(
    !is.na(w_p1_continent) &
      !is.na(w_p2_continent) &
      !is.na(l_p1_continent) &
      !is.na(l_p2_continent)
  ) %>%
  unite(
    col = "w_team",
    c(w_p1_continent, w_p2_continent),
    sep = ", "
  ) %>%
  unite(
    col = "l_team",
    c(l_p1_continent, l_p2_continent),
    sep = ", "
  ) %>%
  mutate(
    sexo = if_else(gender == "M", "Masculino", "Femenino")
  ) %>%
  group_by(
    sexo,
    w_team,
    l_team
  ) %>%
  tally() %>%
  ungroup() %>%
  mutate(
    lbl_color = if_else(n > 7000, "yellow", "black")
  )

ggplot(df, aes(x = l_team, y = w_team, fill = n)) +
  geom_raster() +
  geom_text(aes(label = n), color = df$lbl_color) +
  scale_fill_viridis_c(
    name = "Juegos ganados: ",
    direction = -1,
    limits = c(0, 20000)
  ) +
  facet_wrap(~sexo, scales = "free") +
  labs(
    x = "Equipo perdedor",
    y = "Equipo ganador",
    title = "Volleyball de playa - ¡Choque de continentes!: veces que un equipo ganó a otro",
    subtitle = "Fuente: #TidyTuesday \"Beach volleyball\" (https://github.com/rfordatascience/tidytuesday)",
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_linedraw(18) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"),
    plot.caption = element_text(family = "Inconsolata"),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title = element_text(face = "bold", size = 18),
  )

ggsave(
  filename = "11-heatmap-voley.png",
  width = 18,
  height = 12
)
