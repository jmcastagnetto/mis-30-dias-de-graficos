library(tidyverse)
library(treemapify)
library(gganimate)
library(gifski)

covid19_deaths_url <- "https://github.com/jmcastagnetto/covid-19-data-cleanup/raw/master/data/covid-19_ts_deaths.csv.gz"

covid19_deaths_raw <- read_csv(covid19_deaths_url)

# extraer los fallecimientos en los últimos 60 días
covid19_deaths_americas <- covid19_deaths_raw %>%
  filter(continent == "Americas" & deaths > 0) %>%
  filter(ts > max(ts) - lubridate::days(60) ) %>%
  select(world_bank_income_group, country_region, ts, deaths) %>%
  mutate(
    world_bank_income_group = factor(
      world_bank_income_group,
      levels = c("Low income",
                 "Lower middle income",
                 "Upper middle income",
                 "High income"),
      ordered = TRUE
    )
  )

min_ts <- min(covid19_deaths_americas$ts)
max_ts <- max(covid19_deaths_americas$ts)

animplot <- ggplot(
  covid19_deaths_americas,
  aes(area = deaths, fill = country_region,
      label = country_region, subgroup = world_bank_income_group)
) +
  geom_treemap(layout = "fixed") +
  geom_treemap_subgroup_border(layout = "fixed") +
  geom_treemap_subgroup_text(
    layout = "fixed",
    place = "bottom", alpha = 0.5,
    color = "black", fontface = "italic", min.size = 0
  ) +
  geom_treemap_text(
    layout = "fixed",
    place = "topleft",
    color = "white",
    grow = TRUE,
    reflow = TRUE
  ) +
  facet_wrap(~world_bank_income_group, scales = "free", ncol = 2) +
  labs(
    title = "Fallecimientos acumulados por COVID-19 en las Américas",
    subtitle = paste0("Rango: del ", min_ts,
                      " al ", max_ts, " - ",
                      "Fecha mostrada: {frame_time}"),
    caption = "2020-05-25 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_linedraw(32) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  ) +
  transition_time(ts) +
  ease_aes("linear")

anim_save(
  animation = animplot,
  file = "14-graficos-treemap-fallecimientos-covid19-americas.gif",
  nframes = 60, fps = 1,
  width = 1400,
  height = 900
)
