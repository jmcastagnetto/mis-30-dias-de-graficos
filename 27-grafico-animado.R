library(tidyverse)
library(gganimate)
library(gifski)

covid19_deaths_url <- "https://github.com/jmcastagnetto/covid-19-data-cleanup/raw/master/data/covid-19_ts_deaths.csv.gz"

covid19_deaths_raw <- read_csv(covid19_deaths_url)

wb_pop <- readxl::read_excel("datos/API_SP.POP.TOTL_DS2_en_excel_v2_988396.xls",
                             sheet = 1, range = "A4:BK268") %>%
  janitor::clean_names() %>%
  select(country_code, pop_2018 = x2018)

covid19_deaths_americas <- covid19_deaths_raw %>%
  filter(continent == "Americas" & deaths > 0) %>%
  mutate(
    isowk = lubridate::isoweek(ts)
  ) %>%
  group_by(isowk, iso3c, country_region) %>%
  summarise(
    deaths_wk = max(deaths, na.rm = TRUE),
    last_day = max(ts)
  ) %>%
  left_join(
    wb_pop,
    by = c("iso3c" = "country_code")
  ) %>%
  mutate(
    fallecidos_por_millon = deaths_wk * 1e6 / pop_2018
  )


animplot <- ggplot(
  covid19_deaths_americas,
  aes(x = fallecidos_por_millon,
      y = country_region,
      color = country_region,
      fill = country_region)
) +
  geom_col(width = .3, show.legend = FALSE) +
  geom_point(size = 4, show.legend = FALSE) +
  labs(
    y = "",
    x = "Fallecimientos por millón de habitantes",
    title = "Fallecimientos por COVID-19 en las Americas",
    subtitle = paste0("Fecha: {frame_time}"),
    caption = "2020-06-07 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_minimal(24) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10),
    plot.caption = element_text(family = "Inconsolata")
  ) +
  transition_time(last_day) +
  ease_aes("linear")

nframes = length(unique(covid19_deaths_americas$isowk))

anim_save(
  animation = animplot,
  file = "27-grafico-animado-fallecimientos-covid19-americas.gif",
  nframes = nframes * 8 + 20,
  end_pause = 20,
  width = 800,
  height = 650
)
