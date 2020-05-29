# Fuente: https://www.who.int/ictrp/en/
# 2936 rows, updated on: 26 May 2020
# Downloaded on: 2020-05-28

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

who_ct_raw <- read_csv(
  "datos/COVID19-web.csv",
  col_types = cols(
    "TrialID" = col_character(),
    `Last Refreshed on` = col_character(),
    `Public title` = col_character(),
    `Scientific title` = col_character(),
    "Acronym" = col_character(),
    `Primary sponsor` = col_character(),
    `Date registration` = col_character(),
    `Date registration3` = col_date(format = "%Y%m%d"),
    `Export date` = col_character(),
    `Source Register` = col_character(),
    `web address` = col_character(),
    `Recruitment Status` = col_character(),
    `other records` = col_character(),
    `Inclusion agemin` = col_character(),
    `Inclusion agemax` = col_character(),
    `Inclusion gender` = col_character(),
    `Date enrollement` = col_character(),
    `Target size` = col_character(),
    `Study type` = col_character(),
    `Study design` = col_character(),
    "Phase" = col_character(),
    "Countries" = col_character(),
    `Contact Firstname` = col_character(),
    `Contact Lastname` = col_character(),
    `Contact Address` = col_character(),
    `Contact Email` = col_character(),
    `Contact Tel` = col_character(),
    `Contact Affiliation` = col_character(),
    `Inclusion Criteria` = col_character(),
    `Exclusion Criteria` = col_character(),
    "Condition" = col_character(),
    "Intervention" = col_character(),
    `Primary outcome` = col_character(),
    `results date posted` = col_character(),
    `results date completed` = col_character(),
    `results url link` = col_character(),
    `Retrospective flag` = col_character(),
    `Bridging flag truefalse` = col_logical(),
    `Bridged type` = col_character(),
    `results yes no` = col_logical()
  )
) %>%
  janitor::clean_names()

who_ct_countries <- who_ct_raw %>%
  select(countries, recruitment_status) %>%
  mutate(
    countries = str_trim(countries) %>%
      str_squish(),
    recruitment_status = str_trim(recruitment_status) %>%
      str_to_title() %>%
      str_replace_all(c(
        "Authorised" = "Autorizado",
        "Not Available" = "No disponible",
        "Not Recruiting" = "No está reclutando",
        "Recruiting" = "Reclutando"
      )) %>%
    str_replace_na("Desconocido")
  ) %>%
  separate_rows(
    countries,
    sep = ";"
  ) %>%
  separate_rows(
    countries,
    sep = ","
  ) %>%
  rename(
    country = countries
  ) %>%
  mutate(
    simple_country = simplecountries::simple_country_name(country),
    iso3c = countrycode::countrycode(simple_country,
                                     origin = "country.name",
                                     destination = "iso3c")

  ) %>%
  filter(!is.na(iso3c)) %>%
  group_by(
    simple_country,
    iso3c,
    recruitment_status
  ) %>%
  tally() %>%
  arrange(iso3c)

# Ref: https://slcladal.github.io/maps.html#4_color_coding_geospatial_information

world <- ne_countries(scale = "medium", returnclass = "sf")

who_ct_df <- world %>%
  left_join(
    who_ct_countries,
    by = c("iso_a3" = "iso3c")
  ) %>%
  filter(!is.na(simple_country))

world_ct_map <- ggplot() +
  geom_sf(data = who_ct_df, aes(fill = n)) +
  scale_fill_viridis_c(
    direction = 1,
    breaks = seq(0, 500, by = 50)
    ) +
  labs(
    title = "Ensayos Clínicos sobre COVID-19 en el mundo, por estado de reclutamiento",
    subtitle = "Información al 2020-05-26. Fuente: WHO (https://www.who.int/ictrp/en/)",
    caption = "2020-05-29 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto",
    fill = "Número de Ensayos Clínicos"
  ) +
  facet_wrap(~recruitment_status) +
  ggdark::dark_theme_light(18) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 16, face = "bold", color = "orange"),
    legend.title = element_text(size = 12),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.text = element_text(size = 12),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.caption = element_text(family = "Inconsolata"),
    plot.title.position = "panel",
    plot.caption.position = "panel"
  )

ggsave(
  plot = world_ct_map,
  filename = "18-visualizar-datos-espaciales-ensayos-clinicos-covid19-who.png",
  width = 14,
  height = 8
)
